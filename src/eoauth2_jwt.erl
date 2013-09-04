%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@heroku.com>
%%% @copyright (C) 2013, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 4 Sep 2013 by Tristan Sloughter <tristan@heroku.com>
%%%-------------------------------------------------------------------
-module(eoauth2_jwt).

-export([access_token/4]).

-include_lib("public_key/include/public_key.hrl").

access_token(Iss, Scope, Aud, EncodedPrivateKey) ->
    Jwt = jwt(Iss, Scope, Aud, EncodedPrivateKey),
    Uri = <<"grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=">>,
    Json = make_request(post, <<"https://accounts.google.com">>, <<"/o/oauth2/token">>,
                        [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
                        << Uri/binary, Jwt/binary>>, 200),
    case proplists:get_value(<<"access_token">>, Json) of
        undefined ->
            undefined;
        Token ->
            {ok, Token}
    end.

jwt(Iss, Scope, Aud, EncodedPrivateKey) ->
    [PemEntry] = public_key:pem_decode(EncodedPrivateKey),
    PrivateKey = public_key:pem_entry_decode(PemEntry),

    EncodedJWTHeader = encode(jwt_header()),
    EncodedJWTClaimSet = encode(jwt_claim_set(Iss, Scope, Aud)),
    Signature = compute_signature(EncodedJWTHeader, EncodedJWTClaimSet, PrivateKey),

    binary:replace(
      binary:replace(<<EncodedJWTHeader/binary, ".", EncodedJWTClaimSet/binary, ".", Signature/binary>>,
                     <<"+">>, <<"-">>, [global]),
      <<"/">>, <<"_">>, [global]).

jwt_header() ->
    [{alg, <<"RS256">>}, {typ, <<"JWT">>}].

jwt_claim_set(Iss, Scope, Aud) ->
    [{iss, Iss},
     {scope, Scope},
     {aud, Aud},
     {exp, calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 62167219200 + 3600},
     {iat, calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 62167219200}].

encode(Json) ->
    base64:encode(jsx:encode(Json)).

compute_signature(Header, ClaimSet, #'RSAPrivateKey'{publicExponent=Exponent
                                                    ,modulus=Modulus
                                                    ,privateExponent=PrivateExponent}) ->
    base64:encode(crypto:sign(rsa, sha256, <<Header/binary, ".", ClaimSet/binary>>, [Exponent, Modulus, PrivateExponent])).

make_request(Method, Url, Path, Headers, Body, ExpectedStatus) ->
    {ok, Status, _RespHeaders, Client} = hackney:request(Method, <<Url/binary, Path/binary>>,
                                                                 Headers,
                                                                 Body, []),
    lager:info("at=make_request status=~p", [Status]),
    {ok, Result, _Client1} = hackney:body(Client),
    jsx:decode(Result).

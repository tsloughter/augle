%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2017, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 4 Sep 2017 by Tristan Sloughter <t@crashfast.com>
%%%-------------------------------------------------------------------
-module(augle_jwt).

-export([access_token/4]).

-include_lib("public_key/include/public_key.hrl").

-define(HOST, <<"https://www.googleapis.com">>).
-define(TOKEN_PATH, <<"/oauth2/v4/token">>).

-define(JWT_HEADER, [{alg, <<"RS256">>}, {typ, <<"JWT">>}]).

access_token(Iss, Scope, Aud, EncodedPrivateKey) ->
    Jwt = jwt(Iss, Scope, Aud, EncodedPrivateKey),
    Uri = <<"grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=">>,
    make_request(post, ?HOST, ?TOKEN_PATH,
                 [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
                 <<Uri/binary, Jwt/binary>>).

jwt(Iss, Scope, Aud, EncodedPrivateKey) ->
    [PemEntry] = public_key:pem_decode(EncodedPrivateKey),
    PrivateKey = asn1_decode(public_key:pem_entry_decode(PemEntry)),

    EncodedJWTHeader = encode(?JWT_HEADER),
    EncodedJWTClaimSet = encode(jwt_claim_set(Iss, Scope, Aud)),
    Signature = compute_signature(EncodedJWTHeader, EncodedJWTClaimSet, PrivateKey),

    binary:replace(
      binary:replace(<<EncodedJWTHeader/binary, ".", EncodedJWTClaimSet/binary, ".", Signature/binary>>,
                     <<"+">>, <<"-">>, [global]), <<"/">>, <<"_">>, [global]).

asn1_decode(#'PrivateKeyInfo'{privateKey=DerKey}) ->
    public_key:der_decode('RSAPrivateKey', DerKey);
asn1_decode(Der) ->
    Der.

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

make_request(Method, Url, Path, Headers, Body) ->
    case hackney:request(Method, <<Url/binary, Path/binary>>,
                         Headers,
                         Body, []) of
        {ok, 200, _RespHeaders, Client} ->
            {ok, Result} = hackney:body(Client),
            jsx:decode(Result, [return_maps]);
        {ok, _Status, _RespHeaders, Client} ->
            {ok, ErrorJson} = hackney:body(Client),
            Reason = jsx:decode(ErrorJson, [return_maps]),
            %% error_logger:info_msg("error fetching access token: ~s", [format_error(Reason)]),
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.


%% format_error(#{<<"error">> := Error,
%%                <<"error_description">> := Description}) ->
%%     io_lib:format("error=~s description=~s", [Error, Description]).

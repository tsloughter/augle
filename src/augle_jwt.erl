%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2017, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 4 Sep 2017 by Tristan Sloughter <t@crashfast.com>
%%%-------------------------------------------------------------------
-module(augle_jwt).

-export([access_token/3]).

-include_lib("public_key/include/public_key.hrl").

-define(HOST, "https://www.googleapis.com").
-define(TOKEN_PATH, "/oauth2/v4/token").

-define(AUD, <<"https://www.googleapis.com/oauth2/v4/token">>).
-define(URLENCODED_CONTENT_TYPE, <<"application/x-www-form-urlencoded">>).
-define(JWT_HEADER, [{alg, <<"RS256">>}, {typ, <<"JWT">>}]).

access_token(Iss, Scopes, EncodedPrivateKey) ->
    Jwt = jwt(Iss, Scopes, EncodedPrivateKey),
    Uri = <<"grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=">>,
    case hackney:post(<<?HOST, ?TOKEN_PATH>>,
                      [{<<"Content-Type">>, ?URLENCODED_CONTENT_TYPE}],
                      <<Uri/binary, Jwt/binary>>, []) of
        {ok, 200, _RespHeaders, Client} ->
            {ok, Result} = hackney:body(Client),
            {ok, augle_utils:decode_json(Result)};
        {ok, _Status, _RespHeaders, Client} ->
            {ok, ErrorJson} = hackney:body(Client),
            Reason = augle_utils:decode_json(ErrorJson),
            %% error_logger:info_msg("error fetching access token: ~s", [format_error(Reason)]),
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

jwt(Iss, Scopes, EncodedPrivateKey) ->
    [PemEntry] = public_key:pem_decode(EncodedPrivateKey),
    PrivateKey = asn1_decode(public_key:pem_entry_decode(PemEntry)),

    EncodedJWTHeader = encode(?JWT_HEADER),
    EncodedJWTClaimSet = encode(jwt_claim_set(Iss, Scopes)),
    Signature = compute_signature(EncodedJWTHeader, EncodedJWTClaimSet, PrivateKey),

    binary:replace(
      binary:replace(<<EncodedJWTHeader/binary, ".", EncodedJWTClaimSet/binary, ".", Signature/binary>>,
                     <<"+">>, <<"-">>, [global]), <<"/">>, <<"_">>, [global]).

asn1_decode(#'PrivateKeyInfo'{privateKey=DerKey}) ->
    public_key:der_decode('RSAPrivateKey', DerKey);
asn1_decode(Der) ->
    Der.

jwt_claim_set(Iss, Scopes) ->
    [{iss, Iss},
     {scope, Scopes},
     {aud, ?AUD},
     {exp, calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 62167219200 + 3600},
     {iat, calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 62167219200}].

encode(Json) ->
    base64:encode(jsx:encode(Json)).

compute_signature(Header, ClaimSet, #'RSAPrivateKey'{publicExponent=Exponent
                                                    ,modulus=Modulus
                                                    ,privateExponent=PrivateExponent}) ->
    base64:encode(crypto:sign(rsa, sha256, <<Header/binary, ".", ClaimSet/binary>>, [Exponent, Modulus, PrivateExponent])).

%% format_error(#{error := Error,
%%                error_description := Description}) ->
%%     io_lib:format("error=~s description=~s", [Error, Description]).

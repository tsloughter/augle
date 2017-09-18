%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2017, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 16 Sep 2017 by Tristan Sloughter <t@crashfast.com>
%%%-------------------------------------------------------------------
-module(augle_gcloud_sdk).

-export([app_default_credentials/2,
         creds_from_file/2,
         metadata_fetch_token/1,
         get_config_path/0]).

-define(META_URL(ServiceAccount),
        <<"http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/",
          ServiceAccount/binary, "/token">>).
-define(META_HEADERS, [{<<"Metadata-Flavor">>, <<"Google">>}]).

-define(CREDENTIALS_FILENAME, <<"application_default_credentials.json">>).

-define(APP_CREDENTIALS, "GOOGLE_APPLICATION_CREDENTIALS").
-define(CONFIG_DIR, "CLOUDSDK_CONFIG").

-define(TOKEN_ENDPOINT, <<"https://accounts.google.com/o/oauth2/token">>).
-define(URLENCODED_CONTENT_TYPE, <<"application/x-www-form-urlencoded">>).
-define(REFRESH_GRANT_TYPE, <<"refresh_token">>).

app_default_credentials(ServiceAccount, Scopes) ->
    case os:getenv(?APP_CREDENTIALS) of
        false ->
            default_path_or_metadata(ServiceAccount);
        Path ->
            creds_from_file(Path, Scopes)
    end.

creds_from_file(Path, Scopes) ->
    {ok, File} = file:read_file(Path),
    #{project_id := _ProjectId,
      client_email := Iss,
      private_key := EncodedPrivateKey} = augle_utils:decode_json(File),

    augle_jwt:access_token(Iss, Scopes, EncodedPrivateKey).

default_path_or_metadata(ServiceAccount) ->
    ConfigPath = ?MODULE:get_config_path(),
    CredentialsFile = filename:join(ConfigPath, ?CREDENTIALS_FILENAME),
    case file:read_file(CredentialsFile) of
        {ok, Content} ->
            #{client_id := ClientId,
              client_secret := ClientSecret ,
              refresh_token := RefreshToken,
              type := _Type} = augle_utils:decode_json(Content),
            refresh_grant(?TOKEN_ENDPOINT, RefreshToken, ClientId, ClientSecret);
        _ ->
            %% doesn't exist or we don't have permissions. try instance metadata
            metadata_fetch_token(ServiceAccount)
    end.

get_config_path() ->
    case os:getenv(?CONFIG_DIR) of
        false ->
            {ok, [[Home]]} = init:get_argument(home),
            %% TODO: Windows
            filename:join([Home, ".config", "gcloud"]);
        Dir ->
            Dir
    end.

metadata_fetch_token(ServiceAccount) ->
    case hackney:get(?META_URL(ServiceAccount),
                     ?META_HEADERS, <<>>,
                     []) of
        {ok, 200, _RespHeaders, Client} ->
            {ok, Body} = hackney:body(Client),
            {ok, augle_utils:decode_json(Body)};
        {ok, _Status, _, Client} ->
            {ok, Body} = hackney:body(Client),
            {error, augle_utils:decode_json(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

refresh_grant(TokenUri, RefreshToken, ClientId, ClientSecret) ->
    QS = [{<<"grant_type">>, ?REFRESH_GRANT_TYPE},
          {<<"client_id">>, ClientId},
          {<<"client_secret">>, ClientSecret},
          {<<"refresh_token">>, RefreshToken}],
    Body = hackney_url:qs(QS),
    Headers = [{<<"content-type">>, ?URLENCODED_CONTENT_TYPE}],
    case hackney:post(TokenUri, Headers, Body, []) of
        {ok, 200, _RespHeaders, ClientRef} ->
            {ok, Result} = hackney:body(ClientRef),
            {ok, augle_utils:decode_json(Result)};
        {ok, _Status, _, Client} ->
            {ok, Body} = hackney:body(Client),
            {error, augle_utils:decode_json(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

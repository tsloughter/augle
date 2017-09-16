%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2017, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 16 Sep 2017 by Tristan Sloughter <t@crashfast.com>
%%%-------------------------------------------------------------------
-module(augle_gcloud_sdk).

-export([app_default_credentials/0,
         fetch_token/1]).

-define(META_URL(ServiceAccount),
        <<"http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/",
          ServiceAccount/binary, "/token">>).
-define(META_HEADERS, [{<<"Metadata-Flavor">>, <<"Google">>}]).

-define(AUTH_HEADERS(AccessToken), [{<<"Authorization">>, <<"Bearer ", AccessToken/binary>>}]).

-define(TRACE_URL(ProjectId),
       <<"https://cloudtrace.googleapis.com/v1/projects/", ProjectId/binary, "/traces">>).

-define(CREDENTIALS_FILENAME, <<"application_default_credentials.json">>).

-define(APP_CREDENTIALS, "GOOGLE_APPLICATION_CREDENTIALS").
-define(CONFIG_DIR, "CLOUDSDK_CONFIG").

-define(TOKEN_ENDPOINT, <<"https://accounts.google.com/o/oauth2/token">>).
-define(URLENCODED_CONTENT_TYPE, <<"application/x-www-form-urlencoded">>).
-define(JWT_GRANT_TYPE, <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>).
-define(REFRESH_GRANT_TYPE, <<"refresh_token">>).

app_default_credentials() ->
    case os:getenv(?APP_CREDENTIALS) of
        false ->
            default_path_or_metadata();
        Path ->
            {ok, File} = file:read_file(Path),
            #{<<"project_id">> := _ProjectId,
              <<"client_email">> := Iss,
              <<"private_key">> := EncodedPrivateKey} = jsx:decode(File, [return_maps]),
            Aud = <<"https://www.googleapis.com/oauth2/v4/token">>,
            augle_jwt:access_token(Iss, <<"https://www.googleapis.com/auth/trace.append">>, Aud, EncodedPrivateKey)
    end.

default_path_or_metadata() ->
    ConfigPath = get_config_path(),
    CredentialsFile = filename:join(ConfigPath, ?CREDENTIALS_FILENAME),
    case file:read_file(CredentialsFile) of
        {ok, Content} ->
            #{<<"client_id">> := ClientId,
              <<"client_secret">> := ClientSecret ,
              <<"refresh_token">> := RefreshToken,
              <<"type">> := _Type} = jsx:decode(Content, [return_maps]),
            refresh_grant(?TOKEN_ENDPOINT, RefreshToken, ClientId, ClientSecret);
        _ ->
            %% doesn't exist or we don't have permissions. try instance metadata
            fetch_token(<<"default">>)
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

fetch_token(ServiceAccount) ->
    {ok, _StatusCode, _RespHeaders, Client} = hackney:get(?META_URL(ServiceAccount),
                                                          ?META_HEADERS, <<>>,
                                                          []),
    {ok, Body} = hackney:body(Client),
    #{<<"project_id">> := ProjectId,
      <<"access_token">> := AccessToken} = jsx:decode(Body, [return_maps]),
    #{project_id => ProjectId,
      access_token => AccessToken}.

refresh_grant(TokenUri, RefreshToken, ClientId, ClientSecret) ->
    QS = [{<<"grant_type">>, ?REFRESH_GRANT_TYPE},
          {<<"client_id">>, ClientId},
          {<<"client_secret">>, ClientSecret},
          {<<"refresh_token">>, RefreshToken}],
    Body = hackney_url:qs(QS),
    Headers = [{<<"content-type">>, ?URLENCODED_CONTENT_TYPE}],
    case hackney:request(post, TokenUri, Headers, Body, []) of
        {ok, 200, _RespHeaders, ClientRef} ->
            {ok, Result} = hackney:body(ClientRef),
            jsx:decode(Result, [return_maps]);
        _ ->
            error
    end.

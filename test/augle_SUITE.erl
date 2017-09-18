%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(augle_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [os_var_default, config_path_default, metadata_default, token_cache_refresh].

init_per_suite(Config) ->
    application:ensure_all_started(augle),
    Config.

end_per_suite(_Config) ->
    application:stop(augle),
    ok.

init_per_testcase(_, Config) ->
    meck:new(augle_gcloud_sdk, [passthrough]),
    meck:new(hackney, [passthrough]),
    Config.

end_per_testcase(_, _Config) ->
    meck:unload(augle_gcloud_sdk),
    meck:unload(hackney),
    ok.

os_var_default(Config) ->
    DataDir = ?config(data_dir, Config),
    os:putenv("GOOGLE_APPLICATION_CREDENTIALS", filename:join(DataDir, "fake_service_account.json")),

    meck:expect(hackney, post, 4, {ok, 200, [], []}),
    meck:expect(hackney, body, 1,
                {ok, <<"{\"access_token\":\"abc123\",\"expires_in\":3600,\"token_type\":\"service_account\"}">>}),

    ?assertMatch({ok, #{access_token := <<"abc123">>,
                        expires_in := 3600,
                        token_type := <<"service_account">>}}, augle:app_default_credentials()).

config_path_default(Config) ->
    DataDir = ?config(data_dir, Config),
    os:unsetenv("GOOGLE_APPLICATION_CREDENTIALS"),

    meck:expect(augle_gcloud_sdk, get_config_path, 0, DataDir),

    meck:expect(hackney, post, 4, {ok, 200, [], []}),
    meck:expect(hackney, body, 1,
                {ok, <<"{\"access_token\":\"def123\",\"expires_in\":3600,\"token_type\":\"service_account\"}">>}),

    ?assertMatch({ok, #{access_token := <<"def123">>,
                        expires_in := 3600,
                        token_type := <<"service_account">>}}, augle:app_default_credentials()).

metadata_default(_Config) ->
    os:unsetenv("GOOGLE_APPLICATION_CREDENTIALS"),

    meck:expect(augle_gcloud_sdk, get_config_path, 0, "fake_dir"),

    meck:expect(hackney, get,
               fun(<<"http://metadata.google.internal/computeMetadata"
                     "/v1/instance/service-accounts/default/token">>, _, _, _) ->
                   {ok, 200, [], []}
               end),
    meck:expect(hackney, body, 1,
                {ok, <<"{\"access_token\":\"abc321\",\"expires_in\":3600,\"token_type\":\"Bearer\"}">>}),

    ?assertMatch({ok, #{access_token := <<"abc321">>,
                        expires_in := 3600,
                        token_type := <<"Bearer">>}}, augle:app_default_credentials()).

token_cache_refresh(_Config) ->
    os:unsetenv("GOOGLE_APPLICATION_CREDENTIALS"),

    meck:expect(augle_gcloud_sdk, get_config_path, 0, "fake_dir"),

    meck:expect(hackney, get,
               fun(<<"http://metadata.google.internal/computeMetadata"
                     "/v1/instance/service-accounts/default/token">>, _, _, _) ->
                   {ok, 200, [], []}
               end),
    meck:expect(hackney, body,
                fun(_) ->
                    I = integer_to_binary(erlang:unique_integer()),
                    {ok, <<"{\"access_token\":\"", I/binary, "\",\"expires_in\":10,\"token_type\":\"Bearer\"}">>}
                end),

    ?assertEqual(notfound, augle_token_store:get(default)),

    {ok, Creds} = {ok, #{access_token := Token1,
                         expires_in := 10,
                         token_type := <<"Bearer">>}} = augle:creds_from(default),
    augle_token_store:add(default, Creds),
    timer:sleep(200),

    #{access_token := Token2,
      expires_in := 10,
      token_type := <<"Bearer">>} = augle_token_store:get(default),

    %% verify a new token with a new unique integer was added to the cache
    ?assertNotEqual(Token1, Token2),

    timer:sleep(200),
    {ok, #{access_token := Token3,
           expires_in := 10,
           token_type := <<"Bearer">>}} = augle:creds_from(default),

    %% verify a new token with a new unique integer was added to the cache
    ?assertNotEqual(Token1, Token3),
    ?assertNotEqual(Token2, Token3).

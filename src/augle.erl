%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2017, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 3 Sep 2017 by Tristan Sloughter <t@crashfast.com>
%%%-------------------------------------------------------------------
-module(augle).

-export([app_default_credentials/0,
         app_default_credentials/1,
         creds_from/1,
         headers/1]).

%% private?
-export([new_creds_from/1]).

-export_types([creds/0]).

-type creds() :: #{access_token := unicode:unicode_binary(),
                   expires_in   := integer(),
                   token_type   := unicode:unicode_binary(),

                   id_token => unicode:unicode_binary()}.

-type scopes() :: unicode:unicode_binary() | [unicode:unicode_binary()].
-type credentials_format() ::
        default |
        {file, file:filename_all()} |
        {file, file:filename_all(), scopes()} |
        {metadata, unicode:unicode_binary()}.

-define(AUTH_HEADERS(AccessToken), [{<<"Authorization">>, <<"Bearer ", AccessToken/binary>>}]).

-spec creds_from(credentials_format()) -> {ok, creds()} | {error, term()}.
creds_from(CredsFrom) ->
    case augle_token_store:get(CredsFrom) of
        notfound ->
            case new_creds_from(CredsFrom) of
                {ok, Creds} ->
                    augle_token_store:add(CredsFrom, Creds),
                    {ok, Creds};
                {error, Reason} ->
                    {error, Reason}
            end;
        Creds ->
            {ok, Creds}
    end.

-spec new_creds_from(credentials_format()) -> {ok, creds()} | {error, term()}.
new_creds_from(default) ->
    app_default_credentials();
new_creds_from({file, Path}) ->
    augle_gcloud_sdk:creds_from_file(Path, <<>>);
new_creds_from({file, Path, Scopes}) ->
    augle_gcloud_sdk:creds_from_file(Path, Scopes);
new_creds_from({metadata, ServiceAccount}) ->
    augle_gcloud_sdk:metadata_fetch_token(ServiceAccount).

%% @doc Gets the Application Default Credentials based on the order defined here:
%% https://developers.google.com/identity/protocols/application-default-credentials
-spec app_default_credentials() -> {ok, creds()} | {error, term()}.
app_default_credentials() ->
    augle_gcloud_sdk:app_default_credentials(<<"default">>, []).

-spec app_default_credentials(scopes()) -> {ok, creds()} | {error, term()}.
app_default_credentials(Scopes) when is_list(Scopes) ->
    CombinedScopes = iolist_to_binary(lists:join($,, Scopes)),
    augle_gcloud_sdk:app_default_credentials(<<"default">>, CombinedScopes);
app_default_credentials(Scopes) ->
    augle_gcloud_sdk:app_default_credentials(<<"default">>, Scopes).

-spec headers(#{access_token := binary()}) -> [{binary(), binary()}].
headers(#{access_token := Token}) ->
    ?AUTH_HEADERS(Token).

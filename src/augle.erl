%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2017, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 3 Sep 2017 by Tristan Sloughter <t@crashfast.com>
%%%-------------------------------------------------------------------
-module(augle).

-export([app_default_credentials/0]).

%% @doc Gets the Application Default Credentials based on the order defined here:
%% https://developers.google.com/identity/protocols/application-default-credentials
app_default_credentials() ->
    augle_gcloud_sdk:app_default_credentials().

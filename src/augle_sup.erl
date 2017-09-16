%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2017, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 16 Sep 2017 by Tristan Sloughter <t@crashfast.com>
%%%-------------------------------------------------------------------
-module(augle_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    TokenStore = #{id => augle_token_store,
                   start => {augle_token_store, start_link, []},
                   restart => permanent,
                   shutdown => 2000,
                   type => worker,
                   modules => [augle_token_store]},

    {ok, {#{strategy => one_for_one,
            intensity => 1,
            period => 5}, [TokenStore]}}.

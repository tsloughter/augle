%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2017, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 16 Sep 2017 by Tristan Sloughter <t@crashfast.com>
%%%-------------------------------------------------------------------
-module(augle_token_store).

-behaviour(gen_server).

-export([start_link/0,
         add/2,
         get/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(TOKEN_CACHE, token_ets_cache).

-record(state, {timers :: maps:map()}).

start_link() ->
    case ets:info(?TOKEN_CACHE, name) of
        undefined ->
            ets:new(?TOKEN_CACHE, [named_table, set, public, {read_concurrency, true}]);
        _ ->
            ok
    end,
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(CredsFrom, Creds) ->
    gen_server:cast(?SERVER, {add, CredsFrom, Creds}).

-spec get(augle:credentials_format()) -> augle:creds().
get(Id) ->
    Now = erlang:monotonic_time(seconds),
    case ets:lookup(?TOKEN_CACHE, Id) of
        [{Id, Creds, ExpiresAt}] when ExpiresAt >= Now ->
            Creds;
        _ ->
            notfound
    end.

init([]) ->
    {ok, #state{timers=maps:new()}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({add, CredsFrom, Creds=#{expires_in := ExpiresIn}}, State=#state{timers=Timers}) ->
    %% cancel timers that may be running for this credential due to a race
    cancel_old_timers(undefined, CredsFrom, Timers),
    TimerRef = schedule_refresh(ExpiresIn, CredsFrom),
    ets:insert(?TOKEN_CACHE, {CredsFrom, Creds, erlang:monotonic_time(seconds) + ExpiresIn}),
    {noreply, State#state{timers=maps:put(CredsFrom, TimerRef, Timers)}}.

handle_info({timeout, OldTimerRef, {update, CredsFrom}}, State=#state{timers=Timers}) ->
    cancel_old_timers(OldTimerRef, CredsFrom, Timers),
    TimerRef = update(CredsFrom),
    {noreply, State#state{timers=maps:put(CredsFrom, TimerRef, Timers)}};
handle_info({cancel_timer, _, _}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%

update(CredsFrom) ->
    case augle:new_creds_from(CredsFrom) of
        {ok, NewCreds=#{expires_in := ExpiresIn}} ->
            TimerRef = schedule_refresh(ExpiresIn, CredsFrom),
            ets:insert(?TOKEN_CACHE, {CredsFrom, NewCreds, erlang:monotonic_time(seconds) + ExpiresIn}),
            TimerRef;
        {error, _} ->
            %% log here?
            ets:delete(?TOKEN_CACHE, CredsFrom),
            undefined
    end.

schedule_refresh(ExpiresIn, CredsFrom) ->
    MS = erlang:convert_time_unit(ExpiresIn - 10, seconds, milli_seconds),
    erlang:start_timer(MS, ?SERVER, {update, CredsFrom}).

cancel_old_timers(OldTimerRef, CredsFrom, Timers) ->
    case maps:get(CredsFrom, Timers, undefined) of
        Ref when Ref =:= undefined
               ; Ref =:= OldTimerRef ->
            ok;
        Ref ->
            %% cancel the timer associated with CredsFrom since it is different from
            %% the one that just triggered. Timers are based on the atom name of the
            %% server so do not cancel if the process dies
            erlang:cancel_timer(Ref, [{async, true}])
    end.

%%%--------------------------------------------------------------
%% @doc counter server module.
%%%--------------------------------------------------------------

-module(counter_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([get/0, inc/1, dec/1, reset/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {tid}).

%%===============================================================
%% API functions
%%===============================================================

%% @doc Stars counter server.
-spec start_link() -> {ok, Pid :: pid()} |
                      ignore |
                      {reason, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get current server value.
-spec get() -> term().
get() ->
    gen_server:call(?MODULE, get).

%% @doc Increment current server value by 'Val'.
-spec inc(Val :: integer()) -> ok.
inc(Val) ->
    gen_server:cast(?MODULE, {inc, Val}).

%% @doc Decrement current server value by 'Val'.
-spec dec(Val :: integer()) -> ok.
dec(Val) ->
    gen_server:cast(?MODULE, {dec, Val}).

%% @doc Reset current server value to it's initial value.
-spec reset() -> term().
reset() ->
    gen_server:call(?MODULE, reset).

%%===============================================================
%% Gen Server callbacks
%%===============================================================

init([]) ->
    InitVal = counter_stash:get(init),
    CurrentVal = counter_stash:get(current),
    Tid = ets:new(srv_ets, []),
    ets:insert(Tid, {init_val, InitVal}),
    ets:insert(Tid, {val, CurrentVal}),
    process_flag(trap_exit, true),
    {ok, Tid}.

handle_cast({inc, Val}, Tid) ->
    [{val, CurrentVal}] = ets:lookup(Tid, val),
    NewVal = CurrentVal + Val,
    ets:insert(Tid, {val, NewVal}),
    {noreply, Tid};
handle_cast({dec, Val}, Tid) ->
    [{val, CurrentVal}] = ets:lookup(Tid, val),
    NewVal = CurrentVal - Val,
    ets:insert(Tid, {val, NewVal}),
    {noreply, Tid}.

handle_call(get, _From, Tid) ->
    [{val, Val}] = ets:lookup(Tid, val),
    {reply, Val, Tid};
handle_call(reset, _From, Tid) ->
    [{init_val, Val}] = ets:lookup(Tid, init_val),
    ets:insert(Tid, {val, Val}),
    {reply, ok, Tid}.

terminate(_Reason, Tid) ->
    [{val, Val}] = ets:lookup(Tid, val),
    counter_stash:put(Val).

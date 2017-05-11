%%%--------------------------------------------------------------
%% @doc counter stash module.
%%%--------------------------------------------------------------

-module(counter_stash).
-behaviour(gen_server).

-export([start_link/1]).
-export([put/1, get/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {init = 0 :: integer(),
                current = 0 :: integer()}).

-type state() :: #state{}.

%%===============================================================
%% API functions
%%===============================================================

%% @doc Starts counter stash.
-spec start_link(InitVal :: integer()) -> {ok, Pid :: pid()} |
                                          ignore |
                                          {reason, Reason :: term()}.
start_link(InitVal) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, InitVal, []).

%% @doc Put value into stash.
-spec put(Val :: integer()) -> ok.
put(Val) ->
    gen_server:cast(?MODULE, {put, Val}).

%% @doc Get value from stash.
-spec get(init) -> term().
get(init) ->
    gen_server:call(?MODULE, get_init);
get(_) ->
    gen_server:call(?MODULE, get).

%%===============================================================
%% Gen Server callbacks
%%===============================================================

init(InitVal) ->
    {ok, #state{init = InitVal, current = InitVal}}.

handle_cast({put, Val}, State) ->
    NewState = State#state{current = Val},
    {noreply, NewState}.

handle_call(get_init, _From, State = #state{init = Val}) ->
    {reply, Val, State};
handle_call(get, _From, State = #state{current = Val}) ->
    {reply, Val, State}.

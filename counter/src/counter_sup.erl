%%%-------------------------------------------------------------------
%% @doc counter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(counter_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(InitVal) ->
    {ok, MainSup} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    start_workers(InitVal),
    {ok, MainSup}.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

start_workers(InitVal) ->
    StashSpec = #{id => stash,
                  start => {counter_stash, start_link, [InitVal]},
                  restart => permanent,
                  shutdown => brutal_kill,
                  type => worker},
    ServerSpec = #{id => server,
                   start => {counter_server, start_link, []},
                   restart => permanent,
                   shutdown => brutal_kill,
                   type => worker},
    supervisor:start_child(?SERVER, StashSpec),
    supervisor:start_child(?SERVER, ServerSpec).

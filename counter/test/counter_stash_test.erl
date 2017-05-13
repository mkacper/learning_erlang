%%%--------------------------------------------------------------
%% @doc counter stash test module.
%%%--------------------------------------------------------------

-module(counter_stash_test).

-export([]).

-include_lib("proper/include/proper.hrl").

%%===============================================================
%% PropEr tests
%%===============================================================

%% @doc Check if stash save value and give it back.
prop_put_get() ->
    ?FORALL(I, integer(), put_correctly(I)).

put_correctly(I) ->
    % GIVEN
    
    % WHEN
    counter_stash:put(I),
    
    %THEN
    counter_stash:get(I) =:= I.

%% @doc Check if stash return initial value. 
prop_get_init() ->
    ?FORALL(_I, integer(), get_init()).

get_init() ->
    % GIVEN
    {ok, Init} = application:get_env(counter, init_val),
    
    % WHEN
    
    % THEN
    Init =:= counter_stash:get(init).

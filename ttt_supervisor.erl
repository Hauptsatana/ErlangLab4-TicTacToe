-module(ttt_supervisor).
-behaviour(supervisor).

% Callbacks
-export([init/1]).

-export([start_link/0, id/0]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    ChildSpec = {id(), {ttt_server, start_server, []}, transient, brutal_kill, worker, [ttt_server]},
    {ok, {{one_for_one, 5, 10}, [ChildSpec]}}.
    
id() ->
    "Server" ++ integer_to_list(erlang:system_time()).

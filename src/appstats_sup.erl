-module(appstats_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_worker/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Path) ->
    M = appstats,
    ChildSpec = {Path, {M, start_link, [Path]}, transient, 5000, worker, [M]},
    supervisor:start_child(?MODULE, ChildSpec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.


-module(statsd_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    M = statsd_server_listener,
    Child = {M, {M, start_link, [listener_opts()]}, permanent, 5000, worker, [M]},
    {ok, {{one_for_all, 5, 10}, [Child]}}.

listener_opts() ->
    case application:get_env(statsd_server, listener) of
        {ok, V} ->
            V;
        _ ->
            []
    end.

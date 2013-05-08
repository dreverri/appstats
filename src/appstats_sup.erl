-module(appstats_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_worker/1,
         list_workers/0,
         get_worker/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Path) ->
    M = appstats_session,
    ChildSpec = {Path, {M, start_link, [Path]}, transient, 5000, worker, [M]},
    supervisor:start_child(?MODULE, ChildSpec).

list_workers() ->
    Children = supervisor:which_children(?MODULE),
    [encode_id(Id) || {Id, _, _, _} <- Children].

get_worker(EncodedId) ->
    Children = supervisor:which_children(?MODULE),
    case [Pid || {Id, Pid, _, _} <- Children, encode_id(Id) == EncodedId] of
        [Found] ->
            {ok, Found};
        _ ->
            {error, not_found}
    end.


encode_id(Id) ->
    <<I:128/integer>> = crypto:md5(Id),
    list_to_binary(integer_to_list(I)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.


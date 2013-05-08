%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

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


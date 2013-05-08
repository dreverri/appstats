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

-module(appstats_http_names).

-export([init/3,
         rest_init/2,
         resource_exists/2,
         malformed_request/2,
         content_types_provided/2,
         to_json/2
        ]).

-record(state, {pid,
                start,
                stop
               }).

init(_Transport, _Req, _State) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _Args) ->
    {ok, Req, #state{}}.

resource_exists(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    case appstats_sup:get_worker(Id) of
        {ok, Pid} ->
            {true, Req1, State#state{pid=Pid}};
        _ ->
            {false, Req1, State}
    end.

malformed_request(Req, State) ->
    lists:foldl(fun run_check/2, {false, Req, State}, [fun check_start/2,
                                                       fun check_stop/2
                                                      ]).

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, to_json}
	], Req, State}.

to_json(Req, State) ->
    Pid = State#state.pid,
    Start = State#state.start,
    Stop = State#state.stop,
    Names = appstats_session:names(Pid, Start, Stop),
    {jiffy:encode(Names), Req, Pid}.

run_check(Check, {false, Req, State}) ->
    Check(Req, State);

run_check(_, Passthrough) ->
    Passthrough.

check_start(Req, State) ->
    case cowboy_req:qs_val(<<"start">>, Req) of
        {undefined, Req1} ->
            {true, Req1, State};
        {Start0, Req1} ->
            Start = list_to_integer(binary_to_list(Start0)),
            {false, Req1, State#state{start=Start}}
    end.

check_stop(Req, State) ->
    case cowboy_req:qs_val(<<"stop">>, Req) of
        {undefined, Req1} ->
            {true, Req1, State};
        {Stop0, Req1} ->
            Stop = list_to_integer(binary_to_list(Stop0)),
            {false, Req1, State#state{stop=Stop}}
    end.

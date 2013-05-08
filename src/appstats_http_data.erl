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

-module(appstats_http_data).

-export([init/3,
         rest_init/2,
         resource_exists/2,
         malformed_request/2,
         content_types_provided/2,
         to_json/2
        ]).

-record(state, {pid,
                name,
                start,
                stop,
                step
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
                                                       fun check_stop/2,
                                                       fun check_step/2,
                                                       fun check_name/2
                                                      ]).

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, to_json}
	], Req, State}.

to_json(Req, State) ->
    Pid = State#state.pid,
    Name = State#state.name,
    Start = State#state.start,
    Stop = State#state.stop,
    Step = State#state.step,
    Results = appstats_session:read(Pid, Name, Start, Stop, Step),
    Body = json_encode_results(Results),
    {Body, Req, State}.

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

check_step(Req, State) ->
    case cowboy_req:qs_val(<<"step">>, Req) of
        {undefined, Req1} ->
            {true, Req1, State};
        {Step0, Req1} ->
            Step = list_to_integer(binary_to_list(Step0)),
            case Step > 0 of
                true ->
                    {false, Req1, State#state{step=Step}};
                false ->
                    {true, Req1, State#state{step=Step}}
            end
    end.

check_name(Req, State) ->
    case cowboy_req:qs_val(<<"name">>, Req) of
        {undefined, Req1} ->
            {true, Req1, State};
        {Name, Req1} ->
            {false, Req1, State#state{name=Name}}
    end.

json_encode_results(Results) ->
    Results1 = [json_encode_entry(Entry) || Entry <- Results],
    jiffy:encode(Results1).

json_encode_entry({Epoch, undefined}) ->
    {[{<<"timestamp">>, Epoch}]};

json_encode_entry({Epoch, Stats}) ->
    {[{<<"timestamp">>, Epoch},
      {<<"count">>, proplists:get_value(n, Stats)},
      {<<"min">>, proplists:get_value(min, Stats)},
      {<<"mean">>, proplists:get_value(arithmetic_mean, Stats)},
      {<<"max">>, proplists:get_value(max, Stats)}]}.

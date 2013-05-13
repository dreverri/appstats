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

-module(appstats_event).

-export([epoch/0,
         epoch/1
        ]).

-export([new/2,
         new/3,
         is_type/2,
         is_match/2,
         field_value/2
        ]).

-include("appstats_event.hrl").

new(Type, Data) ->
    new(Type, Data, epoch()).

new(Type, Data, Time) ->
    case jsx:is_term(Data) of
        true ->
            {ok, #event{type=Type, data=Data, time=Time}};
        false ->
            {error, invalid_json}
    end.

epoch() ->
    epoch(os:timestamp()).

epoch({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000 * 1000 + Secs) * 1000 + trunc(MicroSecs/1000).

is_type(Type, #event{type=Type}) ->
    true;

is_type(_, _) ->
    false.

is_match({eq, Field, Value}, Event) ->
    field_value(Field, Event) == Value;

is_match(_Filter, _Event) ->
    false.

field_value(Field, Event) ->
    Keys = list_to_tuple(binary:split(Field, <<".">>, [global])),
    ej:get(Keys, Event#event.data).

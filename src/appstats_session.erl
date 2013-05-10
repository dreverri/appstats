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

-module(appstats_session).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new_event/2,
         new_event/3,
         epoch/0,
         epoch/1
        ]).

-export([open/1,
         start_link/1,
         write/2,
         read/5,
         timespan/1,
         names/1,
         count/1
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% TODO: sample_rate
-record(event, {name, value, timestamp=epoch(), data}).

%% TODO: move event functions to separate module
new_event(Name, Value) ->
    new_event(Name, Value, []).

new_event(Name, Value, Options) ->
    lists:foldl(fun
            ({timestamp, Timestamp}, Event) ->
                Event#event{timestamp=Timestamp};
            ({data, Data}, Event) ->
                Event#event{data=Data}
        end, #event{name=Name, value=Value}, Options).

epoch() ->
    epoch(os:timestamp()).

epoch({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000 * 1000 + Secs) * 1000 + trunc(MicroSecs/1000).

open(Path) ->
    appstats_sup:start_worker(Path).

start_link(Path) ->
    gen_server:start_link(?MODULE, Path, []).

write(Pid, Events) ->
    gen_server:call(Pid, {write, Events}).

read(Pid, Name, Start, Stop, Step) ->
    gen_server:call(Pid, {read, Name, Start, Stop, Step}, infinity).

timespan(Pid) ->
    gen_server:call(Pid, timespan).

names(Pid) ->
    gen_server:call(Pid, names, infinity).

count(Pid) ->
    gen_server:call(Pid, count).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {path, ref, count=0}).

init(Path) ->
    filelib:ensure_dir(Path),
    case eleveldb:open(Path, [{create_if_missing, true}, {compression, true}]) of
        {ok, Ref} ->
            {ok, #state{path=Path, ref=Ref}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({write, Events}, _From, State) ->
    {Updates, State1} = lists:foldl(fun event_updates/2, {[], State}, Events),
    Reply = eleveldb:write(State1#state.ref, Updates, []),
    {reply, Reply, State1};

handle_call({read, Name, Start, Stop, Step}, From, State) ->
    spawn(fun() -> fold(State#state.ref, Name, Start, Stop, Step, From) end),
    {noreply, State};

handle_call(timespan, _From, State) ->
    {ok, Itr} = eleveldb:iterator(State#state.ref, []),
    FirstKey = sext:encode({e, 0, <<>>, 0}),
    case eleveldb:iterator_move(Itr, FirstKey) of
        {ok, K1, _} ->
            {ok, K2, _} = eleveldb:iterator_move(Itr, last),
            ok = eleveldb:iterator_close(Itr),
            {e, T1, _, _} = sext:decode(K1),
            {e, T2, _, _} = sext:decode(K2),
            {reply, {T1, T2}, State};
        {error, invalid_iterator} ->
            {reply, empty, State}
    end;

handle_call(names, From, State) ->
    spawn(fun() -> fold_names(State#state.ref, From) end),
    {noreply, State};

handle_call(count, _From, State) ->
    {ok, Itr} = eleveldb:iterator(State#state.ref, []),
    case eleveldb:iterator_move(Itr, last) of
        {ok, K, _} ->
            ok = eleveldb:iterator_close(Itr),
            {e, _, _, Count} = sext:decode(K),
            {reply, Count, State};
        _ ->
            {reply, 0, State}
    end;

handle_call(ref, _From, State) ->
    {reply, State#state.ref, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

event_updates(Event, {Acc, State}) ->
    Count = State#state.count + 1,
    Timestamp = Event#event.timestamp,
    Name = Event#event.name,
    Value = Event#event.value,
    Data = Event#event.data,
    KeyBin = sext:encode({e, Timestamp, Name, Count}),
    ValueBin = term_to_binary({Value, Data}),
    %% Add an entry for each name on each write so we can later get a list of
    %% metric names. Another option would be to do a full scan each time the
    %% list is requested. A better solution for listing metric names would be
    %% nice.
    NameKey = sext:encode({n, Name}),
    {[{put, KeyBin, ValueBin}, {put, NameKey, <<>>}|Acc],
     State#state{count=Count}}.

fold(Ref, Name, Start, Stop, Step, From) ->
    Fun = fold_fun(Name, Start, Stop, Step),
    FirstKey = sext:encode({e, Start, Name, 0}),
    FoldOpts = [{first_key, FirstKey}],
    try
        Results0 = eleveldb:fold(Ref, Fun, [{Start, dict:new()}], FoldOpts),
        Results = fill_in_the_holes(Results0, Start, Stop, Step),
        Summary = summarize(Results),
        gen_server:reply(From, Summary)
    catch
        {break, Results1} ->
            Results2 = fill_in_the_holes(Results1, Start, Stop, Step),
            Summary1 = summarize(Results2),
            gen_server:reply(From, Summary1)
    end.

fill_in_the_holes(Results, Start, Stop, Step) ->
    LastSlice = slice(Stop, Start, Step),
    case Results of
        [{LastSlice, _}|_] ->
            Results;
        [{PreviousSlice, _}|_] ->
            Holes = holes(PreviousSlice + Step, LastSlice - Step, Step),
            Holes ++ Results
    end.

summarize(Results) ->
    lists:foldl(fun({T, Values}, Acc) ->
                Summary = dict:fold(fun(N, V, SliceAcc) ->
                                [{N, bear:get_statistics(V)}|SliceAcc]
                        end, [], Values),
                [{T, Summary}|Acc]
        end, [], Results).

fold_fun(TargetName, Start, Stop, Step) ->
    LastSlice = slice(Stop, Start, Step),
    fun({KeyBin, ValueBin}, All = [{PreviousSlice, Values}|Acc]) ->
            case sext:decode(KeyBin) of
                {e, Timestamp, Name, _} ->
                    Slice = slice(Timestamp, Start, Step),
                    case Slice >= LastSlice of
                        true ->
                            throw({break, All});
                        false ->
                            ok
                    end,
                    case match_name(TargetName, Name) of
                        true ->
                            {V, _Data} = binary_to_term(ValueBin),
                            if
                                Slice == PreviousSlice ->
                                    Values1 = dict:append(Name, V, Values),
                                    [{Slice, Values1}|Acc];
                                Slice == PreviousSlice + Step ->
                                    Values1 = dict:append(Name, V, dict:new()),
                                    [{Slice, Values1}|All];
                                true ->
                                    Holes = holes(PreviousSlice + Step, Slice - Step, Step),
                                    Values1 = dict:append(Name, V, dict:new()),
                                    [{Slice, Values1}|Holes ++ All]
                            end;
                        false ->
                            All
                    end;
                _ ->
                    throw({break, All})
            end
    end.

match_name(undefined, _) ->
    true;

match_name(TargetName, Name) ->
    TargetName == Name.

slice(Ts, Start, Step) ->
    trunc(Start + trunc((Ts - Start) / Step) * Step).

holes(FromSlice, ToSlice, Step) ->
    lists:reverse([{S, dict:new()} || S <- lists:seq(FromSlice, ToSlice, Step)]).

fold_names(Ref, From) ->
    Fun = fun(NameKey, Acc) ->
            case sext:decode(NameKey) of
                {n, Name} ->
                    [Name|Acc];
                _ ->
                    throw({break, Acc})
            end
    end,
    FirstKey = sext:encode({n, <<>>}),
    FoldOpts = [{first_key, FirstKey}],
    try
        Names = eleveldb:fold_keys(Ref, Fun, [], FoldOpts),
        gen_server:reply(From, Names)
    catch
        {break, Names1} ->
            gen_server:reply(From, Names1)
    end.

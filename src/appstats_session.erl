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

-export([open/1,
         start_link/1,
         write/2,
         read/6,
         summarize/5,
         timespan/1,
         types/1,
         count/1,
         close/1
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

-include("appstats_event.hrl").

-include_lib("stdlib/include/qlc.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

open(Path) ->
    appstats_sup:start_worker(Path).

start_link(Path) ->
    gen_server:start_link(?MODULE, Path, []).

write(Pid, Events) ->
    gen_server:call(Pid, {write, Events}).

%% Query :: {Type, Filters, Extract, Aggregate}
%% Type :: binary()
%% Filters :: [Filter]
%% Filter :: {FilterType, Field, Value}
%% FilterType :: eq
%% Field :: {binary()}
%% Value :: binary() | float() | integer() | true | false
%% Extract :: Field
%% Aggregate :: all
read(Pid, Start, Stop, Query, Limit, SortBy) ->
    gen_server:call(Pid, {read, Start, Stop, Query, Limit, SortBy}, infinity).

summarize(Pid, Start, Stop, Step, Query) ->
    gen_server:call(Pid, {summarize, Start, Stop, Step, Query}, infinity).

types(Pid) ->
    gen_server:call(Pid, types, infinity).

timespan(Pid) ->
    gen_server:call(Pid, timespan).

count(Pid) ->
    gen_server:call(Pid, count).

close(Pid) ->
    gen_server:call(Pid, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {path, ref, count=0}).

init(Path) ->
    filelib:ensure_dir(Path),
    Options = [{create_if_missing, true}, {compression, true}],
    case eleveldb:open(Path, Options) of
        {ok, Ref} ->
            Count = get_count(Ref),
            {ok, #state{path=Path, ref=Ref, count=Count}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({write, Events}, _From, State) ->
    {Updates, State1} = lists:foldl(fun event_updates/2, {[], State}, Events),
    Reply = eleveldb:write(State1#state.ref, Updates, []),
    {reply, Reply, State1};

handle_call({read, Start, Stop, Query, Limit, SortBy}, From, State) ->
    Fun = fun() ->
            get_events(State#state.ref, Start, Stop, Query, Limit, SortBy, From)
    end,
    spawn(Fun),
    {noreply, State};

handle_call({summarize, Start, Stop, Step, Query}, From, State) ->
    Fun = fun() ->
            get_summary(State#state.ref, Start, Stop, Step, Query, From)
    end,
    spawn(Fun),
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
            {reply, [{start, T1},{stop,  T2}], State};
        {error, invalid_iterator} ->
            {reply, [], State}
    end;

handle_call(types, From, State) ->
    spawn(fun() -> get_types(State#state.ref, From) end),
    {noreply, State};

handle_call(count, _From, State) ->
    Count = get_count(State#state.ref),
    {reply, Count, State};

handle_call(ref, _From, State) ->
    {reply, State#state.ref, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    eleveldb:close(State#state.ref).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_count(Ref) ->
    {ok, Itr} = eleveldb:iterator(Ref, []),
    case eleveldb:iterator_move(Itr, last) of
        {ok, K, _} ->
            {e, _, _, Count} = sext:decode(K),
            Count;
        {error, invalid_iterator} ->
            0
    end.

event_updates(Event, {Acc, State}) ->
    Count = State#state.count + 1,
    Time = Event#event.time,
    Type = Event#event.type,
    Data = Event#event.data,
    KeyBin = sext:encode({e, Time, Type, Count}),
    ValueBin = term_to_binary(Data),
    %% Add an entry for each type on each write so we can later get a list of
    %% metric types. Another option would be to do a full scan each time the
    %% list is requested. A better solution for listing metric types would be
    %% nice.
    TypeKey = sext:encode({t, Type}),
    {[{put, KeyBin, ValueBin}, {put, TypeKey, <<>>}|Acc],
     State#state{count=Count}}.

get_events(Ref, Start, Stop, Query, Limit, SortBy, From) ->
    Events = event_table(Ref, Start, Stop),
    QH = qlc:q([Event || Event <- Events, is_match(Query, Event)]),
    QH1 = case is_binary(SortBy) of
        true ->
            SortQH = qlc:q([{appstats_event:field_value(SortBy, Event), Event}
                            || Event <- QH]),
            qlc:q([Event || {_, Event} <- qlc:keysort(1, SortQH)]);
        false ->
            QH
    end,
    case is_valid_limit(Limit) of
        true ->
            QC = qlc:cursor(QH1),
            try
                gen_server:reply(From, qlc:next_answers(QC, Limit))
            after
                qlc:delete_cursor(QC)
            end;
        false ->
            gen_server:reply(From, qlc:eval(QH1))
    end.

get_summary(Ref, Start, Stop, Step, Query, From) ->
    Slices = slice_table(Ref, Start, Stop, Step, Query),
    QH = qlc:q([{T, bear:get_statistics(Values)} || {T, Values} <- Slices]),
    gen_server:reply(From, qlc:eval(QH)).

is_valid_limit(Limit) ->
    is_integer(Limit) andalso Limit > 0.

is_match({Type, Filters, _, _}, Event) when is_list(Filters) ->
    case appstats_event:is_type(Type, Event) of
        true ->
            lists:all(fun(Filter) ->
                        appstats_event:is_match(Filter, Event)
                end, Filters);
        false ->
            false
    end;

is_match(_, _) ->
    false.

extract({_, _, undefined, _}, _Event) ->
    1;

extract({_, _, Field, _}, Event) ->
    appstats_event:field_value(Field, Event);

extract(_Query, _Event) ->
    1.

get_types(Ref, From) ->
    Types = type_fold(Ref, fun(Type, Acc) -> [Type|Acc] end, []),
    gen_server:reply(From, Types).

type_fold(Ref, Fun, OuterAcc) ->
    OuterFun = fun(KeyBin, Acc) ->
            case sext:decode(KeyBin) of
                {t, Type} ->
                    Fun(Type, Acc);
                _ ->
                    throw({break, Acc})
            end
    end,
    FirstKey = sext:encode({t, <<>>}),
    FoldOpts = [{first_key, FirstKey}],
    try
        eleveldb:fold_keys(Ref, OuterFun, OuterAcc, FoldOpts)
    catch
        {break, Results} ->
            Results
    end.

-record(slice, {time, last_key, step, q, values=[], itr}).

slice_table(Ref, Start, Stop, Step, Query) ->
    {FirstKey, LastKey} = event_keys(Start, Stop),
    PreFun = fun(_) ->
            {ok, Itr} = eleveldb:iterator(Ref, []),
            put(iterator, Itr)
    end,
    PostFun = fun() ->
            Itr = get(iterator),
            eleveldb:iterator_close(Itr)
    end,
    TF = fun() ->
            Itr = get(iterator),
            Slice = #slice{time=Start, last_key=LastKey, step=Step, q=Query, itr=Itr},
            slice_next_item(eleveldb:iterator_move(Itr, FirstKey), Slice)
    end,
    qlc:table(TF, [{pre_fun, PreFun}, {post_fun, PostFun}]).

slice_next_item({ok, K, V}, Slice) ->
    case K >= Slice#slice.last_key of
        true ->
            [{Slice#slice.time, Slice#slice.values} | []];
        false ->
            Event = to_event(K, V),
            slice_next_event(Event, Slice)
    end;

slice_next_item(_, Slice) ->
    [{Slice#slice.time, Slice#slice.values} | []].

slice_next_event(Event, Slice) ->
    NextTime = Slice#slice.time + Slice#slice.step,
    case Event#event.time < NextTime of
        true ->
            Values = case is_match(Slice#slice.q, Event) of
                true ->
                    Value = extract(Slice#slice.q, Event),
                    [Value|Slice#slice.values];
                false ->
                    Slice#slice.values
            end,
            Itr = Slice#slice.itr,
            NextItem = eleveldb:iterator_move(Itr, next),
            slice_next_item(NextItem, Slice#slice{values=Values});
        false ->
            NextSlice = Slice#slice{values=[], time=NextTime},
            Fun = fun() -> slice_next_event(Event, NextSlice) end,
            [{Slice#slice.time, Slice#slice.values} | Fun]
    end.

event_table(Ref, Start, Stop) ->
    {FirstKey, LastKey} = event_keys(Start, Stop),
    Options = [{first_key, FirstKey}, {last_key, LastKey}],
    qlc:q([to_event(K, V) || {K, V} <- eleveldb_table(Ref, Options)]).

event_keys(Start, Stop) ->
    FirstKey = sext:encode({e, Start, <<>>, 0}),
    LastKey = sext:encode({e, Stop, <<>>, 0}),
    {FirstKey, LastKey}.

to_event(KeyBin, ValueBin) ->
    {e, Time, Type, _} = sext:decode(KeyBin),
    Data = binary_to_term(ValueBin),
    #event{type=Type, data=Data, time=Time}.

eleveldb_table(Ref, Options) ->
    Start = proplists:get_value(first_key, Options, first),
    Stop = proplists:get_value(last_key, Options),
    PreFun = fun(_) ->
            {ok, Itr} = eleveldb:iterator(Ref, Options),
            put(iterator, Itr)
    end,
    PostFun = fun() ->
            Itr = get(iterator),
            eleveldb:iterator_close(Itr)
    end,
    TF = fun() ->
            Itr = get(iterator),
            StopFun = qlc_stop_fun(Stop),
            qlc_next(eleveldb:iterator_move(Itr, Start), StopFun, Itr)
    end,
    qlc:table(TF, [{pre_fun, PreFun}, {post_fun, PostFun}]).

qlc_stop_fun(undefined) ->
    fun(K, V, Next) ->
            [{K, V} | Next]
    end;

qlc_stop_fun(Stop) ->
    fun(K, V, Next) ->
            case K >= Stop of
                true ->
                    [];
                false ->
                    [{K, V} | Next]
            end
    end.

qlc_next({ok, K, V}, Fun, Itr) ->
    Next = fun() -> qlc_next(eleveldb:iterator_move(Itr, next), Fun, Itr) end,
    Fun(K, V, Next);

qlc_next(_, _, _) ->
    [].

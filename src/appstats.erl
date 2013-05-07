-module(appstats).
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
         first/1,
         last/1,
         timespan/1,
         names/3
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
    gen_server:call(Pid, {read, Name, Start, Stop, Step}).

first(Pid) ->
    gen_server:call(Pid, first).

last(Pid) ->
    gen_server:call(Pid, last).

timespan(Pid) ->
    gen_server:call(Pid, timespan).

names(Pid, Start, Stop) ->
    gen_server:call(Pid, {names, Start, Stop}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {path, ref}).

init(Path) ->
    case eleveldb:open(Path, [{create_if_missing, true}]) of
        {ok, Ref} ->
            {ok, #state{path=Path, ref=Ref}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({write, Events}, _From, State) ->
    Updates = lists:foldl(fun event_updates/2, [], Events),
    Reply = eleveldb:write(State#state.ref, Updates, []),
    {reply, Reply, State};

handle_call({read, Name, Start, Stop, Step}, From, State) ->
    case validate_range(Start, Stop) of
        true ->
            spawn(fun() -> fold(State#state.ref, Name, Start, Stop, Step, From) end),
            {noreply, State};
        false ->
            {reply, {error, bad_range}, State}
    end;

handle_call(first, _From, State) ->
    Event = get_event(State#state.ref, first),
    {reply, Event, State};

handle_call(last, _From, State) ->
    Event = get_event(State#state.ref, last),
    {reply, Event, State};

handle_call(timespan, _From, State) ->
    {ok, Itr} = eleveldb:iterator(State#state.ref, []),
    {ok, K1, _} = eleveldb:iterator_move(Itr, first),
    {ok, K2, _} = eleveldb:iterator_move(Itr, last),
    ok = eleveldb:iterator_close(Itr),
    {e, T1, _, _} = sext:decode(K1),
    {e, T2, _, _} = sext:decode(K2),
    {reply, {T1, T2}, State};

handle_call({names, Start, Stop}, From, State) ->
    case validate_range(Start, Stop) of
        true ->
            spawn(fun() -> fold_names(State#state.ref, Start, Stop, From) end),
            {noreply, State};
        false ->
            {reply, {error, bad_range}, State}
    end.

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

event_updates(Event, Acc) ->
    Timestamp = Event#event.timestamp,
    Name = Event#event.name,
    Value = Event#event.value,
    Data = Event#event.data,
    KeyBin = sext:encode({e, Timestamp, Name, crypto:rand_bytes(20)}),
    ValueBin = term_to_binary({Value, Data}),
    [{put, KeyBin, ValueBin}|Acc].

fold(Ref, Name, Start, Stop, Step, From) ->
    Fun = fold_fun(Name, Start, Stop, Step),
    FirstKey = sext:encode({e, Start, Name, <<>>}),
    FoldOpts = [{first_key, FirstKey}],
    try
        Results = eleveldb:fold(Ref, Fun, [{Start, []}], FoldOpts),
        gen_server:reply(From, summarize(Results))
    catch
        {break, Results1} ->
            gen_server:reply(From, summarize(Results1))
    end.

summarize(Results) ->
    lists:foldl(fun({T, Values}, Acc) ->
                [{T, bear:get_statistics(Values)}|Acc]
        end, [], Results).

fold_fun(TargetName, Start, Stop, Step) ->
    fun({KeyBin, ValueBin}, All = [{PreviousSlice, Values}|Acc]) ->
            {e, Timestamp, Name, _} = sext:decode(KeyBin),
            case Timestamp >= Stop of
                true ->
                    throw({break, All});
                false ->
                    ok
            end,
            case Name == TargetName of
                true ->
                    {V, _Data} = binary_to_term(ValueBin),
                    Slice = slice(Start, Step, Timestamp),
                    if
                        Slice == PreviousSlice ->
                            [{Slice, [V|Values]}|Acc];
                        Slice == PreviousSlice + Step ->
                            [{Slice, [V]}|All];
                        true ->
                            Holes = holes(PreviousSlice, Slice, Step),
                            [{Slice, [V]}|Holes ++ All]
                    end;
                false ->
                    All
            end
    end.

validate_range(Start, Stop) ->
    Stop > Start andalso (Stop - Start) < 14400000. %% 4 hours

slice(Start, Step, Ts) ->
    trunc(Start + trunc((Ts - Start) / Step) * Step).

holes(PreviousSlice, CurrentSlice, Step) ->
    lists:reverse([{S, []} || S <- lists:seq(PreviousSlice, CurrentSlice, Step)]).

get_event(Ref, Pos) ->
    {ok, Itr} = eleveldb:iterator(Ref, []),
    {ok, KeyBin, ValueBin} = eleveldb:iterator_move(Itr, Pos),
    ok = eleveldb:iterator_close(Itr),
    {e, Timestamp, Name, _} = sext:decode(KeyBin),
    {Value, Data} = binary_to_term(ValueBin),
    {Name, Value, Timestamp, Data}.

fold_names(Ref, Start, Stop, From) ->
    Fun = fun(KeyBin, Acc) ->
            {e, Timestamp, Name, _} = sext:decode(KeyBin),
            case Timestamp >= Stop of
                true ->
                    throw({break, Acc});
                false ->
                    ok
            end,
            sets:add_element(Name, Acc)
    end,
    FirstKey = sext:encode({e, Start, <<>>, <<>>}),
    FoldOpts = [{first_key, FirstKey}],
    try
        Names = eleveldb:fold_keys(Ref, Fun, sets:new(), FoldOpts),
        gen_server:reply(From, sets:to_list(Names))
    catch
        {break, Names1} ->
            gen_server:reply(From, sets:to_list(Names1))
    end.

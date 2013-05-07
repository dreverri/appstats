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
         write/2,
         read/5,
         first/1,
         last/1
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
    gen_server:start_link(?MODULE, Path, []).

write(Pid, Events) ->
    gen_server:call(Pid, {write, Events}).

read(Pid, Name, Start, Stop, Step) ->
    gen_server:call(Pid, {read, Name, Start, Stop, Step}).

first(Pid) ->
    gen_server:call(Pid, first).

last(Pid) ->
    gen_server:call(Pid, last).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {ref}).

init(Path) ->
    case eleveldb:open(Path, [{create_if_missing, true}]) of
        {ok, Ref} ->
            {ok, #state{ref=Ref}};
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
    {reply, Event, State}.

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
    Stop > Start andalso (Stop - Start) < 1000000.

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

-module(appstats_session_test).

-include_lib("eunit/include/eunit.hrl").

appstats_test_() ->
    appstats:start(),
    Tests = [fun read/0,
             fun summarize/0,
             fun timespan/0,
             fun types/0,
             fun count/0
            ],
    [wrap(Test) || Test <- Tests].

wrap(Fun) ->
    %% Error reporting from eunit can be less than useful. This will report
    %% useful information from any errors, exits, or throws; after reporting
    %% it'll re-raise the caught exception.
    fun() ->
            try
                Fun()
            catch
                Type:Reason ->
                    error_logger:error_report([{type, Type},
                                               {reason, Reason},
                                               {st, erlang:get_stacktrace()}]),
                    erlang:Type(Reason)
            end
    end.

%% test functions

read() ->
    {ok, Pid} = open("read_test"),
    Start = appstats_event:epoch(),
    Stop = Start + 1,
    Values = lists:seq(1,1000),
    Times = [Start - 1, Start, Start + 1],
    Events = [new_event(<<"foo">>, [{<<"value">>, Value}], Time) || 
            Value <- Values, Time <- Times],
    ok = appstats_session:write(Pid, Events),
    Query = {<<"foo">>, [], {<<"value">>}, undefined},
    [Event] = appstats_session:read(Pid, Start, Stop, Query, 1, undefined),
    ?assertEqual({event, <<"foo">>, [{<<"value">>, 1}], Start}, Event).

summarize() ->
    {ok, Pid} = open("summarize_test"),
    Start = appstats_event:epoch(),
    Stop = Start + 100,
    Values = lists:seq(1,100),
    Times = lists:seq(Start, Stop),
    Events = [new_event(<<"foo">>, [{<<"value">>, V}], T) ||
            V <- Values, T <- Times],
    ok = appstats_session:write(Pid, Events),
    Query = {<<"foo">>, [], {<<"value">>}, all},
    Step = 10,
    Results = appstats_session:summarize(Pid, Start, Stop, Step, Query),
    %% expect 10 slices
    %% each slice aggregating <<"value">> in 10ms steps
    %% 100 Values per 1ms (max: 100, min: 1, mean: 50.5, n: 1000)
    ?assertEqual(10, length(Results)),
    {Start, Summary} = hd(Results),
    ?assertEqual(1, proplists:get_value(min, Summary)),
    ?assertEqual(50.5, proplists:get_value(arithmetic_mean, Summary)),
    ?assertEqual(100, proplists:get_value(max, Summary)),
    ?assertEqual(1000, proplists:get_value(n, Summary)).

timespan() ->
    {ok, Pid} = open("timespan_test"),
    Start = appstats_event:epoch(),
    Stop = Start + 1000,
    Step = 100,
    Times = lists:seq(Start, Stop, Step),
    Values = lists:seq(1,100),
    Events = [new_event(<<"foo">>, [{<<"value">>, V}], T) ||
            V <- Values, T <- Times],
    ok = appstats_session:write(Pid, Events),
    Timespan = appstats_session:timespan(Pid),
    ?assertEqual(Start, proplists:get_value(start, Timespan)),
    ?assertEqual(Stop, proplists:get_value(stop, Timespan)).

types() ->
    {ok, Pid} = open("types"),
    Types = [<<"foo">>, <<"bar">>, <<"baz">>],
    Start = appstats_event:epoch(),
    Stop = Start + 1000,
    Step = 100,
    Times = lists:seq(Start, Stop, Step),
    Values = lists:seq(1,100),
    Events = [new_event(Type, [{<<"value">>, V}], T) ||
            Type <- Types, V <- Values, T <- Times],
    ok = appstats_session:write(Pid, Events),
    Results = appstats_session:types(Pid),
    [?assert(lists:member(Type, Results)) || Type <- Types].

count() ->
    {ok, Pid} = open("count"),
    Start = appstats_event:epoch(),
    Stop = Start + 1000,
    Step = 100,
    Times = lists:seq(Start, Stop, Step),
    Values = lists:seq(1,100),
    Events = [new_event(<<"foo">>, [{<<"value">>, V}], T) ||
            V <- Values, T <- Times],
    ok = appstats_session:write(Pid, Events),
    %% 11 steps, 100 events per step => 1000 total events
    ?assertEqual(1100, appstats_session:count(Pid)).

%% utility functions

open(Name) ->
    os:cmd(["rm -rf ", Name]),
    appstats_session:open(Name).

new_event(Type, Data, Time) ->
    case appstats_event:new(Type, Data, Time) of
        {ok, Event} ->
            Event;
        Error ->
            throw(Error)
    end.

-module(appstats_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    start_app(),
    {ok, Pid} = appstats:open("simple_test"),
    Start = appstats:epoch(),
    Events = [appstats:new_event(N, V, [{timestamp, T}]) || V <- lists:seq(1,1000),
                                                            N <- [<<"foo">>, <<"bar">>, <<"baz">>],
                                                            T <- [Start - 1, Start, Start + 1]],
    ok = appstats:write(Pid, Events),
    [{Time, Summary}] = appstats:read(Pid, <<"foo">>, Start, Start + 1, 1),
    ?assertEqual(Start, Time),
    ?assertEqual(13, length(Summary)).

first_and_last_test() ->
    start_app(),
    os:cmd("rm -rf first_and_last"),
    {ok, Pid} = appstats:open("first_and_last"),
    T = appstats:epoch(),
    Events = [appstats:new_event(<<"foo">>, V, [{timestamp, T+V}]) || V <- lists:seq(0,10)],
    ok = appstats:write(Pid, Events),
    {_, _, FirstTime, _} = appstats:first(Pid),
    {_, _, LastTime, _} = appstats:last(Pid),
    ?assertEqual(T, FirstTime),
    ?assertEqual(T+10, LastTime).

timespan_test() ->
    start_app(),
    os:cmd("rm -rf timespan"),
    {ok, Pid} = appstats:open("timespan"),
    T = appstats:epoch(),
    Events = [appstats:new_event(<<"foo">>, V, [{timestamp, T+V}]) || V <- lists:seq(0,10)],
    ok = appstats:write(Pid, Events),
    {T1, T2} = appstats:timespan(Pid),
    ?assertEqual({T, T+10}, {T1, T2}).

names_test() ->
    start_app(),
    {ok, Pid} = appstats:open("names"),
    T = appstats:epoch(),
    Events = [appstats:new_event(N, V, [{timestamp, T+V}]) || V <- lists:seq(0,10),
                                                              N <- [<<"foo">>, <<"bar">>]],
    Events1 = [appstats:new_event(<<"baz">>, 11, [{timestamp, T+11}])|Events],
    ok = appstats:write(Pid, Events1),
    Names = appstats:names(Pid, T, T+10),
    ?assert(lists:member(<<"foo">>, Names)),
    ?assert(lists:member(<<"bar">>, Names)),
    ?assertNot(lists:member(<<"baz">>, Names)).

start_app() ->
    start_app(eleveldb),
    start_app(appstats).

start_app(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, _}} ->
            ok;
        Error ->
            throw({start_app_error, App, Error})
    end.

-module(appstats_session_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    appstats:start(),
    {ok, Pid} = appstats_session:open("simple_test"),
    Start = appstats_session:epoch(),
    Events = [appstats_session:new_event(N, V, [{timestamp, T}]) || V <- lists:seq(1,1000),
                                                            N <- [<<"foo">>, <<"bar">>, <<"baz">>],
                                                            T <- [Start - 1, Start, Start + 1]],
    ok = appstats_session:write(Pid, Events),
    [{Time, [{_, Summary}]}] = appstats_session:read(Pid, <<"foo">>, Start, Start + 1, 1),
    ?assertEqual(Start, Time),
    ?assertEqual(13, length(Summary)).

first_and_last_test() ->
    appstats:start(),
    os:cmd("rm -rf first_and_last"),
    {ok, Pid} = appstats_session:open("first_and_last"),
    T = appstats_session:epoch(),
    Events = [appstats_session:new_event(<<"foo">>, V, [{timestamp, T+V}]) || V <- lists:seq(0,10)],
    ok = appstats_session:write(Pid, Events),
    {_, _, FirstTime, _} = appstats_session:first(Pid),
    {_, _, LastTime, _} = appstats_session:last(Pid),
    ?assertEqual(T, FirstTime),
    ?assertEqual(T+10, LastTime).

timespan_test() ->
    appstats:start(),
    os:cmd("rm -rf timespan"),
    {ok, Pid} = appstats_session:open("timespan"),
    T = appstats_session:epoch(),
    Events = [appstats_session:new_event(<<"foo">>, V, [{timestamp, T+V}]) || V <- lists:seq(0,10)],
    ok = appstats_session:write(Pid, Events),
    {T1, T2} = appstats_session:timespan(Pid),
    ?assertEqual({T, T+10}, {T1, T2}).

names_test() ->
    appstats:start(),
    {ok, Pid} = appstats_session:open("names"),
    T = appstats_session:epoch(),
    Events = [appstats_session:new_event(N, V, [{timestamp, T+V}]) || V <- lists:seq(0,10),
                                                              N <- [<<"foo">>, <<"bar">>]],
    Events1 = [appstats_session:new_event(<<"baz">>, 11, [{timestamp, T+11}])|Events],
    ok = appstats_session:write(Pid, Events1),
    Names = appstats_session:names(Pid, T, T+10),
    ?assert(lists:member(<<"foo">>, Names)),
    ?assert(lists:member(<<"bar">>, Names)),
    ?assertNot(lists:member(<<"baz">>, Names)).

sup_test() ->
    appstats:start(),
    {ok, Pid1} = appstats_session:open("a"),
    {ok, Pid2} = appstats_session:open("b"),
    Workers = appstats_sup:list_workers(),
    Pids = [element(2, appstats_sup:get_worker(Id)) || Id <- Workers],
    ?assert(lists:member(Pid1, Pids)),
    ?assert(lists:member(Pid2, Pids)).

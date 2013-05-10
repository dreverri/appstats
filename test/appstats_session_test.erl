-module(appstats_session_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    appstats:start(),
    os:cmd("rm -rf simple_test"),
    {ok, Pid} = appstats_session:open("simple_test"),
    Start = appstats_session:epoch(),
    Stop = Start + 1,
    Values = lists:seq(1,1000),
    Names = [<<"foo">>, <<"bar">>, <<"baz">>],
    Times = [Start - 1, Start, Start + 1],
    Events = [new_event(N, V, [{timestamp, T}]) || V <- Values,
                                                   N <- Names,
                                                   T <- Times],
    ok = appstats_session:write(Pid, Events),
    [{Time,
      [{_, Summary}]}] = appstats_session:read(Pid, <<"foo">>, Start, Stop, 1),
    ?assertEqual(Start, Time),
    ?assertEqual(13, length(Summary)).

timespan_test() ->
    appstats:start(),
    os:cmd("rm -rf timespan"),
    {ok, Pid} = appstats_session:open("timespan"),
    T = appstats_session:epoch(),
    Values = lists:seq(0,10),
    Events = [new_event(<<"foo">>, V, [{timestamp, T+V}]) || V <- Values],
    ok = appstats_session:write(Pid, Events),
    {T1, T2} = appstats_session:timespan(Pid),
    ?assertEqual({T, T+10}, {T1, T2}).

timespan2_test() ->
    appstats:start(),
    os:cmd("rm -rf timespan2"),
    {ok, Pid} = appstats_session:open("timespan2"),
    ?assertEqual(empty, appstats_session:timespan(Pid)).

names_test() ->
    appstats:start(),
    {ok, Pid} = appstats_session:open("names"),
    T = appstats_session:epoch(),
    Values = lists:seq(0,10),
    Names = [<<"foo">>, <<"bar">>, <<"baz">>],
    Events = [new_event(N, V, [{timestamp, T+V}]) || V <- Values,
                                                     N <- Names],
    ok = appstats_session:write(Pid, Events),
    Names1 = appstats_session:names(Pid),
    ?assert(lists:member(<<"foo">>, Names1)),
    ?assert(lists:member(<<"bar">>, Names1)),
    ?assert(lists:member(<<"baz">>, Names1)).

count_test() ->
    appstats:start(),
    os:cmd("rm -rf count"),
    {ok, Pid} = appstats_session:open("count"),
    Count = 786,
    Events = [new_event(<<"foo">>, V) || V <- lists:seq(1, Count)],
    ok = appstats_session:write(Pid, Events),
    ?assertEqual(Count, appstats_session:count(Pid)).

sup_test() ->
    appstats:start(),
    {ok, Pid1} = appstats_session:open("a"),
    {ok, Pid2} = appstats_session:open("b"),
    Workers = appstats_sup:list_workers(),
    Pids = [element(2, appstats_sup:get_worker(Id)) || Id <- Workers],
    ?assert(lists:member(Pid1, Pids)),
    ?assert(lists:member(Pid2, Pids)).

new_event(N, V) ->
    new_event(N, V, []).

new_event(N, V, Options) ->
    appstats_session:new_event(N, V, Options).

-module(appstats_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
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
    os:cmd("rm -rf start_time"),
    {ok, Pid} = appstats:open("start_time"),
    T = appstats:epoch(),
    Events = [appstats:new_event(<<"foo">>, V, [{timestamp, T+V}]) || V <- lists:seq(0,10)],
    ok = appstats:write(Pid, Events),
    {_, _, FirstTime, _} = appstats:first(Pid),
    {_, _, LastTime, _} = appstats:last(Pid),
    ?assertEqual(T, FirstTime),
    ?assertEqual(T+10, LastTime).

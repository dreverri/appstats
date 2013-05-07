-module(appstats_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    {ok, Pid} = appstats:open("data"),
    Start = appstats:epoch(),
    Events = [appstats:new_event(M, V, [{timestamp, T}]) || V <- lists:seq(1,1000),
                                 M <- [<<"foo">>, <<"bar">>, <<"baz">>],
                                 T <- [Start - 1, Start, Start + 1]],
    ok = appstats:write(Pid, Events),
    [{Time, Summary}] = appstats:read(Pid, <<"foo">>, Start, Start + 1, 1),
    ?assertEqual(Start, Time),
    ?assertEqual(13, length(Summary)).

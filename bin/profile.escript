#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    RootDir = filename:join(filename:dirname(escript:script_name()), ".."),
    code:add_paths(filelib:wildcard(RootDir ++ "/deps/*/ebin")),
    code:add_paths([RootDir ++ "/ebin"]),
    appstats:start(),
    {ok, Pid} = open(RootDir ++ "/data/profile"),
    Start = appstats_event:epoch(),
    Stop = Start + 100,
    Values = lists:seq(1,100),
    Times = lists:seq(Start, Stop),
    Events = [new_event(<<"foo">>, [{<<"value">>, V}], T) ||
            V <- Values, T <- Times],
    ok = appstats_session:write(Pid, Events),
    Query = {<<"foo">>, [], {<<"value">>}, all},
    Step = 10,
    fprof:trace([start, {procs, [Pid]}]),
    appstats_session:summarize(Pid, Start, Stop, Step, Query),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse().

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

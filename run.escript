#!/usr/bin/env escript
%% -*- erlang -*-

main([Arg1]) ->
    code:add_paths(filelib:wildcard("deps/*/ebin")),
    code:add_paths(filelib:wildcard("ebin")),
    appstats:start(),
    {ok, Pid} = appstats_session:open("data/test"),
    Start = 1368207760000,
    Stop = 1368207780000,
    case Arg1 of
        "load" ->
            timeit(fun() -> load(Pid, Start, Stop) end);
        "names" ->
            timeit(fun() -> appstats_session:names(Pid) end);
        "read" ->
            timeit(fun() -> appstats_session:read(Pid, undefined, Start, Stop, 1000) end)
    end;

main([]) ->
    main(["names"]).

load(Pid, Start, Stop) ->
    Events = [appstats_session:new_event(N, V, [{timestamp, T}]) ||
            N <- [list_to_binary(integer_to_list(X)) || X <- lists:seq(1,100)],
            V <- lists:seq(1,100),
            T <- lists:seq(Start, Stop, 1000)],
    appstats_session:write(Pid, Events).

timeit(Fun) ->
    {T, _} = timer:tc(Fun),
    io:format("~pms~n", [T/1000]).

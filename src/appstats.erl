-module(appstats).

-export([start/0]).

start() ->
    start(crypto),
    start(ranch),
    start(cowboy),
    start(eleveldb),
    start(appstats).

start(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, _}} ->
            ok;
        Error ->
            throw({error, App, Error})
    end.

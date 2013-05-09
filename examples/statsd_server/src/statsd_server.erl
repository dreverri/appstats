-module(statsd_server).

-export([start/0]).

start() ->
    appstats:start(),
    application:start(statsd_server).

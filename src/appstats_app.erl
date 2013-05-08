-module(appstats_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
                {'_',
                 [{"/sessions", appstats_http_sessions, []},
                  {"/session/:id/timespan", appstats_http_timespan, []},
                  {"/session/:id/names", appstats_http_names, []},
                  {"/session/:id/data", appstats_http_data, []},
                 
                  %% "/" -> index.html
                  {"/", cowboy_static,
                   [{directory, {priv_dir, appstats, [<<"www">>]}},
                    {mimetypes, {fun mimetypes:path_to_mimes/2, default}},
                    {file, <<"index.html">>}
                   ]},

                  %% "/anything" -> anything
                  {"/[...]", cowboy_static,
                   [{directory, {priv_dir, appstats, [<<"www">>]}},
                    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                   ]}
                 ]}
                ]),
	cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
    appstats_sup:start_link().

stop(_State) ->
    ok.

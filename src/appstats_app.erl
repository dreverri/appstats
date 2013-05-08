%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

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

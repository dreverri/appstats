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

-module(riakc_example).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {session,
                client,
                timer,
                bucket= <<"bucket">>,
                events=[]
               }).

init(_) ->
    appstats:start(),
    case appstats_session:open("data/riakc_example") of
        {ok, Session} ->
            case riakc_pb_socket:start_link("127.0.0.1", 8087) of
                {ok, Client} ->
                    State = #state{session=Session, client=Client},
                    State1 = manage_timer(State),
                    gen_server:cast(self(), get),
                    {ok, State1};
                {error, Reason} ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(get, State) ->
    Key = term_to_binary(trunc(random:uniform(1000))),
    T1 = os:timestamp(),
    riakc_pb_socket:get(State#state.client, State#state.bucket, Key),
    T2 = os:timestamp(),
    LatencyMs = timer:now_diff(T2, T1)/1000,
    E = appstats_session:new_event(<<"riakc.get">>, LatencyMs),
    Events = [E|State#state.events],
    timer:sleep(100),
    gen_server:cast(self(), get),
    {noreply, State#state{events=Events}}.

handle_info(report, State) ->
    appstats_session:write(State#state.session, State#state.events),
    State1 = manage_timer(State#state{events=[]}),
    {noreply, State1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

manage_timer(State) ->
    case is_reference(State#state.timer) of
        true ->
            erlang:cancel_timer(State#state.timer);
        false ->
            ok
    end, 
    Timer = erlang:send_after(1000, self(), report),
    State#state{timer=Timer}.

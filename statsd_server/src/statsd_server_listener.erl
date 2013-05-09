-module(statsd_server_listener).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

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

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {socket, session, timer, acc=dict:new()}).

init(Options) ->
    Port = proplists:get_value(port, Options, 8125),
    case gen_udp:open(Port, [{active, once}]) of
        {ok, Socket} ->
            case appstats_session:open("data/statsd") of
                {ok, Session} ->
                    {ok, #state{socket=Socket, session=Session}};
                {error, Reason} ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, _Host, _Port, Packet}, State) ->
    State1 = maybe_start_timer(State),
    Acc = process_packet(Packet, State1#state.acc),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State1#state{acc=Acc}};

handle_info(flush, State) ->
    Data = dict:fold(fun
                ({gauge, Name}, Value, Acc) ->
                    Event = new_event(Name, Value),
                    [Event|Acc];
                ({count, Name}, Value, Acc) ->
                    Event = new_event(Name, Value),
                    [Event|Acc];
                ({measure, Name}, Values, Acc) ->
                    Events = [new_event(Name, Value, [{timestamp, T}]) || {Value, T} <- Values],
                    Events ++ Acc
            end, [], State#state.acc),
    appstats_session:write(State#state.session, Data),
    State1 = cancel_timer(State#state{acc=dict:new()}),
    {noreply, State1};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

flush_interval() ->
    case application:get_env(statsd_server, flush_interval) of
        {ok, V} ->
            V;
        _ ->
            10000
    end.

new_event(Name, Value) ->
    new_event(Name, Value, []).

new_event(Name, Value, Options) ->
    appstats_session:new_event(iolist_to_binary(Name), Value, Options).

maybe_start_timer(State) ->
    case is_reference(State#state.timer) of
        false ->
            Timer = erlang:send_after(flush_interval(), self(), flush),
            State#state{timer=Timer};
        true ->
            State
    end.

cancel_timer(State) ->
    case is_reference(State#state.timer) of
        true ->
            erlang:cancel_timer(State#state.timer),
            State#state{timer=undefined};
        false ->
            State
    end.

process_packet(Packet, Acc) ->
    Data = string:tokens(Packet, "\n"),
    lists:foldl(fun process_data/2, Acc, Data).

process_data(MetricString, Acc) ->
    case process_metric(MetricString) of
        {ok, {Bucket, Value, "g", _SampleRate}} ->
            dict:store({gauge, Bucket}, Value, Acc);
        {ok, {Bucket, Value, "ms", _SampleRate}} ->
            %% TODO: use sample rate
            T = appstats_session:epoch(),
            dict:append({measure, Bucket}, {Value, T}, Acc);
        {ok, {Bucket, Value, "c", SampleRate}} ->
            Value1 = Value * (1 / SampleRate),
            dict:update_counter({count, Bucket}, Value1, Acc);
        _Other ->
            %% TODO: sets
            Acc
    end.

process_metric(Metric) ->
    case string:tokens(Metric, ":") of
        [Bucket, Info] ->
            case string:tokens(Info, "|") of
                [Value, Type] ->
                    process_metric1(Bucket, Value, Type, 1.0);
                [Value, Type, SampleRate] ->
                    case process_sample_rate(SampleRate) of
                        {ok, SampleRate1} ->
                            process_metric1(Bucket, Value, Type, SampleRate1);
                        error ->
                            {error, sample_rate}
                    end;
                _ ->
                    {error, info}
            end;
        _ ->
            {error, bucket}
    end.

process_metric1(Bucket, Value, Type, SampleRate) ->
    case process_value(Value, Type) of
        {ok, Value1} ->
            {ok, {Bucket, Value1, Type, SampleRate}};
        Error ->
            Error
    end.

process_sample_rate(SampleRate) ->
    try
        SampleRate1 = list_to_float(SampleRate),
        case is_float(SampleRate1) andalso SampleRate1 =< 1 of
            true ->
                {ok, SampleRate1};
            false ->
                error
        end
    catch
        _:_ ->
            error
    end.

process_value(Value, Type) ->
    case Type of
        "c" ->
            value_to_number(Value);
        "ms" ->
            value_to_number(Value);
        "g" ->
            value_to_number(Value);
        "s" ->
            {ok, Value};
        _ ->
            {error, unrecognized_type}
    end.

value_to_number(Value) ->
    try
        Value1 = list_to_integer(Value),
        {ok, Value1}
    catch
        _:_ ->
            try
                Value2 = list_to_float(Value),
                {ok, Value2}
            catch
                _:_ ->
                    {error, value_to_number_failed}
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_packet_test() ->
    try
        Packet = "gorets:1|c\ngorets:2|c\nglork:320|ms\nglork:330|ms\ngaugor:333|g",
        Dict = process_packet(Packet, dict:new()),
        ?assertEqual(3.0, dict:fetch({count, "gorets"}, Dict)),
        ?assertEqual([320, 330], dict:fetch({measure, "glork"}, Dict)),
        ?assertEqual(333, dict:fetch({gauge, "gaugor"}, Dict))
    catch
        Type:Reason ->
            error_logger:error_report([{type, Type},
                                       {reason, Reason},
                                       {stacktrace, erlang:get_stacktrace()}]),
            erlang:Type(Reason)
    end.

-endif.

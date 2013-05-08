-module(appstats_http_timespan).

-export([init/3,
         rest_init/2,
         resource_exists/2,
         content_types_provided/2,
         to_json/2
        ]).

init(_Transport, _Req, _State) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _Args) ->
    {ok, Req, ok}.

resource_exists(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    case appstats_sup:get_worker(Id) of
        {ok, Pid} ->
            {true, Req1, Pid};
        _ ->
            {false, Req1, State}
    end.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, to_json}
	], Req, State}.

to_json(Req, Pid) ->
    {T1, T2} = appstats_session:timespan(Pid),
    Body = jiffy:encode({[{<<"start">>, T1}, {<<"stop">>, T2}]}),
    {Body, Req, Pid}.

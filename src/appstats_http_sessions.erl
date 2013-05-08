-module(appstats_http_sessions).

-export([init/3,
         rest_init/2,
         content_types_provided/2,
         to_json/2
        ]).

init(_Transport, _Req, _State) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _Args) ->
    {ok, Req, ok}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, to_json}
	], Req, State}.

to_json(Req, State) ->
    Workers = appstats_sup:list_workers(),
    {jiffy:encode(Workers), Req, State}.

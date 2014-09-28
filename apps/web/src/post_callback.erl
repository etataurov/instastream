-module(post_callback).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	{ok, Req3} = handle_req(Method, Req2),
	{ok, Req3, State}.

handle_req(<<"POST">>, Req) ->
	io:format("~w~n", [Req]),
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),
	Echo = proplists:get_value(<<"echo">>, PostVals),
	echo(Echo, Req2);

handle_req(<<"GET">>, Req) ->
	io:format("~w~n", [Req]),
    {QsVal, Req2} = cowboy_req:qs_val(<<"hub.challenge">>, Req),
    cowboy_req:reply(200, [], QsVal, Req2).	

echo(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(Echo, Req) ->
	notifier ! {notify, Echo},
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

-module(callback_handler).

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
    {ok, Body, Req2} = cowboy_req:body(Req),
    manager:new_instaevent(Body),
    cowboy_req:reply(200, Req2);
 
handle_req(<<"GET">>, Req) ->
    {QsVal, Req2} = cowboy_req:qs_val(<<"hub.challenge">>, Req),
    cowboy_req:reply(200, [], QsVal, Req2).

terminate(_Reason, _Req, _State) ->
	ok.

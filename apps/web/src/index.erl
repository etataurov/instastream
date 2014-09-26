-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() ->
	#dtl{file="index", bindings=[{body, body()}]}.
body() -> 
	io:format("RENDER~n"),
	Body = [
	 #panel{body = <<"Hello records template!">>},
	 #span{id=updater}
	],
	wf:comet(fun() -> loop(updater, "olo") end),
	Body.

event(init) ->
	io:format("INIT~n"),
	wf:reg(room).

loop(Elem, Text) ->
	timer:sleep(1000),
	wf:update(Elem, #span{id=updater, body=Text}),
	wf:flush(room),
	loop(Elem, ["olo"|Text]).

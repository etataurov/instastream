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
	{ok, Pid} = wf:comet(fun() -> loop_start(updater) end),
	register(listener, Pid),
	Body.

event(init) ->
	io:format("INIT~n"),
	wf:reg(room);

event(terminate) ->
	io:format("Terminate"),
	wf:unreg(room),
	listener ! stop.

loop_start(E) ->
	notifier ! {subscribe, self()},
	loop(E).

loop(Elem) ->
	receive
		{new, Text} ->
	       wf:update(Elem, #span{id=updater, body=Text}),
	       wf:flush(room),
	       loop(Elem);
		stop ->
			ok
	after 1000 ->
	    io:format("Timeout~n"),
	    loop(Elem)
	end.

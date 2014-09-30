-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() ->
	#dtl{file="index", bindings=[{body, body()}]}.
body() -> 
	#panel{id=updater}.

event(init) ->
	{ok, Pid} = wf:comet(fun() -> loop_start(updater) end),
    put(listener, Pid),
	register(listener, Pid),
	wf:reg(room);

event(terminate) ->
	wf:unreg(room),
	get(listener) ! stop,
    erase(listener).

loop_start(E) ->
    gproc_ps:subscribe(l, instaevent),
	loop(E).

loop(Elem) ->
	receive
		{gproc_ps_event, instaevent, Text} ->
           case maps:get(<<"type">>, Text) of
               <<"image">> ->
                   try
                      wf:insert_top(Elem,
                                       #panel{body=[
                                        #link{href=maps:get(<<"link">>, Text),
                                              body=#image{src=maps:get(<<"url">>, 
                                                                       maps:get(<<"low_resolution">>, 
                                                                                maps:get(<<"images">>, Text)))}},
                                        #panel{body=wf:f("by ~s", [maps:get(<<"username">>, maps:get(<<"user">>, Text))])}]})
                      %TODO seconds ago
                   catch
                     %% FIXME what the reason of error??
                     _:_ -> ok
                   end,
                   % need to use active once and sleep in some other place
                   timer:sleep(1000), % leads to increasing queue
                   wf:flush(room);
                _ ->
                   ok
           end,
	       loop(Elem);
		stop ->
            gproc_ps:unsubscribe(l, instaevent),
			ok
	after 1000 ->
	    loop(Elem)
	end.

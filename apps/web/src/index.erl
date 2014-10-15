-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() ->
	#dtl{file="index", bindings=[{body, body()}]}.
body() -> 
	#panel{id=updater}.

event(init) ->
	{ok, Pid} = wf:async(fun() -> loop_start(updater) end),
    put(listener, Pid),
	register(listener, Pid),
	wf:reg(room);

event(terminate) ->
	wf:unreg(room),
	get(listener) ! stop,
    erase(listener).

loop_start(E) ->
    Tag = <<"instagood">>,
    gproc_ps:create_single(l, {tag, Tag}),
    manager:subscribe(Tag),
	loop(E, Tag).

loop(Elem, Tag) ->
	receive
        % TODO add monitoring to resubscribe
		{gproc_ps_event, {tag, Tag}, Text} ->
           io:format("EVENTT~n"),
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
                     _:_ -> io:format("ERROR~n"), ok
                   end,
                   timer:sleep(1000),
                   wf:flush(room);
                _ ->
                   ok
           end,
           gproc_ps:enable_single(l, {tag, Tag}),
	       loop(Elem, Tag);
		stop ->
			ok
	end.

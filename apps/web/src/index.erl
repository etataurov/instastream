-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() ->
	#dtl{file="index", bindings=[{body, body()}]}.
body() -> 
    [#panel{body=[
      #radiogroup { id=myRadio, body=[
            #radio { id=myRadio1, body="#cats", value = <<"cats">>, postback={tag, <<"cats">>}, checked=true }, #br{},
            #radio { id=myRadio2, body="#instagood", value = <<"instagood">>, postback={tag, <<"instagood">>} }, #br{}
    ]}
    ]},
	#panel{id=updater}].

event(init) ->
    ok;

event(terminate) ->
    io:format("DISCON~n"),
    get(listener) ! stop,
    erase(listener);

event({tag, Tag}) ->
    case get(listener) of
        undefined -> pass;
        Process -> Process ! stop
    end,
    {ok, Pid} = wf:async(random:uniform(100000), fun() -> loop_start(updater, Tag) end),
    put(listener, Pid),
    wf:reg(Tag).

loop_start(E, Tag) ->
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
                   %timer:sleep(1000),
                   wf:flush(Tag);
                _ ->
                   ok
           end,
           gproc_ps:enable_single(l, {tag, Tag}),
	       loop(Elem, Tag);
		stop ->
            io:format("LOOP STOPPED~n"),
            gproc_ps:delete_single(l, {tag, Tag}),
			ok
	end.

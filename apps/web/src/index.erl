-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() ->
	#dtl{file="index", bindings=[{body, body()}]}.
body() -> 
    [#panel{body=[
      #radiogroup { id=radioGroup, body=[
            #radio { id=catsRadio, body="#cats", value = <<"cats">>, postback={tag, <<"cats">>}, checked=true }, #br{},
            #radio { id=goodRadio, body="#instagood", value = <<"instagood">>, postback={tag, <<"instagood">>} }, #br{}
    ]}
    ]},
	#panel{id=updater}].

event(init) ->
    ok;

event(terminate) ->
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
        {gproc_ps_event, {tag, Tag}, Text} ->
           case maps:get(<<"type">>, Text) of
               <<"image">> ->
                  wf:insert_top(Elem,
                                   #panel{body=[
                                    #link{href=maps:get(<<"link">>, Text),
                                          body=#image{src=maps:get(<<"url">>,
                                                                   maps:get(<<"low_resolution">>,
                                                                            maps:get(<<"images">>, Text)))}},
                                    #panel{body=wf:f("by ~s", [maps:get(<<"username">>, maps:get(<<"user">>, Text))])}]}),
                   wf:flush(Tag);
                _ ->
                   ok
           end,
           gproc_ps:enable_single(l, {tag, Tag}),
	       loop(Elem, Tag);
		stop ->
            gproc_ps:delete_single(l, {tag, Tag}),
			ok
	end.

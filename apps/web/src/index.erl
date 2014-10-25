-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() ->
	#dtl{file="index", bindings=[{body, body()}]}.
body() -> 
    [#panel{id=butPanel, body=[
       #button{id=catsButton, class= <<"btn btn-primary btn-lg btn-block">>, 
               body= <<"show #cats">>, postback={tag, <<"cats">>}}
    ]},
	#panel{id=updater}].

event(init) ->
    ok;

event(terminate) ->
    ok;

event({tag, Tag}) ->
    wf:update(butPanel, #h1{body= << <<"#">>/binary, Tag/binary>>}),
    {ok, _Pid} = wf:async(Tag, fun() -> loop_start(updater, Tag) end),
    wf:reg(Tag).

loop_start(E, Tag) ->
    gproc_ps:create_single(l, {tag, Tag}),
    manager:subscribe(Tag),
	loop(E, Tag).

insert_image(Elem, Img) ->
    wf:insert_top(Elem,
                   #panel{class= <<"row">>, body=#panel{class= <<"col-md-4 col-md-offset-4">>, body=[
                    #link{href=maps:get(<<"link">>, Img),
                          body=#image{src=maps:get(<<"url">>,
                                        maps:get(<<"low_resolution">>,
                                        maps:get(<<"images">>, Img))),
                                     class= <<"img-thumbnail">>}},
                    #panel{body=wf:f("by ~s", [maps:get(<<"username">>, maps:get(<<"user">>, Img))])}]}}).


loop(Elem, Tag) ->
    receive
        {gproc_ps_event, {tag, Tag}, Text} ->
           case maps:get(<<"type">>, Text) of
               <<"image">> ->
                   insert_image(Elem, Text),
                   wf:flush(Tag);
                _ ->
                   ok
           end,
           timer:sleep(500),
           gproc_ps:enable_single(l, {tag, Tag}),
	       loop(Elem, Tag);
		_ ->
            gproc_ps:delete_single(l, {tag, Tag}),
			stop
	end.

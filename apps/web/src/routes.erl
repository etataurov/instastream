-module(routes).
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

finish(State, Ctx) ->
    {ok, State, Ctx}.

init(State, Ctx) ->
    Path = wf:path(Ctx#cx.req),
    {ok, State, Ctx#cx{path=Path,module=route_prefix(Path)}}.

%route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

%%not sure if we need this
%route(<<"callback">>) -> post_callback;
route(_) -> index.     % always return `index` handler for any url.

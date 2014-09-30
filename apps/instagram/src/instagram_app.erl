-module(instagram_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/callback", callback_handler, []}]}
    ]),
    cowboy:start_http(instagram_callback, 100, [{port, 8081}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    instagram_sup:start_link().

stop(_State) ->
    ok.

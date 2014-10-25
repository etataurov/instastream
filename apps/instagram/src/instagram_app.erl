-module(instagram_app).

-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

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

prep_stop(State) ->
    % FIXME this does not properly stops manager
    instagram_sup:stop(),
    State.

stop(_State) ->
    ok.

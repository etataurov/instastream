-module(web_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:ensure_all_started(web),     % start all dependent applications
    application:set_env(n2o, route, routes), % setup router module
    application:set_env(n2o, port, 8080), % setup router module
    application:start(web).                  % start application

start(_StartType, _StartArgs) ->
    web_sup:start_link().

stop(_State) ->
    ok.

-module(web_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, _} = cowboy:start_http(http, 3, [{port, wf:config(n2o,port,8080)}],
                                       [{env, [{dispatch, rules()}]}]),
  register(notifier, spawn(fun() -> listen_loop([]) end)),
  {ok, { {one_for_one, 5, 10}, []} }.

mime() -> [{mimetypes,cow_mimetypes,all}].

rules() ->
  cowboy_router:compile([
    {'_', [                  %% handle all domains
       {"/", n2o_cowboy, []},  %% handle all urls
       {"/n2o/[...]", n2o_dynalo, {dir, "deps/n2o/priv", mime()}},
       {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
	   {"/callback", post_callback, []}
     ]}
  ]).


listen_loop(Subscribers) ->
	%% TODO maybe multiple subs for every district
	Params = [
			  {"client_id", ""},
			  {"client_secret", ""},
			  {"object", "geography"},
			  {"aspect", "media"},
			  {"lat", "60.605514"},
			  {"lng", "56.838607"},
			  {"radius", "5000"},
			  {"callback_url", "http://instaweather.zzzz.io:8080/callback"}
			 ],
	{ok, StatusCode, RespHeaders, ClientRef} = hackney:request(post, 
		"https://api.instagram.com/v1/subscriptions/",
        [], 
		{form, Params}
    ),
	{ok, Body} = hackney:body(ClientRef),
	io:format("body: ~p~n~n", [Body]),
	%% TODO unsubscribe
	%% TODO json parser
	receive
		{subscribe, Pid} -> listen_loop([Pid|Subscribers]);
		{notify, Message} ->
			[P ! {new, Message} || P <- Subscribers],
			listen_loop(Subscribers)
	end.

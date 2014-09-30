-module(manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([new_instaevent/1]).

-record(state, {geo_id, min_id}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_instaevent(Body) ->
    gen_server:cast(?MODULE, {instaev, Body}). 
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    gen_server:cast(?MODULE, request_subscription),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(request_subscription, State) ->
    GeoId = request_subscribe(),
    {noreply, State#state{geo_id=GeoId}};
handle_cast({instaev, Body}, State) ->
    %Data = jiffy:decode(Body, [return_maps]),
    case handle_updated_objects(State) of
        none -> {noreply, State};
        MinId -> {noreply, State#state{min_id=MinId}}
    end;
    
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

request_subscribe() ->
    {ok, ClientID} = application:get_env(client_id),
    {ok, ClientSecret} = application:get_env(client_secret), 
    {ok, Lat} = application:get_env(lat), 
    {ok, Lng} = application:get_env(lng), 
    {ok, CallbackURL} = application:get_env(callback_url), 
    Params = [
			  {"client_id", ClientID},
			  {"client_secret", ClientSecret},
			  {"object", "geography"},
			  {"aspect", "media"},
              {"lat", Lat},
              {"lng", Lng},
              {"radius", "5000"},
			  {"callback_url", CallbackURL}
			 ],
	{ok, _Code, _, ClientRef} = hackney:request(post, 
		"https://api.instagram.com/v1/subscriptions/",
        [], 
		{form, Params}
    ),
	{ok, Body} = hackney:body(ClientRef),
    DecodedBody = jiffy:decode(Body, [return_maps]),
    maps:get(<<"object_id">>, maps:get(<<"data">>, DecodedBody)). 
    % TODO store sub_id ans unsub later


handle_updated_objects(#state{geo_id=GeoId, min_id=MinId}) ->
    {ok, ClientID} = application:get_env(client_id),
    {ok, _Code, _, ClientRef} = hackney:request(get,
        hackney_url:make_url("https://api.instagram.com/v1/geographies/", [GeoId, <<"media">>, <<"recent">>],
                             [{<<"client_id">>, ClientID}, {<<"min_id">>, MinId}])),
    {ok, Body} = hackney:body(ClientRef),
    Data = jiffy:decode(Body, [return_maps]),
    DataList = maps:get(<<"data">>, Data),
    notify(DataList),
    case DataList of
        [] -> none;
        [H|_T] -> maps:get(<<"id">>, H)
    end.

notify([Obj|Other]) ->
    gproc_ps:publish(l, instaevent, Obj),
    notify(Other);

notify([]) ->
    ok.
    

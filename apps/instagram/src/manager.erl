-module(manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
        stop/0]).

-export([new_instaevent/1,
        subscribe/1]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {subs=maps:new()}).

-define(CONFIG_PARAM, fun(Key) -> {ok, Res} = application:get_env(Key), Res end).

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
    gproc_ps:tell_singles(l, instaev, Body).

subscribe(Tag) ->
    gen_server:cast(?MODULE, {subscribe, Tag}).

stop() ->
    gen_server:call(?MODULE, stop, 100000).
 
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
    %gen_server:cast(?MODULE, request_subscription),
    gproc_ps:create_single(l, instaev),
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

handle_call(stop, _From, S) ->
    {ok, _Code, _, _ClientRef} = hackney:request(delete,
        hackney_url:make_url("https://api.instagram.com/v1/subscriptions/", [],
                             [{<<"client_id">>, ?CONFIG_PARAM(client_id)},
                              {<<"client_secret">>, ?CONFIG_PARAM(client_secret)},
                              {<<"object">>, <<"all">>}])),
    {stop, normal, S};

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
handle_cast({subscribe, Tag}, S = #state{subs=Subs}) ->
    case maps:is_key(Tag, Subs) of
        true -> {noreply, S};
        false -> request_subscribe(Tag),
                 {noreply, S#state{subs=maps:put(Tag, "", Subs)}}
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
handle_info({gproc_ps_event, instaev, Body}, State) ->
    Data = jiffy:decode(Body, [return_maps]),
    case handle_updated_objects(Data, State) of
        none -> {noreply, State};
        NewState -> {noreply, NewState}
    end;
handle_info(Info, State) ->
    io:format("some info received~p~n", [Info]),
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

request_subscribe(Tag) ->
    ClientID = ?CONFIG_PARAM(client_id),
    ClientSecret = ?CONFIG_PARAM(client_secret),
    CallbackURL = ?CONFIG_PARAM(callback_url),
    Params = [
			  {"client_id", ClientID},
			  {"client_secret", ClientSecret},
			  {"object", "tag"},
              {"object_id", Tag},
			  {"aspect", "media"},
			  {"callback_url", CallbackURL}
			 ],
	{ok, 200, _, ClientRef} = hackney:request(post, 
		"https://api.instagram.com/v1/subscriptions/",
        [], 
		{form, Params}
    ),
	{ok, Body} = hackney:body(ClientRef).
    % TODO store sub_id ans unsub later


handle_updated_objects([Obj|Other], S=#state{subs=Subs}) ->
    ClientID = ?CONFIG_PARAM(client_id),
    ObjectId = maps:get(<<"object_id">>, Obj),
    MinId = maps:get(ObjectId, Subs, ""),
    {ok, _Code, _, ClientRef} = hackney:request(get,
        hackney_url:make_url("https://api.instagram.com/v1/tags/", [ObjectId, <<"media">>, <<"recent">>],
                             [{<<"client_id">>, ClientID}, {<<"min_id">>, MinId}])),
    {ok, Body} = hackney:body(ClientRef),
    ResponseData = jiffy:decode(Body, [return_maps]),
    DataList = maps:get(<<"data">>, ResponseData),
    notify(DataList, ObjectId),
    %TODO fix min_id
    case DataList of
        [] -> handle_updated_objects(Other, S);
        [H|_T] -> handle_updated_objects(Other, S#state{subs=maps:put(ObjectId, maps:get(<<"id">>, H), Subs)})
    end;

handle_updated_objects([], S) ->
    gproc_ps:enable_single(l, instaev),
    S.
 
notify([Obj|Other], Tag) ->
    gproc_ps:tell_singles(l, {tag, Tag}, Obj),
    notify(Other, Tag);

notify([], _) ->
    ok.
    

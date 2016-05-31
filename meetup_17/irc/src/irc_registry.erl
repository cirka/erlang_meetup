-module(irc_registry).

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

-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).

-record(state, {}).


%Registry stores tuples in format {Name, Pid, MonitorReference}
-define(TAB,irc_registry_table).

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


-spec register_name(Name :: term(), Pid :: erlang:pid()) -> yes | no.
register_name(Name,Pid) ->
    gen_server:call(?MODULE,{register,Name,Pid}).


-spec unregister_name(Name :: term() ) -> term().
unregister_name(Name) ->
    gen_server:call(?MODULE,{unregister,Name}).

-spec whereis_name(Name :: term()) -> erlang:pid() | undefined.
whereis_name(Name) ->
    case ets:lookup(?TAB,Name) of
        [{Name,Pid,_MonRef}] -> Pid;
        [] -> undefined
    end.

-spec send(Name :: term(), Msg :: term()) -> pid().
send(Name,Msg) ->
    case whereis_name(Name) of
        Pid when is_pid(Pid) ->
            Pid ! Msg,
            Pid;
        _ -> error(badarg,{Name,Msg})
    end.
        

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
    ?TAB = ets:new(?TAB,[{read_concurrency,true},named_table]),
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
handle_call({register,Name,Pid}, _From, State) ->
    case ets:match(?TAB,{Name,'$1','_'}) of
        [[RPid]] when is_pid(RPid) -> {reply,no,State};
        _Else -> 
            MonRef = monitor(process,Pid),
            true = ets:insert(?TAB,{Name,Pid,MonRef}),
            {reply, yes, State}
    end;

handle_call({unregister,Name}, _From, State) ->
    case ets:match(?TAB,{Name,'_','$1'}) of
        [[Ref]] when is_reference(Ref) ->
            true = demonitor(Ref,[flush]),
            true =  ets:delete(?TAB,Name),
            {reply,ok,State};
        _Else -> {reply,ok,State} 
    end;


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

handle_info({'DOWN',MonRef,_Type,_Pid,_info}, State) ->
    case ets:match(?TAB,{'$1','_',MonRef}) of
        [[Name]] ->
            true = demonitor(MonRef,[flush]),
            true =  ets:delete(?TAB,Name),
            {noreply,State};
        _Else -> {noreply,State} 
    end;

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

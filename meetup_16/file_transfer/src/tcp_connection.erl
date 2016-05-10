-module(tcp_connection).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket,lsocket}).

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
start_link(Socket) ->
    io:format("starting server with socket ~p~n",[Socket]),
    gen_server:start_link(?MODULE, [Socket], []).

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
init([LSocket]) ->
    {ok, #state{lsocket=LSocket},0}.

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
handle_info(timeout,#state{lsocket=LSocket}=State) ->
    {ok,Socket} = gen_tcp:accept(LSocket),
    {noreply,State#state{socket=Socket}};

handle_info({tcp,Socket,<<"PUT", FileSize:64, NameSize:8, FileName:NameSize/binary, Rest/binary>>},
              #state{socket=Socket} = State) ->
    {ok,LocalFile} = file:open("/tmp/"++ binary_to_list(FileName),[binary,write]),
    file:write(LocalFile,Rest),
    AlreadyReceived=size(Rest),
    case receive_rest(Socket,LocalFile,FileSize-AlreadyReceived) of
        ok -> gen_tcp:send(Socket,<<"OK">>);
        error -> gen_tcp:send(Socket,<<"ERROR">>)
    end,
    file:close(LocalFile),
    {stop,normal,State};

handle_info({tcp_closed,Socket}, #state{socket=Socket} = State) ->
    supervisor:start_child(tcp_pool,[]),
    {stop, normal,State};


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

receive_rest(_Socket,_LocalFile,0) -> ok;

receive_rest(Socket,LocalFile,ToGo) ->
    Size = min(ToGo,65535),
    case gen_tcp:recv(Socket,Size) of
        {ok,Data} ->
                Received = size(Data),
                file:write(LocalFile,Data),
                receive_rest(Socket,LocalFile,ToGo-Received);
        {error,_E} -> error
    end.


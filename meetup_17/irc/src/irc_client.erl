-module(irc_client).

-behaviour(gen_fsm).

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).
-compile(export_all).
-record(state, {sock,server,nick,user,real_name}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Nick,User,RealName) ->
    gen_fsm:start_link(?MODULE, [Nick,User,RealName], []).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Nick,User,RealName]) ->
    gen_fsm:send_event_after(0,connect),
    {ok, not_connected, #state{nick=Nick,user=User,real_name=RealName}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
not_connected(connect,State) ->
    ServerName = application:get_env(irc,server,"irc.data.lt"),
    ServerPort = application:get_env(irc,port,6667),
    case gen_tcp:connect(ServerName,ServerPort,[{active,true},{packet,line},binary]) of
        {ok,Sock} ->
            gen_fsm:send_event_after(0,present_yourself),
            {next_state,connected,State#state{sock=Sock}};
        _Else ->
            gen_fsm:send_event_after(5000,connect)
    end.

connected(present_yourself,#state{sock=Sock,nick=Nick,user=User,real_name=RealName} = State) ->
    send_nick(Sock,Nick),
    send_user(Sock, list_to_binary(User), unicode:characters_to_binary(RealName)),
    {next_state, wait_confirm, State}.

wait_confirm({msg_in,Binary},State) ->
    io:format("Got msg: ~p~n",[Binary]),
    {next_state,wait_confirm,State};
 
wait_confirm(_,State) ->
    {next_state,wait_confirm,State}.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

handle_info({tcp,Sock,<<"PING :",Token/binary>>}, StateName, #state{sock=Sock} = State) ->
    gen_tcp:send(Sock,<<"PONG :",Token/binary>>),
    {next_state, StateName, State};

handle_info({tcp,Sock,Data}, StateName, #state{sock=Sock} = State) ->
    Msg = decode(Data),
    gen_fsm:send_event(self(),{msg_in,Msg}),
    {next_state, StateName, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%:irc.data.lt 001 oposum :Welcome to the Aitvaras IRC Network oposum!~cirka@88.216.170.123

decode(Payload) ->
    case re:split(Payload,"^:(\\S+)\\s(.*)") of 
        [<<>>,Prefix,Rest,_Whitespaces] ->decode(Prefix,Rest);
        [Payload] -> decode(no_prefix,Payload)
    end.

decode(Prefix,Payload) ->
    case  re:split(Payload,"^(\\d{3})\\s(.*)") of
        [<<>>,<<":",Command/binary>>,Params,<<>>] ->
                {in_msg,{Prefix,binary_to_integer(Command),Params}};
        Else ->decode_simple(Prefix,Else)
    end.
    
decode_simple(Prefix, Command) -> 
     {in_msg,{Prefix,Command}}.

send_nick(Sock,Nick) ->
    Msg = ["NICK ", Nick, "\r\n"],
    gen_tcp:send(Sock,Msg).

send_user(Sock, User, RealName) ->
    Msg = [ "USER ", User," 0 * ", RealName, "\r\n"],
    gen_tcp:send(Sock,Msg).



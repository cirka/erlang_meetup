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
-record(state, {sock,server,nick,user,real_name,event_server,rules}).

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

get_epid(PID) ->
    gen_fsm:sync_send_all_state_event(PID,get_epid).

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
    {ok,EPid} = irc_event_server:start_link(),
    gen_fsm:send_event_after(0,connect),
    {ok, not_connected, #state{nick=Nick,user=User,real_name=RealName,rules=[],event_server=EPid}}.

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
            gen_fsm:send_event_after(5000,connect),
            {next_state,not_connected,State}
    end.

connected(present_yourself,#state{nick=Nick,user=User,event_server=EPid, real_name=RealName} = State) ->
    IrcSession = self(),
    
    Worker = 
    fun() ->
        WorkerPid = self(),
            Handler =
            fun
                ({in_msg,_,<<"001">>,_},State1) ->
                     WorkerPid ! connected,State1;
                ( _, State1) -> State1
            end,
        irc_event_server:add_handler(EPid,Handler,0),
        receive 
            connected -> gen_fsm:send_event(IrcSession,login_success)
        after 5000 -> ok
        end
    end,
    spawn(Worker),
    send_msg(self(),["NICK ", Nick, "\r\n"]),
    send_msg(self(),["USER ", User," 0 * :", unicode:characters_to_binary(RealName), "\r\n"]),
    {next_state, wait_confirm, State}.

wait_confirm(login_success,State) ->
    {next_state,logged_in,State};

wait_confirm(_,State) ->
    {next_state,wait_confirm,State}.
    
logged_in(_Event,State) ->
    {next_state,logged_in,State}.
 
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


handle_event({send_msg,Msg}, StateName, #state{sock=Sock} = State) ->
    gen_tcp:send(Sock,Msg),
    {next_state, StateName, State};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%--------------------------------------------------------------------
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
handle_sync_event(get_epid, _From, StateName, #state{event_server = Epid} = State) ->
    {reply, {ok,Epid}, StateName, State};

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

handle_info({tcp_close,Sock},_StateName,#state{sock=Sock} = State) ->
    gen_fsm:send_event_after(0,connect),
    {next_state,not_connected,State};

handle_info({tcp,Sock,Data}, StateName, #state{sock=Sock,event_server=Epid} = State) ->
    Msg = decode(Data),
    
    gen_event:notify(Epid,Msg),
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

decode(Bin) when is_binary(Bin) ->
    decode([],Bin).


%% take prefix
decode(Tokens,<<":",Rest/binary>>) ->
    [Prefix,Rest1] = re:split(Rest," ", [{parts,2}]),
    decode(Tokens++[{sender,decode_prefix(Prefix)}],Rest1);

%% add prefix token when command is send without prefix
decode(Tokens,Bin) when Tokens == [] ->
    decode([{sender,peer}],Bin);

%% take command
decode([{sender,_}] = Tokens,Bin) ->
    [<<>>,Command,Params,_] = re:split(Bin,"(*CRLF)^([[:alpha:]]+|\\d{3})(.*)"),
    decode(Tokens ++ [{command,Command}],Params);

%% take trailing
decode([_,_|_Params] = Tokens,<<" :",Trailing/binary>>) ->
   decode(Tokens ++ [strip_crlf(Trailing)],<<>>);

%% take middle  
decode([_,_|_Params] = Tokens,<<" ",BinRest/binary>>) ->
    case re:split(BinRest," ", [{parts,2}]) of
        [Middle,Rest1] ->
            decode(Tokens ++ [strip_crlf(Middle)], <<" ",Rest1/binary>>);
        [Middle] ->
            decode(Tokens ++ [Middle],<<>>)
    end;

%% return results
decode([{sender,Sender},{command,Command}|Params],<<>>) ->
    {in_msg,Sender,Command,Params}.

decode_prefix(Prefix) -> Prefix.

strip_crlf(Bin) ->
    [<<>>,Result|_Rest] = re:split(Bin,"(*CRLF)(.*)"),
    Result.

dispatch(_Msg,_Rules) -> ok.

send_msg(Pid,Msg) ->
    gen_fsm:send_all_state_event(Pid,{send_msg,Msg}).



%list_channels(Pid) ->
%    
%   Handler =  fun({in_msg,_,_,<<"322">>,Name,Count,Topic},State) ->
%        [{Name,Count,Topic} |State];
%        (_,State) -> State;
%    end,
%    
%

-module(slot_machine).

-behaviour(gen_fsm).

%% API
-export([start_link/0,
         insert_coin/0,
         insert_trash/0,
         eject/0,
         total/0,
         served/0,
         magic/0]).

%% gen_fsm callbacks
-export([init/1,
         idle/2,
         have1coin/2,
         have2coins/2,
         idle/3,
         have1coin/3,
         have2coins/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(state, {coins,drinks_served}).

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
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).



insert_coin() -> gen_fsm:send_event(?MODULE,insert_coin).

insert_trash() -> gen_fsm:send_event(?MODULE,insert_trash).

eject() -> gen_fsm:send_all_state_event(?MODULE,cancel).

total() -> gen_fsm:send_all_state_event(?MODULE,total).

served() -> gen_fsm:sync_send_all_state_event(?MODULE,served).

magic() -> gen_fsm:sync_send_event(?MODULE,magic).


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
init([]) ->
    io:format("It takes only three coins to make me happy, and I serve drinks then~n"),
    {ok, idle, #state{coins=0,drinks_served=0}}.

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
idle(insert_coin,State) ->
    io:format("Nice coin, two more to go~n"),
    {next_state,have1coin,State#state{coins=1}};

idle(_Else,State) ->
    io:format("Do not mess with the machine!~n"),
    {next_state,idle,State}.

have1coin(insert_coin,State) ->
     io:format("Mhmmm, I have two coins,  give me one more~n"),
    {next_state,have2coins,State#state{coins=2}};

have1coin(_Else,State) ->
    io:format("Do not mess with the machine !~n"),
    {next_state,have1coin,State}.

have2coins(insert_coin,State=#state{drinks_served=Served}) ->
    io:format("I have three coins and I am happy for you, here is your drink~n"),
    {next_state,idle,State#state{coins=0,drinks_served=Served+1}};

have2coins(_Else,State) ->
    io:format("Heyyy ! Do not mess with the machine, I have your coins !"),
    {next_state,have2coins,State}.

%%%--------------------------------------------------------------------
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

% Čia galėtų būti sinchroniniai state callbackai
idle(_Event,_From,State) ->
    {reply,ok,idle,State}.

have1coin(_Event,_From,State) ->
    {reply,ok,have1coin,State}.

have2coins(magic,_From,State) ->
    io:format("Here is your magic pony!!!!~n"),
    {reply,pony,have2coins,State}.

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
handle_event(cancel, idle, State) ->
    io:format("No free lunch..heh~n"),
    {next_state, idle, State};

handle_event(cancel, _Other_states, State=#state{coins=Coins}) ->
    io:format("Dzing dzing take your ~p coin(s)~n",[Coins]),
    {next_state, idle, State#state{coins=0}};

handle_event(total,Any_state,State=#state{coins=Coins}) ->
    io:format("I have ~p coin(s)~n",[Coins]),
    {next_state,Any_state,State};

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


handle_sync_event(served, _From, StateName, State=#state{drinks_served=Served}) ->
    
    Reply = Served ,
    {reply, Reply, StateName, State};

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

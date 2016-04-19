-module(ets_server_users).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-export([
    give_me_table/0,
    take_table_back/0,
    add_user/2,
    check_user/2,
    list_members/0,
    list_members2/0
    ]).


-record(state, {
        tab,
        owner,
        pid
        }).

-record(user,{username,password,member}).

-define(TAB,ets_server_users_table).


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

give_me_table() ->
   My_pid = self(),
   case gen_server:call(?MODULE,{give_me_table,My_pid}) of
        {ok,TAB} ->
            receive 
                {'ETS-TRANSFER',TAB,FromPid,GiftData } ->
                    io:format("table ~p was given by ~p with msg:~p~n",[TAB,FromPid,GiftData]),
                    {ok,TAB}
            after 1000 -> timeout
            end;
        {error,Reason} -> {error,Reason}
    end.

take_table_back() ->
    case gen_server:call(?MODULE,{is_your_table,?TAB}) of
        true -> ets:give_away(?TAB,whereis(?MODULE),finished_using),
        ok;
        false -> {error,wrong_server}
    end.


add_user(Username,Password) ->
    case is_owner(?TAB,self()) of 
        true -> add_user_local(Username,Password);
        false -> {error,you_must_own_table}
    end.

add_user_local(Username,Password) ->
        case ets:insert_new(?TAB,#user{username=Username,password=Password,member=true}) of
            true -> ok;
            false -> {error,not_unique}
        end.

check_user(Username,Password) ->
    gen_server:call(?MODULE,{check_user,Username,Password}).


list_members() ->
%MatchSpec = [MatchFunction]
%MatchFunction = {MatchHead, [Guard], [Result]}
%MatchHead = "Pattern as in ets:match"
%Guard = {"Guardtest name", ...}
%Result = "Term construct"
    Match_head = {user,'$1','$2','$3'}, % arba #user{username=$1,password=$2,member=$3}
    Guard = { '==',true,'$3'},
    Result = ['$1'],
    Match_function = {Match_head,[Guard],Result},
    Match_spec = [Match_function],
% arba [{{user,'$1','$2','$3'},[{'==',true,'$3'}],['$1'] } ]
    ets:select(?TAB,Match_spec).

list_members2() ->
    ets:select(?TAB,ets:fun2ms(
    fun(#user{username=User,password='_',member=true}) -> User end)
    ).


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
    ?TAB = ets:new(?TAB,[set,{keypos,#user.username},named_table]),
    true = ets:setopts(?TAB,{heir,self(),client_died}),
    {ok, #state{tab=?TAB,owner=self(),pid=self()}}.

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
handle_call({give_me_table,PID}, _From, State = #state{tab=TAB,owner=SPID,pid=SPID}) ->
    case ets:give_away(?TAB,PID,hi_client) of
        true -> {reply,{ok,TAB},State#state{owner=PID}};
        false ->{reply,{error,give_away_failed},State}
    end;

handle_call({give_me_table,_PID}, _From, State) ->
    {reply,{error,do_not_own_it_now},State};

handle_call({is_your_table,TAB}, _From, State=#state{tab=TAB}) -> {reply,true,State};
handle_call({is_your_table,_TAB}, _From, State) -> {reply,false,State};

    
handle_call({check_user,Username,Password}, _From, State=#state{tab=TAB}) ->
    case ets:match(TAB,#user{username = Username,password= Password,member='$1'}) of
        [[true]] -> {reply,member,State};
        [[false]] -> {reply,not_anymore,State};
        [] -> {reply,unknown_user,State}
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

handle_info({'ETS-TRANSFER',TAB,FromPid,finished_using} , State) ->
   io:format("User ~p returned table ~p ~n",[FromPid,TAB]), 
    {noreply, State#state{owner=self()}};

handle_info({'ETS-TRANSFER',TAB,FromPid,client_died} , State) ->
   io:format("User ~p died and table ~p was inherted by me n",[TAB,FromPid]), 
    {noreply, State#state{owner=self()}};

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


is_owner(TAB,PID) ->
    case ets:info(TAB,owner) of
        PID -> true;
        _Else -> false
    end.



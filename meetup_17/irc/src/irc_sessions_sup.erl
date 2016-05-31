-module(irc_sessions_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_session/5,
         stop_session/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_session(Session, Servername, Nick, User, Realname) ->
    supervisor:start_child(?MODULE,[Session, Servername, Nick, User, Realname]).

stop_session(Session) ->
    case irc_session_sup:whereis(Session) of
        Pid when is_pid(Pid) -> supervisor:terminate_child(?MODULE,Pid);
        _Else -> not_found
    end.

    

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(session, irc_session_sup, supervisor, [])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-module(irc_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/5,
         whereis/1]).

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
start_link(Session,ServerName, Nick,User,RealName) ->
    supervisor:start_link({via, irc_registry,{Session,sup}}, ?MODULE, [Session,ServerName, Nick,User,RealName]).

whereis(Session) ->
    irc_registry:whereis_name({Session,sup}).

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
init([Session|_Rest] = Args) ->
    IrcClient = {connection, {irc_client, start_link, Args}, permanent, 5000, worker, [irc_client]},
    IrcEventServer = {event_server,{irc_event_server,start_link,[Session]}, permanent,5000,worker,[irc_event_server]},
    {ok, {{rest_for_one, 5, 10}, [IrcEventServer,IrcClient]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

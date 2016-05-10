-module(tcp_pool).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     temporary, 5000, Type, [Mod]}).

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
    {ok,Socket} = gen_tcp:listen(5555,[binary,{active,true},{packet,line}]),
    start_listeners(self()),
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(tcp_connection, tcp_connection, worker, [Socket])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


start_listeners(Pid) ->
    Fun =  fun() ->
        io:format("starting children~n"),
        [supervisor:start_child(Pid,[]) || _X <- lists:seq(1,10)]
    end,
    spawn(Fun).

            

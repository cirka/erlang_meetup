-module(test_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

Server1 = #{id => server1,             % id mandatory
                 start => {opt_server1,start_link,[]}, % {M,F,A}  mandatory
                 restart => permanent,  % optional: permanent,temporary, transient, defaults to permanent
                 shutdown => 5000,      % optional: brutal_kill, integer(), defaults to 5000 if type==worker else infinity
                 type => worker,       % optional: worker, supervisor, defaults to worker 
                 modules => [opt_server1]}, % optional: list of modules for hot code upgrade 

    {ok, { {one_for_one, 5, 10}, [Server1]} }.


-module(testing1_server_tests).
-include_lib("eunit/include/eunit.hrl").

-export([
         start_server/0,
         stop_server/1,
         two_servers/1,
         first_round/1,
         second_round/1
        ]).


%%  Single function  some_name_test()
blackbox_server_test() ->
    {ok,PID}  = testing1_server:start_link(),
    ?assert(is_pid(PID)),
    ?assertEqual(ok,testing1_server:set_property_a(10)),
    ?assertEqual(10,testing1_server:get_property_a()),
    ?assertEqual(20,testing1_server:get_calculated_a()),
    ?assertEqual(ok,testing1_server:set_property_a(20)),
    ?assertEqual(20,testing1_server:get_property_a()),
    ?assertEqual(ok,testing1_server:stop(PID)),
    ?assertEqual(false,is_process_alive(PID)).



whitebox_server_init_test() ->
      {ok,State} =  testing1_server:init([]),
     ?assertEqual({state,undefined},State).

whitebox_server_set_property_a_test() ->
     ?assertEqual( {reply,ok,{state,10}}, testing1_server:handle_call({set_a,10},from,{state,undefined})).

whitebox_server_get_property_a_test() ->
    
     ?assertEqual( {reply,100,{state,100}},
                   testing1_server:handle_call(get_a,from,{state,100})).


%% test generators some_name_test_() -> [test()] where test :: {Name, fun/0} | fun/0
whitebox_server_generator_test_() ->
    [{"Server init returns expected state",
       fun whitebox_server_init_test/0 },
     {"Server correctly stored modified property",
       fun whitebox_server_set_property_a_test/0 },
     {"Server returns correct stored property value",
       fun whitebox_server_get_property_a_test/0 },
     ?_assertEqual(1,1+0)].


%% fixtures single run 
%{setup, Setup, Instantiator}
%{setup, Setup, Cleanup, Instantiator}
%{setup, Where, Setup, Instantiator} Where :: local | spawn | {spawn,Node} 
%{setup, Where, Setup, Cleanup, Instantiator}

%% add {"name",{setup...}} if you want to name fixtured generator


blackbox_fixtured_test_() ->
    {setup,fun start_server/0, fun stop_server/1, fun first_round/1 }.

start_server() ->
    {ok,Pid} = testing1_server:start_link(),
    Pid.
 
stop_server(Pid) -> testing1_server:stop(Pid).

two_servers(Pid) ->
    ?_assertMatch({error,{already_started,Pid}},testing1_server:start_link()).


first_round(Pid) ->
    [{"It should be impossible to start additional server instances", two_servers(Pid)},
     {"Setting server property should succeed",?_assertEqual(ok, testing1_server:set_property_a(10))},
     {"Getting server property should succeed",?_assertEqual(10, testing1_server:get_property_a())},
     {"Getting server calculated property should succeed",?_assertEqual(20,testing1_server:get_calculated_a())}
    ].
 

second_round(_Pid) ->
    [
     {"Setting server property should succeed",?_assertEqual(ok, testing1_server:set_property_a(20))},
     {"Getting server property should succeed",?_assertEqual(20, testing1_server:get_property_a())},
     {"Getting server calculated property should succeed",?_assertEqual(40,testing1_server:get_calculated_a())}
    ].
 
%% Fixtures multirun
%{foreach, Where, Setup, Cleanup, [Instantiator]}
%{foreach, Setup, Cleanup, [Instantiator]}
%{foreach, Where, Setup, [Instantiator]}
%{foreach, Setup, [Instantiator]}

blackbox_fixtured_two_rounds_test_() ->
    {"two rounds test",
     {foreach, fun start_server/0,  fun stop_server/1, [fun first_round/1, fun second_round/1]}
    }.




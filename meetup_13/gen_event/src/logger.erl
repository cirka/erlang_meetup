-module(logger).
-compile(export_all).
-define(SERV,logger_server).


start() -> application:start(logger).

log(Event) -> gen_event:notify(?SERV,Event).

log(Format,Vars) ->
   Event =  lists:flatten(io_lib:format(Format,Vars)),
   log(Event).

add_console_handler(Id) ->
    gen_event:add_handler(?SERV,{logger_console,Id}, []).

add_file_handler(Id,Filename) ->
    gen_event:add_handler(?SERV,{logger_disklog,Id}, [Filename]).



mute(Id) ->
    case lists:member(Id,gen_event:which_handlers(?SERV)) of
        true -> gen_event:call(?SERV,Id,mute);
        false -> not_running
    end.



activate(Id) ->
    case lists:member(logger_console,gen_event:which_handlers(?SERV)) of
        true -> gen_event:call(?SERV,Id,activate);
        false -> not_running
    end.






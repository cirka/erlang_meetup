-module(server).
-export([start/0,loop/1,topic_loop/1]).




start() ->
    PID = spawn(server,loop,[[]]),
    register(server,PID).

loop(Topic_list) ->
    receive
    {publish,Topic,Msg} ->
       case proplists:get_value(Topic, Topic_list,'$notopic') of
        '$notopic' -> loop(Topic_list);
         TPid -> TPid ! {publish,Msg},
                 loop(Topic_list)
       end;
    {subscribe, Topic,Pid} -> 
       case proplists:get_value(Topic, Topic_list,'$notopic') of
        '$notopic' -> TPid = spawn(server,topic_loop,[{Topic,[]}]),
                      TPid ! {subscribe, Pid},
                      loop([{Topic,TPid}|Topic_list]);
         TPid -> TPid ! {subscribe,Pid},
                 loop(Topic_list)
       end;
    {list_subs,Topic} ->
        io:format("gavau lists_subs"),
       case proplists:get_value(Topic, Topic_list,'$notopic') of                 
        '$notopic' -> loop(Topic_list);                                          
         TPid -> TPid ! {list_subs},                                           
                 loop(Topic_list)                                                
       end; 
     _ -> loop(Topic_list)
    end.    


topic_loop({Topic,Subs}) ->
    receive 
        {subscribe,Pid} ->
            case proplists:get_value(Pid,Subs,'$nopid') of
            '$nopid' ->
                Pid ! {subscribe_success,Topic},
                erlang:monitor(process,Pid), 
                topic_loop({Topic,[Pid|Subs]});
            Pid -> Pid ! {subscribe_success,Topic},
                topic_loop({Topic,Subs})
            end;
        {publish,Msg} ->
            lists:foreach( 
                fun (Pid) -> Pid ! {message,Topic,Msg} end,
                Subs),
            topic_loop({Topic,Subs});
        {'DOWN', _Ref, _Type, Pid, _Exit_status} ->
                case Subs of 
                    [] -> ok;
                    _Else -> topic_loop({Topic,lists:delete(Pid,Subs)})
                end;
        {list_subs} -> io:format("Subs ~p~n",[Subs]),
                    topic_loop({Topic,Subs});
        _ -> topic_loop({Topic,Subs})
    end.

    

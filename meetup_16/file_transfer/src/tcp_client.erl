-module(tcp_client).

-export([upload/3]).

upload(Server, Port,FileName) ->
    {ok,{hostent,"localhost",[],inet,4,[Host]}} = inet:gethostbyname(Server),
    {ok,Socket} = gen_tcp:connect(Host,Port,[binary,{active,false}]),
    case filelib:is_file(FileName) of
        true ->
            FileSize = filelib:file_size(FileName),
            NameSize = length(filename:basename(FileName)),
            gen_tcp:send(Socket,<<"PUT", FileSize:64, NameSize:8, (list_to_binary(filename:basename(FileName)))/binary
   >>),
            case file:sendfile(FileName,Socket) of
                {ok,ByteSend} -> ok;
                {error,Some} -> failed
            end;
        false -> no_file
    end.


    


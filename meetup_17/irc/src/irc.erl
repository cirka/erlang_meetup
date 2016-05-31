-module(irc).

-compile(export_all).


-spec start_session( SessionName :: term(),
                    Server :: list(),
                    Nick :: list(),
                    Username :: list(),
                    RealName :: unicode:unicode_binary()) ->
    ok | {error, Reason :: term}.

start_session(SessionName, Server, Nick, Username, RealName) ->
    irc_sessions_sup:start_session(SessionName, Server, Nick, Username, RealName).

-spec stop_session(SessionName :: term()) -> ok.
stop_session(SessionName) ->
    irc_clients_sup:stop_client(SessionName).

-spec send_msg(Session :: term(), Msg :: term()) -> ok | {error, Reason :: term()}.
send_msg(Session,Msg) ->
    irc_client:send_msg(Session,Msg).

add_handler(Session,Handler,InitialState) ->
    irc_event_server:add_handler(Session,Handler,InitialState).





    

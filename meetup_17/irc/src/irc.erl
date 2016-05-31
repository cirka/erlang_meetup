-module(irc).

-compile(export_all).


-spec start_client( SessionName :: term(),
                    Server :: list(),
                    Nick :: list(),
                    Username :: list(),
                    RealName :: unicode:unicode_binary()) ->
    ok | {error, Reason :: term}.

start_session(SessionName, Server, Nick, Username, RealName) ->
    irc_sessions_sup:start_client(SessionName, Server, Nick, Username, RealName).

-spec stop_Session(SessionName :: term()) -> ok.
stop_session(SessionName) ->
    irc_clients_sup:stop_client(SessionName).

-spec send_msg(Session :: term(), Msg :: term()) -> ok | {error, Reason :: term()}.
send_msg(Session,Msg) ->
    gen_fsm:send_all_state_event({via,irc_registry,{Session,session}},{send_msg,Msg}).



    

-module(irc).

-compile(export_all).


-spec start_session(SessionName :: term(),
                    Server :: list(),
                    Nick :: list(),
                    Username :: list(),
                    RealName :: unicode:unicode_binary()) ->
    ok | {error, Reason :: term}.

start_session(SessionName, Server, Nick, Username, RealName) ->
    irc_sessions_sup:start_session(SessionName, Server, Nick, Username, RealName).

-spec stop_session(SessionName :: term()) -> ok.
stop_session(SessionName) ->
    irc_sessions_sup:stop_session(SessionName).

-spec send_msg(Session :: term(), Msg :: term()) -> ok | {error, Reason :: term()}.
send_msg(Session,Msg) ->
    irc_client:send_msg(Session,Msg).

-spec add_handler(Session :: term(),
                  Handler :: fun((Event :: term, State :: term()) -> {ok,NewState :: term()}),
                  InitialState :: term()) ->
     NewState :: term().

add_handler(Session,Handler,InitialState) ->
    irc_event_server:add_handler(Session,Handler,InitialState).





    

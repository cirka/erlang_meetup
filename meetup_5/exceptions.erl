-module(test).
-compile(export_all).

start() ->
 FUN = fun A () ->
       throw(lala),
       receive 
         quit -> ok
         after 1000 -> A()
       end
      end,
 erlang:spawn(FUN).



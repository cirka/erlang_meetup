-module(some2).

-compile(export_all).


incrementer(A) when is_list(A) ->
 [X+1 || X <-A].
 
decrementer(A) when is_list(A) ->
 [X-1 || X <-A].


inc(X) -> X+1.
dec(X) -> X-1.

iterator(Fun, A,B,C) when is_list(A) ->

{[Fun(X) || X <- A],B+C}.

incrementer2(A) ->
    iterator(fun ?MODULE:inc/1,A,1,2).
decrementer2(A) ->
    iterator(fun dec/1,A,2,3).
multiplyer(A) -> iterator( fun(X) when X rem 2 ==0 -> X*2 ;(X) -> X*3 end,A,4,5).










 

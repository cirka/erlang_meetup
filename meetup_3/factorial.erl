-module(recursive).
-export([fac/1]).
-export([fac2/1]).

 
fac(N) when N == 0 ->
     1;
fac(N) when N > 0  ->
     N*fac(N-1).

fac2(N) -> fac2(N,1).

fac2(0,Acc) -> Acc;
fac2(N,Acc) -> fac2(N-1,Acc*N).



 

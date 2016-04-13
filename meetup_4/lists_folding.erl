-module(some).
-export([some1/0,some1/1]).

-import(lists,[foldl/3]).

-compile(export_all).

some2 () -> some1().


some1() -> foldl( fun(El,Acc) -> ok end, 0,lists:seq(1,2)).

some1(Some) -> nook.


funk1({funky,Name}) -> Name;


funk1({disco,Name1}) -> Name1;

funk1([H|Tail]) -> H.


funk2(Some) when is_list(Some)  ->
    Args1 = case Some of 
        "sdfome" -> some1;
        "sdf" -> kita_dalyka
    end,


    [Args1|"ssdfsdf"]
.

 






-module('8unused_term').
-compile(export_all).


some() ->
    _A = fun () -> ok end,
    ok.


-module('11unsafe_in_case').
-compile(export_all).

some(A) ->
    case A of
        true -> B = 2;
        false -> B = 3;
        _Else -> ok 
    end,
    B.



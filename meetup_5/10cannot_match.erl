-module('10cannot_match').
-compile(export_all).


some(A) ->
    case A of
        Some -> Some;
        atom -> false
    end.


-module('9head_mismatch').
-compile(export_all).


some(A) -> A;

some(A,B) -> A + B.


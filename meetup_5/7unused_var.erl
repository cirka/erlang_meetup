-module('7unused_var').
-compile(export_all).


some(Var1) -> ok.

some2() -> Var2=some, ok.


-module(mod2).

-compile(export_all).

-import(hello, [start/0]).

run() ->
  start(),
  ok.

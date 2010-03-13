-module(ex1).
-export([run/0,increment_each/1]).

run() ->
  Pid = spawn(fun ping/0),
  io:format("run: spawned ping/0(~p), self=~p~n", [Pid,self()]),
  Pid ! self(),
  receive
    pong -> 
      io:format("run: received pong~n"),
      ok
  end.

ping() ->
  io:format("ping: started~n"),
  receive
    From -> 
      io:format("ping: received, resopnding From:~p~n", [From]),
      From ! pong
  end.

increment_each([Num|Tail]) ->
  [ Num+1 | increment_each(Tail) ];
increment_each([]) ->
  [].

-module(yet_again).
-export([fibonacci/1, factorial/1, fib2/1]).

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N-1).

fibonacci(0) ->
    1;
fibonacci(1) ->
    1;
fibonacci(N) ->
    fibonacci(N-1) + fibonacci(N-2).

fib2(0,F1,_F2) ->
    F1;
fib2(N,F1,F2) -> 
    fib2(N-1,F2,F1+F2).

fib2(N)->
    fib2(N,1,1).
    









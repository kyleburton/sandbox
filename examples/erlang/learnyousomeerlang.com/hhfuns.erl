-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

% NB: hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X+1.
decr(X) -> X-1.

% L = [1,2,3,4,5].
% hhfuns:increment(L).
% hhfuns:decrement(L).
% hhfuns:map(fun hhfuns:incr/1, L).
% hhfuns:map(fun hhfuns:decr/1, L).
% hhfuns:map(fun(X) -> X * 2 end, L).
% hhfuns:map(fun (X) -> math:pow(X,X) end, lists:seq(1,20)).

square(X) -> X*X.

% hhfuns:map(fun hhfuns:square/1, lists:seq(1,20)). 
% NB: square looks to return an int, while math:pow results in a float! (look like some kind of bigdecimal)




% NB closures can be named:
% PrepareAlarm = fun(Room) ->
%    io:format("Alarm set in ~s.~n",[Room]),
%     fun Loop() ->
%        io:format("Alarm tripped in ~s! Call Batman!~n",[Room]),
%        timer:sleep(500),
%         Loop()
%     end
% end.


filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).

filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
  case Pred(H) of
    true  -> filter(Pred, T, [H|Acc]);
    false -> filter(Pred, T, Acc)
  end.

% hhfuns:filter(fun(X) -> X rem 2 =:= 0 end, lists:seq(1,100)).
% People = [{male,45},{female,67},{male,66},{female,12},{unknown,174}, {male,74}].
% hhfuns:filter(fun({Gender,Age}) -> Gender == male andalso Age > 60 end, People).

% fold (left) aka reduce
fold(_, []) -> none;
fold(F, [H|T]) -> fold(F, H, T).

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).

% max:
% hhfuns:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T).
% 
% min:
% hhfuns:fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T).
%
% sum:
% hhfuns:fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)).
%
% after adding fold/2
% hhfuns:fold(fun(Elt, Acc) -> Acc + Elt end, lists:seq(1,10)).
% hhfuns:fold(fun(Elt, Acc) -> Acc + Elt end, 0, lists:seq(1,10)).
%

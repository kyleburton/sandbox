-module(llist).
-export([append/1, append/2, reverse/1, array_to_list/1]).

% linked list, using immutable 2-tuples

array_to_list([H]) ->
  {H, none};
array_to_list([H|T]) ->
  {H, array_to_list(T)}.

append(X) ->
  {X, none}.

append({Y, none}, X) ->
  {Y, {X, none}};

append({Y, Rest}, X) ->
  {Y, append(Rest, X)}.


reverse({X, none}) ->
  {X, none};
reverse({Y, Rest}) ->
  append(reverse(Rest), Y).

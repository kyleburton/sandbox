-module(what_the_if).
-export([heh_fine/0]).


heh_fine() ->
  if 1 =:= 1 ->
       works
  end,
  if 1 =:= 2; 1 =:= 1 ->
       works
  % end,
  % if 1 =:= 2, 1 =:= 1 ->
  %      fails
  end.

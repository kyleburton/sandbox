-module(heap_tests).

-compile(export_all).

-include("heap.hrl").

-include_lib("eunit/include/eunit.hrl").

bst_array_test_() ->
  {foreach,
   fun setup/0,
   fun teardown/1,
   []}.

setup() ->
  <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> = crypto:strong_rand_bytes(12),
  rand:seed(exsplus, {I1, I2, I3}),
  ok.

teardown(_) ->
  ok.


max_int_cmp(A, B) when A > B ->
  -1;
max_int_cmp(A, B) when A == B ->
  0;
max_int_cmp(A, B) when A < B ->
  1.

min_int_cmp(A, B) when A > B ->
  -1;
min_int_cmp(A, B) when A == B ->
  0;
min_int_cmp(A, B) when A < B ->
  1.

random_heap(CmpFn, Elts) ->
  Elts2 = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- Elts])],
  H = heap:new(CmpFn),
  lists:foldl(fun (E, H0) -> heap:insert(E, H0) end, H, Elts2).

length_test() ->
  H = heap:new(fun max_int_cmp/2),
  ?assertEqual(0, heap:length(H)),
  H2 = heap:insert(33, H),
  ?assertEqual(1, heap:length(H2)).
  %% ?debugFmt("H2=~p", [H2]).

reduce_test() ->
  H = random_heap(fun max_int_cmp/2, lists:seq(1, 15)),
  Sum = heap:reduce(
          fun (Val, Acc) ->
              ?debugFmt("reduce_test#fn: Val=~p", [Val]),
              Val + Acc
          end,
          0,
          H),
  ?assertEqual(Sum, 120).

print_test() ->
  H0 = random_heap(fun max_int_cmp/2, []),
  P0 = heap:position_pairs(H0),
  ?debugFmt("H0=~p; P0=~p", [array:to_list(H0#heap.buff), P0]),

  H1 = random_heap(fun max_int_cmp/2, [1]),
  P1 = heap:position_pairs(H1),
  ?debugFmt("H1=~p; P1=~p", [array:to_list(H1#heap.buff), P1]),

  H2 = random_heap(fun max_int_cmp/2, [1, 2]),
  P2 = heap:position_pairs(H2),
  ?debugFmt("H2=~p; P2=~p", [array:to_list(H2#heap.buff), P2]),

  H3 = random_heap(fun max_int_cmp/2, [1, 2, 3]),
  P3 = heap:position_pairs(H3),
  ?debugFmt("H3=~p; P3=~p", [array:to_list(H3#heap.buff), P3]),
  
  H4 = random_heap(fun max_int_cmp/2, lists:seq(1, 15)),
  P4 = heap:position_pairs(H4),
  ?debugFmt("H4=~p; P4=~p", [array:to_list(H4#heap.buff), P4]),
  
  ok.


dotty_test() ->
  % H0 = random_heap(fun max_int_cmp/2, lists:seq(1, 13)),
  % Data = heap:to_dotty(H0),
  % file:write_file("dotty_test.dot", Data).
  lists:foreach(fun gen_dotty/1, lists:seq(1, 15)).

gen_dotty(N) ->
  H0 = random_heap(fun max_int_cmp/2, lists:seq(1, N)),
  Data = heap:to_dotty(H0),
  Fname = io_lib:format("../dotty_test-~2..0B.dot", [N]),
  ?debugFmt("Fname=~s", [Fname]),
  file:write_file(Fname, Data).

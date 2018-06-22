-module(bst_array_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


bst_array_test_() ->
  {foreach,
   fun setup/0,
   fun teardown/1,
   [fun length_test/0]}.


setup() ->
  <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> = crypto:strong_rand_bytes(12),
  rand:seed(exsplus, {I1, I2, I3}),
  ok.

teardown(_) ->
  ok.

int_cmp(A, B) when A > B ->
  -1;
int_cmp(A, B) when A == B ->
  0;
int_cmp(A, B) when A < B ->
  1.

random_tree(CmpFn, Elts) ->
  Elts2 = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- Elts])],
  %% ?debugFmt("Elts2=~p; Elts=~p", [Elts2, Elts]),
  T = bst_array:new(CmpFn),
  lists:foldl(fun (E, T0) -> bst_array:add(E, T0) end, T, Elts2).

length_test() ->
  T = bst_array:new(fun int_cmp/2),
  ?assertEqual(0, bst_array:length(T)),
  T2 = bst_array:add(99, T),
  ?assertEqual(1, bst_array:length(T2)),
  T3 = random_tree(fun int_cmp/2, lists:seq(1, 10)),
  ?assertEqual(10, bst_array:length(T3)).

pre_order_traverse_test() ->
  Nums = lists:seq(1,10),
  T = random_tree(fun int_cmp/2, Nums),
  %% ?debugFmt("T=~p", [T]),
  Nums2 = bst_array:pre_order_traverse(
            T,
            fun (N, Acc) -> [N | Acc] end,
            []),
  ?assertEqual(Nums, Nums2).

post_order_traverse_test() ->
    Nums = lists:seq(1,10),
    T = random_tree(fun int_cmp/2, Nums),
    Nums2 = bst_array:post_order_traverse(
              T,
              fun (N, Acc) -> [N | Acc] end,
              []),
    ?assertEqual(lists:reverse(Nums), Nums2).

in_order_traverse_test() ->
    Nums = lists:seq(1,10),
    T = random_tree(fun int_cmp/2, Nums),
    Nums2 = bst_array:in_order_traverse(
              T,
              fun (N, Acc) -> [N | Acc] end,
              []),
    %% can we assert an ordering here if the tree is randomly constructed?
    ?assertEqual(length(Nums), length(Nums2)).




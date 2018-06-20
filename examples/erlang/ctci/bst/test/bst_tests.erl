-module(bst_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


bst_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun length_test/0]}.


setup() ->
    rand:seed(os:timestamp()),
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
    T = bst:new(CmpFn),
    lists:foldl(fun (E, T0) -> bst:add(E, T0) end, T, Elts2).

length_test() ->
    T = bst:new(fun int_cmp/2),
    ?assertEqual(0, bst:length(T)),
    T2 = bst:add(99, T),
    ?assertEqual(1, bst:length(T2)),
    T3 = random_tree(fun int_cmp/2, lists:seq(1, 10)),
    ?assertEqual(10, bst:length(T3)).

pre_order_traverse_test() ->
    Nums = lists:seq(1,10),
    T = random_tree(fun int_cmp/2, Nums),
    %% ?debugFmt("T=~p", [T]),
    Nums2 = bst:pre_order_traverse(
              T,
              fun (N, Acc) -> [N | Acc] end,
              []),
    ?assertEqual(Nums, Nums2).

post_order_traverse_test() ->
    Nums = lists:seq(1,10),
    T = random_tree(fun int_cmp/2, Nums),
    %% T = lists:foldl(fun (N, T0) -> bst:add(N, T0) end, bst:new(fun int_cmp/2), Nums),
    %% ?debugFmt("tree: T=~p", [T]),
    Nums2 = bst:post_order_traverse(
              T,
              fun (N, Acc) -> [N | Acc] end,
              []),
    ?assertEqual(lists:reverse(Nums), Nums2).

in_order_traverse_test() ->
    Nums = lists:seq(1,10),
    T = random_tree(fun int_cmp/2, Nums),
    %% T = lists:foldl(fun (N, T0) -> bst:add(N, T0) end, bst:new(fun int_cmp/2), Nums),
    %% ?debugFmt("tree: T=~p", [T]),
    Nums2 = bst:in_order_traverse(
              T,
              fun (N, Acc) -> [N | Acc] end,
              []),
    %% can we assert an ordering here if the tree is randomly constructed?
    ?assertEqual(length(Nums), length(Nums2)).



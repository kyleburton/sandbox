-module(bst_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


bst_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun length_test/0]}.


setup() ->
    ok.

teardown(_) ->
    ok.

int_cmp(A, B) when A > B ->
    -1;
int_cmp(A, B) when A == B ->
    0;
int_cmp(A, B) when A < B ->
    1.


length_test() ->
    T = bst:new(fun int_cmp/2),
    ?assertEqual(0, bst:length(T)),
    Nums = [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- lists:seq(1, 10)])],
    %% ?debugFmt("Nums=~p", [Nums]),
    T2 = bst:add(99, T),
    %% ?debugFmt("T2=~p", [T2]),
    ?assertEqual(1, bst:length(T2)),
    T3 = lists:foldl(fun (N, T) -> bst:add(N, T) end, T, Nums),
    %% ?debugFmt("T3=~p", [T3]),
    ?assertEqual(10, bst:length(T3)).

pre_order_traverse_test() ->
    Nums = lists:seq(1,10),
    T = lists:foldl(fun (N, T0) -> bst:add(N, T0) end, bst:new(fun int_cmp/2), Nums),
    ?debugFmt("T=~p", [T]),
    Nums2 = bst:pre_order_traverse(
              T,
              fun (N, Acc) -> [N | Acc] end,
              []),
    ?assertEqual(Nums, Nums2).

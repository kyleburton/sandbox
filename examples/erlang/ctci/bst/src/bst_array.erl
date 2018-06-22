-module(bst_array).

-export([new/1,
         length/1,
         add/2,
         pre_order_traverse/3,
         post_order_traverse/3,
         in_order_traverse/3
        ]).

-record(bst_array, {
          cmp_fn,
          buff :: array:array()
         }).

new(CmpFn) ->
  #bst_array{cmp_fn=CmpFn, buff=array:new()}.

length(T) ->
    pre_order_traverse(
      T,
      fun (_Val, Acc) ->
              Acc + 1
      end,
      0).

left_pos(N) ->
  N*2 + 1.

right_pos(N) ->
  N*2 + 2.

%% NB: can we support storing undefined?
visit(undefined, _F, Acc) ->
  Acc;
visit(Val, F, Acc) ->
  F(Val, Acc).

pre_order_traverse(T, F, Acc) ->
  pre_order_traverse(T, F, Acc, 0).

pre_order_traverse(#bst_array{buff=Buff} = T, F, Acc, NodePos) ->
  case NodePos < array:size(Buff) of
    true ->
      Acc2 = pre_order_traverse(T, F, Acc, left_pos(NodePos)),
      Acc3 = visit(array:get(NodePos, Buff), F, Acc2),
      pre_order_traverse(T, F, Acc3, right_pos(NodePos));
    false ->
      Acc
  end.

post_order_traverse(T, F, Acc) ->
  post_order_traverse(T, F, Acc, 0).

post_order_traverse(#bst_array{buff=Buff} = T, F, Acc, NodePos) ->
  case NodePos < array:size(Buff) of
    true ->
      Acc2 = post_order_traverse(T, F, Acc, right_pos(NodePos)),
      Acc3 = visit(array:get(NodePos, Buff), F, Acc2),
      post_order_traverse(T, F, Acc3, left_pos(NodePos));
    false ->
      Acc
  end.


in_order_traverse(T, F, Acc) ->
  in_order_traverse(T, F, Acc, 0).

in_order_traverse(#bst_array{buff=Buff} = T, F, Acc, NodePos) ->
  case NodePos < array:size(Buff) of
    true ->
      Acc2 = visit(array:get(NodePos, Buff), F, Acc),
      Acc3 = pre_order_traverse(T, F, Acc2, right_pos(NodePos)),
      in_order_traverse(T, F, Acc3, left_pos(NodePos));
    false ->
      Acc
  end.



add(Val, #bst_array{} = T) ->
  add(Val, T, 0).

add(Val, #bst_array{buff=Buff, cmp_fn=F} = T, NodePos) ->
  Entry = array:get(NodePos, Buff),
  case {Entry, F(Val, Entry)} of
    {undefined, _} ->
      T#bst_array{buff=array:set(NodePos, Val, Buff)};
    {_, -1} ->
      add(Val, T, NodePos*2 + 1);
    {_, 0} ->
      T;
    {_, 1} ->
      add(Val, T, NodePos*2 + 2)
  end.

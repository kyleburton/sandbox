-module(bst).

-export([new/1,
         length/1,
         add/2,
         pre_order_traverse/3]).

new(CmpFn) ->
    {bst, CmpFn, none}.

length(T) ->
    pre_order_traverse(
      T,
      fun (_Val, Acc) ->
              Acc + 1
      end,
      0).

%% pre_order_traverse({bst, _CmpFn, Top}, F, Acc) ->
%%     pre_order_traverse2(Top, F, Acc).
%% 
%% pre_order_traverse2(none, _F, Acc) ->
%%     Acc;
%% pre_order_traverse2({none, Val, none}, F, Acc) ->
%%     F(Val, Acc);
%% pre_order_traverse2({Left, Val, none}, F, Acc) ->
%%     Acc2 = pre_order_traverse2(Left, F, Acc),
%%     F(Val, Acc2);
%% pre_order_traverse2({none, Val, Right}, F, Acc) ->
%%     Acc2 = F(Val, Acc),
%%     pre_order_traverse2(Right, F, Acc2);
%% pre_order_traverse2({Left, Val, Right}, F, Acc) ->
%%     Acc2 = pre_order_traverse2(Left, F, Acc),
%%     Acc3 = F(Val, Acc2),
%%     pre_order_traverse2(Right, F, Acc3).

pre_order_traverse({bst, _CmpFn, Top}, F, Acc) ->
    pre_order_traverse2(Top, F, Acc).

pre_order_visit(none, _F, Acc) ->
  Acc;
pre_order_visit(Val, F, Acc) ->
  F(Val, Acc).

pre_order_traverse2(none, _F, Acc) ->
    Acc;
pre_order_traverse2({Left, Val, Right}, F, Acc) ->
    Acc2 = pre_order_traverse2(Left, F, Acc),
    Acc3 = pre_order_visit(Val, F, Acc2),
    pre_order_traverse2(Right, F, Acc3).



add(Val, {bst, CmpFn, Nodes}) ->
    {bst, CmpFn, add(Val, CmpFn, Nodes)}.

add(Val, _CmpFn, none) ->
    {none, Val, none};
add(Val, CmpFn, {Left, NodeVal, Right} = T) ->
    case CmpFn(Val, NodeVal) of
        -1 ->
            {add(Val, CmpFn, Left), NodeVal, Right};
        0 ->
            T;
        1 ->
            {Left, NodeVal, add(Val, CmpFn, Right)}
    end.

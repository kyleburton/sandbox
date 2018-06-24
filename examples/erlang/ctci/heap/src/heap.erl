-module(heap).

-export([new/1,
         length/1,
         insert/2,
         pop/1,
         reduce/3]).

-export([position_pairs/1,
         to_dotty/1]).

-include("heap.hrl").

new(CmpFn) ->
  #heap{cmp_fn=CmpFn}.


length(#heap{buff=Buff}) ->
  array:size(Buff).

insert(Val, #heap{buff=Buff, cmp_fn=CmpFn} = H) ->
  %% add to tree, keepig it balanced
  Pos = array:size(Buff),
  Buff2 = array:set(Pos, Val, Buff),
  H#heap{buff=up_heap(Pos, CmpFn, Buff2)}.

is_odd(N) ->
  N rem 2 =:= 1.

left_pos(P) ->
  P*2 + 1.

right_pos(P) ->
  P*2 + 2.

parent_pos(Pos) ->
  %% lpos = pos*2+1
  %% rpos = pos*2+2
  %% 0: l=1; r=2
  %% 1: l=3; r=4
  %% 2: l=5; r=6
  %% 3: l=7; r=8
  %% 4: l=9; r=10
  %% left is always odd, right is always even
  case is_odd(Pos) of
    true ->
        (Pos-1) div 2;
    false ->
        (Pos-2) div 2
  end.

up_heap(0, _CmpFn, Buff) ->
  Buff;
up_heap(Pos, CmpFn, Buff) ->
  PPos = parent_pos(Pos),
  Val = array:get(Pos, Buff),
  PVal = array:get(PPos, Buff),
  case CmpFn(Val, PVal) of
    -1 ->
      Buff2 = array:set(Pos, PVal, Buff),
      Buff3 = array:set(PPos, Val, Buff2),
      up_heap(PPos, CmpFn, Buff3);
    _ ->
      Buff
  end.

reduce(Fn2, Acc, #heap{buff=Buff}) ->
  reduce(Fn2, Acc, Buff, 0).

reduce(Fn2, Acc0, Buff, Pos) ->
  case Pos >= array:size(Buff) of
    true ->
      Acc0;
    false ->
      Acc2 = Fn2(array:get(Pos, Buff), Acc0),
      Acc3 = reduce(Fn2, Acc2, Buff, left_pos(Pos)),
      reduce(Fn2, Acc3, Buff, right_pos(Pos))
  end.

position_pairs(#heap{} = H) ->
  position_pairs(H#heap.buff, 0).

position_pairs(Buff, Pos) ->
  position_pairs(Buff, Pos, left_pos(Pos), right_pos(Pos), []).

position_pairs(Buff, Pos, LPos, RPos, Acc0) ->
  io:format("position_pairs: Buff=~p", [Buff]),
  io:format("position_pairs: Pos=~p; LPos=~p; RPos=~p; size=~p~n",
            [Pos, LPos, RPos, array:size(Buff)]),
  case {LPos < array:size(Buff), RPos < array:size(Buff)} of
    {true, true} ->
      Acc1 = [{Pos, LPos} | Acc0],
      Acc2 = [{Pos, RPos} | Acc1],
      Acc3 = position_pairs(Buff, LPos, left_pos(LPos), right_pos(LPos), Acc2),
      position_pairs(Buff, RPos, left_pos(RPos), right_pos(RPos), Acc3);
    {true, false} ->
      Acc1 = [{Pos, LPos} | Acc0],
      position_pairs(Buff, LPos, left_pos(LPos), right_pos(LPos), Acc1);
    {false, true} ->
      Acc1 = [{Pos, RPos} | Acc0],
      position_pairs(Buff, RPos, left_pos(RPos), right_pos(RPos), Acc1);
    _ ->
      Acc0
  end.

to_dotty(#heap{buff=Buff} = H) ->
  Pairs0 = [{L, array:get(L, Buff), R,array:get(R, Buff)} || {L,R} <- position_pairs(H)],
  Pairs1 = [io_lib:format("\"~p=~p\" -> \"~p=~p\"", [Lp, L, Rp, R]) || {Lp, L, Rp, R} <- Pairs0],
  "digraph {\n" ++
  string:join(Pairs1, ";\n  ") ++
  "\n}\n".


down_heap(0, _CmpFn, Buff) ->
  Buff;
down_heap(Pos, CmpFn, Buff) ->
  %% compare Pos to it's children, if in the correct order, we're done
  %% otherwise swap with the lesser child
  Buff.

pop(#heap{buff=Buff0, cmp_fn=CmpFn} = H) ->
  TopVal = array:get(0, Buff0),
  LastVal = array:get(array:size(Buff0) - 1, Buff0),
  Buff1 = array:set(0, LastVal, Buff0),
  Buff2 = array:resize(array:size(Buff0) - 1, Buff1),
  Buff3 = down_heap(0, CmpFn, Buff2),
  {TopVal, H#heap{buff=Buff3}}.



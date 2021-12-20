-module(erlp_ch1).
-export([sum/1]).


sum(Num) ->
    sum(0,Num).


sum(Acc,Num) when Num == 0 ->
    Acc;
sum(Acc,Num) ->
    sum(Acc+Num,Num-1).



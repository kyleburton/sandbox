-module(boolean).
-export([b_not/1,b_and/2,b_or/2,b_nand/2]).

b_not(true) ->
    false;
b_not(false) ->
    true;
b_not(_Other) ->
    true.

b_and(true,true) ->
    true;
b_and(_,_) ->
    false.

b_or(true,_) ->
    true;
b_or(_,true) ->
    true;
b_or(_,_) ->
    false.

%% NAND
%%   INPUT     OUTPUT
%%   A   B     A NAND B
%%
%%   0   0      1
%%   0   1      1
%%   1   0      1
%%   1   1      0
b_nand(true,true) ->
    false;
b_nand(_,_) ->
    true.

-module(tree).
-export([empty/0, insert/3, lookup/2]).

% should use gb_trees instead of rolling out own: http://erlang.org/doc/man/gb_trees.html

empty() ->
  {node, nil}.

insert(NewKey, NewVal, {node, nil}) ->
  {node, 
   {NewKey, NewVal, 
    {node, nil}, 
    {node, nil}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
  {node, 
   {Key, Val, 
    insert(NewKey, NewVal, Smaller),
    Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
  {node, 
   {Key, Val, 
    Smaller, 
    insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
  {node, 
   {Key, Val, 
    Smaller, 
    Larger}}.

lookup(_Key, {node, nil}) ->
  undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
  Val;
lookup(Key, {node, {K2, _, Smaller, _}}) when Key < K2 ->
  lookup(Key, Smaller);
lookup(Key, {node, {K2, _, _, Larger}}) when Key > K2 ->
  lookup(Key, Larger).

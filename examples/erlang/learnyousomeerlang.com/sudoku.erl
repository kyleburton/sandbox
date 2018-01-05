-module(sudoku).
-export([]).

%
% Sudoku, one erlang process per cell
%
% cell state, a prop list:
% [
%  {solved, bool}, 
%  {values, [int]}, 
%  {neighbors, [pids]}
% ]
%


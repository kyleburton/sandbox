-module(sudoku).
-export([
         make_board/0,
         cell_proc/1,
         stop_all/1,
         check_cell_values/2,
         board_from_file/1,
         board_to_html/2
        ]).

%
% Sudoku, one erlang process per cell
%
% cell state, a prop list:
% [
%  {position, X, Y}
%  {values, [int]}, 
%  {neighbors, [pids]}
% ]
%
% a neighbor is
%
% * in the same 3x3 grid
% * same row 
% * same column
% 
% if a cell has 1 value, then it's solved
% * broadcast to all neighbors to remove that value
% 

position_to_cell(N) when N =:= 0 ->
  [1,2,3,4,5,6,7,8,9];
position_to_cell(N) when N > 0 ->
  [N].


parse_position(Str) ->
  {Int,_} = string:to_integer(string:trim(Str)),
  position_to_cell(Int).

parse_line(Line) ->
  [parse_position(C) || C <- string:split(Line, ",", all), C =/= "\n"].

board_from_file(FileName) ->
  [parse_line(Line) || Line <- util:readlines(FileName)].

make_board() ->
  make_board(9, []).

make_board(0, Grids) ->
  Grids;
make_board(NumGrids, Grids) ->
  make_board(NumGrids-1, [make_cell()|Grids]).


cell_proc(State) ->
  io:format("cell[pid=~p; values=~p] in receive~n", [self(), State]),
  receive 

    {get_values, FromPid} -> 
      FromPid ! {ok, State},
      cell_proc(State);

    {set_values, FromPid, Values} -> 
      FromPid ! ok,
      cell_proc({values, Values});

    {remove, FromPid, Val} -> 
      FromPid ! ok,
      cell_proc(remove_value(State, Val));

    {stop, FromPid} -> 
      FromPid ! ok

  end.

remove_value({values, Values}, Val) ->
  [V  || V <- Values, V =/= Val].

make_cell() -> 
  spawn(sudoku, cell_proc, [{values, [1, 2, 3, 4, 5, 6, 7, 8, 9]}]).

stop_all([]) ->
  ok;
stop_all([H|T]) ->
  H ! stop,
  stop_all(T).

check_cell_values(CellNum, Board) ->
  lists:nth(CellNum, Board) ! {get_values, self()},
  receive
    {ok, State} ->
      io:format("state=~p~n", [State]),
      State
  end.

cell_to_str(Elts) ->
  string:join([io_lib:format("~p", [E]) || E <- Elts], ",").

row_to_tr(Board, RNum) ->
  "<tr>" ++ string:join([ 
                         io_lib:format("<td>~s</td>", [cell_to_str(lists:nth(CNum, lists:nth(RNum, Board)))])
                         || CNum <- [1,2,3,4,5,6,7,8,9]
                        ], "") ++ "</tr>".

% render to a table
board_to_html(FName, Board) ->
  file:write_file(FName, 
                     "<html>"
                  ++ "<head>"
                  ++ "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">"
                  ++ "</head>"
                  ++ "<body>"
                  ++ "<table>"
                  ++ "<thead>"
                  ++ "</thead>"
                  ++ "<tbody>"
                  ++ string:join([row_to_tr(Board, RNum) || RNum <- [1,2,3,4,5,6,7,8,9]], "")
                  ++ "</tbody>"
                  ++ "<tfoot>"
                  ++ "</tfoot>"
                  ++ "</table>"
                  ++ "</body>"
                  ++ "</html>"
                 ).


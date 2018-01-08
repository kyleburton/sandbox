-module(util).
-export([readlines/1]).


readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try 
    get_all_lines(Device, [])
  after 
    file:close(Device)
  end.

get_all_lines(Device, Acc) ->
  case io:get_line(Device, "") of
    eof  -> lists:reverse(Acc);
    Line -> get_all_lines(Device, [Line|Acc])
  end.

-module(cluster).

-export([test_node/1, join/1, create_usr_table/0, local_init/0,
  insert_some_recs/0,
  add_usr/3, find_usr/1]).

-include("usr.hrl").

test_node(OtherNode) ->
  case net_adm:ping(OtherNode) of
    pong ->
      io:format("~p is reachable\n", [OtherNode]),
      ok;
    pang ->
      io:format("~p not is reachable\n", [OtherNode]),
      error
  end.

local_init() ->
  mnesia:change_table_copy_type(schema, node(), disc_copies).

join(OtherNode) ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  application:start(mnesia),
  mnesia:change_config(extra_db_nodes, [OtherNode]),
  local_init().

% load the usr record before you call this from erl, with
%    rr("erl/usr.hrl").
create_usr_table() ->
  Fields = record_info(fields, usr),
  mnesia:create_table(
    usr,
    [ {disc_copies, [node()|nodes()]},
      {type,        set},
      {attributes,  Fields},
      {index,       [id]}]).


% mnesia:info().
% mnesia:system_info(tables).
% mnesia:table_info(usr, attributes).  % also all, arity, size, type

insert_some_recs () ->
  Rec = #usr{msisdn=700000003, id=3, status=enabled, plan=prepay, services=[data, sms, lbs]},
  mnesia:transaction(fun () -> mnesia:write(Rec) end).

add_usr(PhoneNo, CustId, Plan) when Plan==prepay; Plan==postpay ->
  Rec = #usr{msisdn = PhoneNo,
             id     = CustId,
             plan   = Plan},
          Fun = fun() -> mnesia:write(Rec) end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

find_usr(Phone) ->
  mnesia:transaction(fun() -> mnesia:read({usr, Phone}) end).


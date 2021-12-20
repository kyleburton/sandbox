-module(exml_cdata_app).

-behaviour(application).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    exml_cdata_sup:start_link().

stop(_State) ->
    ok.


-ifdef(TEST).

simple_test() ->
  io:format("simple_test~n", []),
  {ok, Parser} = exml:parse(<<"<my_xml_doc/>">>),
  ?debugFmt("Parser=~p~n", [Parser]),
  El = {xmlel, <<"foo">>,
        [{<<"attr1">>, <<"bar">>}],
        [{xmlcdata, <<"Some Value">>}]},
  ?debugFmt("El=~p~n", [El]),
  L = exml:to_list(El),
  ?debugFmt("L=~p~n", [L]).
  %ok = application:start(exml_cdata),
  %?assertNot(undefined == whereis(exml_cdata_sup)).

-endif.

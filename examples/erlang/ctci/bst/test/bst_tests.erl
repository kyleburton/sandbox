-module(bst_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


bst_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun length_test/0]}.


setup() ->
    ok.

teardown(_) ->
    ok.


length_test() ->
    ok.

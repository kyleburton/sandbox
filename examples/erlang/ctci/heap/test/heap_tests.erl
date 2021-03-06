-module(heap_tests).

-compile(export_all).

-include("heap.hrl").

-include_lib("eunit/include/eunit.hrl").

bst_array_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     []}.

setup() ->
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> = crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {I1, I2, I3}),
    ok.

teardown(_) ->
    ok.

%%----------------------------------------------------------------------

min_int_cmp(A, B) when A < B ->
    -1;
min_int_cmp(A, B) when A == B ->
    0;
min_int_cmp(A, B) when A > B ->
    1.

max_int_cmp(A, B) ->
    min_int_cmp(A,B) * -1.

shuffle_list(Elts) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- Elts])].

random_heap(CmpFn, Elts) ->
    heap:from_list(CmpFn, shuffle_list(Elts)).

%%----------------------------------------------------------------------

length_test() ->
    H = heap:new(fun max_int_cmp/2),
    ?assertEqual(0, heap:length(H)),
    H2 = heap:insert(33, H),
    ?assertEqual(1, heap:length(H2)).

reduce_test() ->
    H = random_heap(fun max_int_cmp/2, lists:seq(1, 15)),
    Sum = heap:reduce(
            fun (Val, Acc) ->
                    %% ?debugFmt("reduce_test#fn: Val=~p", [Val]),
                    Val + Acc
            end,
            0,
            H),
    ?assertEqual(Sum, 120).

print_test() ->
    H0 = random_heap(fun max_int_cmp/2, []),
    P0 = heap:position_pairs(H0),
    %% ?debugFmt("H0=~p; P0=~p", [array:to_list(H0#heap.buff), P0]),

    H1 = random_heap(fun max_int_cmp/2, [1]),
    P1 = heap:position_pairs(H1),
    %% ?debugFmt("H1=~p; P1=~p", [array:to_list(H1#heap.buff), P1]),

    H2 = random_heap(fun max_int_cmp/2, [1, 2]),
    P2 = heap:position_pairs(H2),
    %% ?debugFmt("H2=~p; P2=~p", [array:to_list(H2#heap.buff), P2]),

    H3 = random_heap(fun max_int_cmp/2, [1, 2, 3]),
    P3 = heap:position_pairs(H3),
    %% ?debugFmt("H3=~p; P3=~p", [array:to_list(H3#heap.buff), P3]),

    H4 = random_heap(fun max_int_cmp/2, lists:seq(1, 15)),
    P4 = heap:position_pairs(H4),
    %% ?debugFmt("H4=~p; P4=~p", [array:to_list(H4#heap.buff), P4]),

    ok.

randlist(Size) ->
<<<<<<< Updated upstream
    [trunc(rand:uniform()*100) || _X <- lists:seq(1, Size)].
=======
    [trunc(rand:uniform()*1000) || _X <- lists:seq(1, Size)].
>>>>>>> Stashed changes

uniq_randlist(Size) ->
    uniq_randlist(Size*10, Size).

uniq_randlist(MaxVal, Size) ->
    uniq_randlist(MaxVal, Size, [], sets:new()).

uniq_randlist(_MaxVal, 0, Acc, _Seen) ->
    Acc;
uniq_randlist(MaxVal, Size, Acc, Seen) 
  when MaxVal > Size ->
    V = trunc(rand:uniform()*MaxVal),
    case sets:is_element(V, Seen) of
        true ->
            uniq_randlist(MaxVal, Size, Acc, Seen);
        false ->
            uniq_randlist(MaxVal, Size - 1, [V|Acc], sets:add_element(V, Seen))
    end;
uniq_randlist(MaxVal, Size, Acc, Seen) ->
    %% NB: if MaxVal is < Size this process cannot terminate.
    %% if they're equal 
    throw(invalid_maxval).

dotty_test() ->
    lists:foreach(fun gen_dotty/1, lists:seq(1, 15)),
    %% 1 = 2.
    ok.

gen_dotty(N) ->
    Fname = io_lib:format("../dotty_test-~2..0B.dot", [N]),
    heap_to_dot_file(Fname, random_heap(fun max_int_cmp/2, randlist(N))).

heap_to_dot_file(Fname, H) ->
    Data = heap:to_dotty(H),
    file:write_file(Fname, Data).


enable_tracing(Specs) ->
    %% dbg:start(),
    dbg:tracer(),
    dbg:p(all, [c]),
    enable_traces(Specs).

%% https://stackoverflow.com/questions/34658714/how-to-debug-erlang-code-during-rebar3-eunit
%% eunit:test(your_test_module,[verbose]).
enable_traces([{M,F} | Rest]) ->
    %% ?debugFmt("ENABLE TRACING: ~p:~p", [M, F]),
    dbg:tpl(M, F, [cx]),
    enable_traces(Rest);
enable_traces([M| Rest]) ->
    %% ?debugFmt("ENABLE TRACING: ~p:*", [M]),
    dbg:tpl(M, [cx]),
    enable_traces(Rest);
enable_traces([]) ->
    ok.

pop_test() ->
    %% enable_tracing([heap]),

    %% NB: this should throw
    %% HEmpty = heap:new(fun max_int_cmp/2),
    %% {Val, HEmpty2} = heap:pop(),
    H1 = random_heap(fun max_int_cmp/2, [137]),
    ?assertEqual(1, heap:length(H1)),
    %% ?debugFmt("pre-pop: H1=~p", [H1]),
    {Val, H2} = heap:pop(H1),
    %% ?debugFmt("post-pop: H2=~p", [H2]),
    ?assertEqual(0, heap:length(H2)),
    ?assertEqual(137, Val),

    H3 = random_heap(fun max_int_cmp/2, [11, 22]),
    ?assertEqual(2, heap:length(H3)),
    %% ?debugFmt("pre-pop: H3=~p", [H3]),
    {Val2, H4} = heap:pop(H3),
    %% ?debugFmt("post-pop: H4=~p", [H4]),
    ?assertEqual(1, heap:length(H4)),
    ?assertEqual(22, Val2),

    H5 = random_heap(fun max_int_cmp/2, [11, 22, 33]),
    ?assertEqual(3, heap:length(H5)),
    %% ?debugFmt("pre-pop: H3=~p", [H5]),
    {Val3, H6} = heap:pop(H5),
    %% ?debugFmt("post-pop: H4=~p", [H6]),
    ?assertEqual(2, heap:length(H6)),
    ?assertEqual(33, Val3),
    ok.

pop_visual_test() ->
<<<<<<< Updated upstream
    H0  = random_heap(fun max_int_cmp/2, randlist(15)),
    H0a = lists:foldl(fun(N, H1) ->
                              BFname = io_lib:format("../pop_test-before-~2..0B.dot", [N]),
                              heap_to_dot_file(BFname, H1),
                              {Val, H2} = heap:pop(H1),
                              %% ?debugFmt("pop_test: N=~p Val=~p", [N, Val]),
                              AFname = io_lib:format("../pop_test-after-~2..0B.dot", [N]),
                              heap_to_dot_file(AFname, H2),
                              H2
                      end,
                      H0,
                      lists:seq(1, 5)),
=======
    NumElts = 15,
    H0  = random_heap(fun max_int_cmp/2, randlist(NumElts)),
    H0a = lists:foldl(
            fun(N, H1) ->
                    BFname = io_lib:format("../pop_test-~2..0B-00before.dot", [N]),
                    heap_to_dot_file(BFname, H1),
                    {Val, H2} = heap:pop(H1),
                    ?debugFmt("pop_test: N=~p Val=~p", [N, Val]),
                    AFname = io_lib:format("../pop_test-~2..0B-01after.dot", [N]),
                    heap_to_dot_file(AFname, H2),
                    H2
            end,
            H0,
            lists:seq(1, NumElts)),
    FFname = io_lib:format("../pop_test-XX-99final.dot", []),
    heap_to_dot_file(FFname, H0a),
    ok.
>>>>>>> Stashed changes

krb_test() ->
    H0  = random_heap(fun max_int_cmp/2, [3,7,5,9,1]),
    ?debugFmt("H0=~p", [heap:to_list(H0)]),
    {Val, H1} = heap:pop(H0),
    ?debugFmt("H1=~p", [heap:to_list(H1)]),
    {Val, H2} = heap:pop(H1),
    ?debugFmt("H2=~p", [heap:to_list(H2)]),
    {Val, H3} = heap:pop(H2),
    ?debugFmt("H3=~p", [heap:to_list(H3)]),
    {Val, H4} = heap:pop(H3),
    ?debugFmt("H4=~p", [heap:to_list(H4)]).

%% ok, the heaps are semi-ordered, so after popping the MAX value
%% we take the last value, place it at the top and then do a 'down heap'
%% that doens't mean that the last value is the MIN value as the heap
%% is partially ordered so there is on gauranttee this is the MIN value



krb2_test() ->
    NumElts = 3,
    H0  = heap:from_list(fun max_int_cmp/2, [122,26,8]),
    H0a = lists:foldl(
            fun(N, H1) ->
                    BFname = io_lib:format("../pop_test-~2..0B-00before.dot", [N]),
                    heap_to_dot_file(BFname, H1),
                    {Val, H2} = heap:pop(H1),
                    ?debugFmt("pop_test: N=~p Val=~p", [N, Val]),
                    AFname = io_lib:format("../pop_test-~2..0B-01after.dot", [N]),
                    heap_to_dot_file(AFname, H2),
                    H2
            end,
            H0,
            lists:seq(1, NumElts)),
    FFname = io_lib:format("../pop_test-XX-99final.dot", []),
    heap_to_dot_file(FFname, H0a),
    ok.

<<<<<<< Updated upstream

minheap_order_test() ->
    %% enable_tracing([heap, heap_tests]),
    H1 = random_heap(fun max_int_cmp/2, randlist(15))
    L1 = heap:pop_to_list(H1),
    L1 = lists:sort(L1),
    L2 = heap:pop_to_list(random_heap(fun max_int_cmp/2, randlist(15))),
    L2 = lists:sort(L2).
=======
order_test() ->
    Elts = [26,122,8],
    H0 = heap:from_list(fun max_int_cmp/2, shuffle_list(Elts)),
    L = heap:to_ordered_list(H0),
    ?assertEqual(lists:sort(Elts), L).
>>>>>>> Stashed changes

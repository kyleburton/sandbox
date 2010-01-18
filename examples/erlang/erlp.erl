-module(erlp).
-export([area/1,returnsTrue/0,returnsFalse/0,returnsIfTrue/1,returnsNotTrue/1,booleanTests/0,
        factorial/1,bump/1,bump_by/2,merge/2,average/1,md5_hex/1,receivingFunction/0]).


%% % 'printf'
%% io:format("this: ~p~n", [{an_atom,"is here"}]).

%% % note: the IO functions often support a 'non-flat' structure as the format string
%% io:format(["this", "is some", "stuff","\n"]).

%% % how can you test if something is an atom?

%% % how to convert an atom to a string?
%% % and back?

%% % you can have improper lists (eg: [1|a] vs [1|[a]])
%% % how can you test something to see if it's a list?

area({square,Side}) ->
    Side * Side;
area({circle, Radius}) ->
    math:pi() * Radius * Radius;
area({triangle, A, B, C}) ->
    S = (A + B + C) / 2,
    math:sqrt(S*(S-A)*(S-B)*(S-C));
area(_Other) ->
    {error, invalid_object}.


% and and andalso, or and orelse
returnsTrue()->
    io:format("returnsTrue~n"),
    true.

returnsFalse()->
    io:format("returnsFalse~n"),
    false.

returnsIfTrue(Thing)->
    io:format("returnsIfTrue(~p)~n",[Thing]),
    true == Thing.

returnsNotTrue(Thing)->
    io:format("returnsNotTrue(~p)~n",[Thing]),
    true /= Thing.

booleanTests() ->
    io:format("true and true      => ~p~n", [returnsTrue() and returnsTrue()]),
    io:format("true andalso true  => ~p~n", [returnsTrue() andalso returnsTrue()]),
    io:format("true or true       => ~p~n", [returnsTrue() or returnsTrue()]),
    io:format("true orelse true   => ~p~n", [returnsTrue() orelse returnsTrue()]),
    io:format("false and true     => ~p~n", [returnsFalse() and returnsTrue()]),
    io:format("false andalso true => ~p~n", [returnsFalse() andalso returnsTrue()]),
    io:format("false or true      => ~p~n", [returnsFalse() or returnsTrue()]),
    io:format("false orelse true  => ~p~n", [returnsFalse() orelse returnsTrue()]).

factorial(0)->
    1;
factorial(N) ->
    factorial2(N,N-1).

factorial2(Acc,0) ->
    Acc;
factorial2(Acc,1) ->
    Acc;
factorial2(Acc,N) ->
    factorial2(Acc*N,N-1).




%
% Dynamically calling a function with apply:
%
% Module = erlp.
% Function = area.
% Arguments = [{circle,10}].
%  => 314.1592653589793

% read a string (gets/fgets):
%   io:get_line("gissa line>").

% read a limited number of characters:
%   io:get_chars("tell me> ",2).

% read an erlang term:
%   io:read("ok, then>>").
% (try '2+3.' -- it errors as it's not a term)

% io:format
%
%   ~c      - character
%   ~f      - float
%   ~e      - scientific notation
%   ~w      - erlang term in standard syntax
%   ~p      - erlang term / data (like ~w), with 'pretty printing'
%   ~W, ~P  - similar to ~w and ~p, limits structure to a depth of 3,
%             these take an extra argument indicating the max depth
%             for printing terms
%   ~B      - integer in base 10
%


bump_acc(Acc,[])->
    lists:reverse(Acc);
bump_acc(Acc,[H|T]) ->
    bump_acc([H+1|Acc],T).

bump(L)->
    bump_acc([],L).

bump_by_acc(_Val,Acc,[])->
    lists:reverse(Acc);
bump_by_acc(Val,Acc,[H|T]) ->
    bump_acc([H+Val|Acc],T).

bump_by(Val,L)->
    bump_by_acc(Val,[],L).


merge(Xs,Ys)->
    lists:reverse(mergeL(Xs,Ys,[])).

mergeL([X|Xs],Ys,Zs)->
    mergeR(Xs,Ys,[X|Zs]);
mergeL([],[],Zs)->
    Zs.

mergeR(Xs,[Y|Ys],Zs)->
    mergeL(Xs,Ys,[Y|Zs]);
mergeR([],[],Zs) ->
    Zs.

% erlp:merge([a,b,c],[1,2,3]).
%   => [a,1,b,2,c,3]


average([]) ->
    0;
average(List)->
    average_acc(List,0,0).

average_acc([],Sum,Length) ->
    Sum/Length;
average_acc([H|T],Sum,Length) ->
    average_acc(T,Sum+H,Length+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% array module
%% A0 = array:new().
%% A1 = array:set(0,1,A0).
%% A2 = array:set(1,2,A1).
%% A3 = array:set(2,3,A2).
%% array:foldl(fun(_Index,Term,Acc) -> Term+Acc end, 0, A3).
%% => 6

%% A4 = array:from_list([1,2,3,4,5,6,7,8,9,10]).
%% array:foldl(fun(_Index,Term,Acc) -> Term+Acc end, 0, A4).
%% => 55


%% make an extendable list (with a default value)
%% A5 = array:from_list([1,2,3,4,5,6,7,8,9,10],0).
%% array:foldl(fun(_Index,Term,Acc) -> Term+Acc end, 0, A5).
%% => 55
%% A6 = array:set(10,11,A5).
%% array:foldl(fun(_Index,Term,Acc) -> Term+Acc end, 0, A6).
%% => 66


%% array:from_orddict([{0,a},{1,b},{2,c}]).
%% => creates a list from the index+term pairs


%% array:is_fix(A0). => false
% if you specify a size on construction (and no default), the array will have fixed size (performance optimization)ss
%% array:is_fix(array:new(4)). => true


%% A7 = array:map(fun (Index,Term) -> x end, array:from_orddict([{0,a},{1,b},{2,c}])).

% arrays can be made resiable (relax) or fixed (fix)

% length can be obtained with size:

% array:size(A7).

% the array:sparse* functions perform aggregate operations on the array while skipping the default values


% array:to_list(A7).
%  => [x,x,x]


% array:to_orddict(A7).
%  => [{0,x},{1,x},{2,x}]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calendar module

% calendar:date_to_gregorian_days({2010,1,16}).
% => 734153


% calendar:datetime_to_gregorian_seconds({{2010,1,16},{13,33,28}}).
% => 63430868008


% calendar:day_of_the_week({2010,1,16}).
% => 6

% calendar:local_time().
% => {{2010,1,16},{13,35,25}}

% calendar:local_time_to_universal_time(calendar:local_time()).
% {{2010,1,16},{18,36,2}}

% calendar:local_time_to_universal_time_dst(calendar:local_time()).
% [{{2010,1,16},{18,36,18}}]

% erlang:now().
% calendar:now_to_local_time(erlang:now()).
%  => {{2010,1,16},{13,37,8}}


% calendar:seconds_to_daystime(86400).
% => {1,{0,0,0}}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dict module

% D0 = dict:new().
% D1 = dict:append(kyle,"burton",D0).
% D2 = dict:append(kristin,"burton",D1).
% D3 = dict:erase(kyle,D2).

% dict:fetch(kristin,D3).
%  => ["burton"]
% dict:fetch(bob,D3).
%  => ``an exception''

% dict:fetch_keys(D2).

% dict:find(kyle,D2).
% => {ok,["burton"]}
% dict:find(bob,D2).
% => error (not an exception, but ther term `error').


% dict:from_list([{kyle,"burton"},{krsitin,"burton"}]).
% sh ould be about the same as D2

% dict:is_key(kyle,D2).
% => true
% dict:is_key(bob,D2).
% => false

%% dict:merge(fun(Key,V1,V2) -> V1+V2 end,
%%            dict:from_list([{kyle,1},{kristin,1}]),
%%            dict:from_list([{kyle,1}])).
% => dict with kyle=>2, kristin=>1

% dict:to_list( dict:merge(fun(Key,V1,V2) -> V1+V2 end, dict:from_list([{kyle,1},{kristin,1}]), dict:from_list([{kyle,1}])) ).
% => [{kyle,2},{kristin,1}]


%% store() adds or replaces a key/val pair


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% erlang module

% abs(-3.33).
% abs(-3).

% erlang:append_element({a,b},c).
% => {a,b,c}

% apply(fun(X,Y) -> X*Y end, [10,99]).
% => 990


%% doesn't work on erlang R12?, nope, but it does in R13B01
% erlang:atom_to_binary(foo,latin1).
% >> or
% atom_to_binary(foo,latin1).
% => <<"foo">>



% atom_to_list(foo).
% => "foo"

% read more into these and their uses:
% erlang:check_process_code(Pid, Module)
% erlang:cancel_timer


% concat_binary([<<"foo">>,<<" ">>,<<"bar">>]).
% <<"foo bar">>
% binary_to_list(<<"foo bar">>).
% => "foo bar"


% binary_to_atom(<<"foo_bar">>,latin1).
% => foo_bar


% erlang:crc32("foo").
% => 2356372769
% erlang:crc32(<<"foo">>).
% => 2356372769

% erlang:crc32([1,2,3]).
% => 1438416925

% erlang:date(). or date().
% => {2010,1,16}

% look for a tutorial or longer explanation for decode_packet?


% element(1,{a,b,c}).
% => a
% element(2,{a,b,c}).
% => b

% float(123).
% => 123.0

% float_to_list(123.0).
% => "1.23000000000000000000e+02"
% this is like 'spritnf', but just for floats

% doesn't seem to work on built in functions?
% erlang:fun_info(fun(A) -> A+1 end).
%% [{pid,<0.91.0>},
%%  {module,erl_eval},
%%  {new_index,2},
%%  {new_uniq,<<83,63,182,157,53,152,27,64,224,39,165,36,92,
%%              123,196,191>>},
%%  {index,6},
%%  {uniq,13229925},
%%  {name,'-expr/5-fun-2-'},
%%  {arity,1},
%%  {env,[[],
%%        {value,#Fun<shell.7.113407251>},
%%        {eval,#Fun<shell.24.81044707>},
%%        [{clause,1,
%%                 [{var,1,'A'}],
%%                 [],
%%                 [{op,1,'+',{var,1,'A'},{integer,1,...}}]}]]},
%%  {type,local}]



% erlang:fun_to_list(fun(A) -> A+1 end).

% get/put for interacting with the process dictionary
% put(kyle,{name,"burton"}).
% get(kyle).
% => {name,"burton"}
% get_keys({name,"burton"}).
% => [kyle]


% erlang:get_cookie().


% hd([1,2,3]).
% => 1

% integer_to_list(123).
% => "123"
% again, like sprintf but only for integers...it does support other number bases...
%% erlang:integer_to_list(123,2).
%% => "1111011"
%% erlang:integer_to_list(123,16).
%% => "7B"
%% 36 is the max (26 letters + 10 decimal numbers)
%% erlang:integer_to_list(123,36).
%% "3F"


%% predicate tests for types is_atom, is_binary, is_bitstring,
%% is_boolean, is_float, is_function(Term), is_function(Term,Arity),
%% is_integer, is_list, is_numbr, is_pid, is_oprt, is_reference,
%% is_tuple,


%% is_process_alive


%% length([1,2,3]).
%% => 3

%% convert a string to an erlang atom
%% list_to_atom("foo").
%% => foo


%% 'atof', String.to_f, Float.parseFloat
%% list_to_float("2.2017764e+0").
%% 2.2017764

%% atoi / atol, String.to_i, Integer.parseInt
%% list_to_integer("1234").
%% => 1234
%% this errors:
%% list_to_integer("1234.45").

%% also takes number bases
%% erlang:list_to_integer("1010",2).
%% => 10


%% list_to_tuple([name,[[first,"Kyle"],[last,"Burton"]]]).
%% {name,[[first,"Kyle"],[last,"Burton"]]}

% load_module(Module,Binary) -> Binary contains the object code for
% the module, loads or replaces Module in the running erlang node.


%% list the loaded modules
%% erlang:loaded().
%% [string,auth,lib,shell_default,eval_bits,erl_bits,
%%  io_lib_pretty,erl_internal,otp_internal,sets,ordsets,
%%  erl_lint,unicode,io,erl_scan,erl_parse,epp,filelib,ram_file,
%%  beam_lib,file_io_server,orddict,erl_eval,file,c,
%%  error_logger_tty_h,kernel_config,shell,io_lib_format|...]

%% io:format("~p", [erlang:loaded()]).
%% [string,auth,lib,shell_default,eval_bits,erl_bits,io_lib_pretty,erl_internal,
%%  otp_internal,sets,ordsets,erl_lint,unicode,io,erl_scan,erl_parse,epp,filelib,
%%  ram_file,beam_lib,file_io_server,orddict,erl_eval,file,c,error_logger_tty_h,
%%  kernel_config,shell,io_lib_format,proplists,io_lib,edlin,group,user_drv,
%%  user_sup,supervisor_bridge,standard_error,filename,ets,gb_sets,
%%  hipe_unified_loader,packages,code_server,code,file_server,net_kernel,
%%  global_group,erl_distribution,inet_gethost_native,inet_parse,inet,inet_udp,
%%  os,inet_config,inet_db,global,gb_trees,rpc,dict,supervisor,kernel,
%%  application_master,sys,application,gen_server,lists,application_controller,
%%  proc_lib,gen,gen_event,error_logger,heart,error_handler,erlang,
%%  erl_prim_loader,prim_zip,zlib,prim_file,prim_inet,init,otp_ring0]ok


%% erlang:localtime().
%% => {{2010,1,16},{15,22,49}}


%% what are references?
%% make_ref().
%% #Ref<0.0.0.413>

%% erlang:make_tuple(3,atom).
%% => {atom,atom,atom}

%% erlang:make_tuple(3,atom,[{1,this},{2,that}]).
%% => {this,that,atom}


%% erlang:max(a,b).
%% => b
%% erlang:max(10,12).
%% => 12
%% erlang:max({foo,"qux"},{foo,"bar"}).
%% => {foo,"qux"}


% erlang:md5("foo").
% <<172,189,24,219,76,194,248,92,237,239,101,79,204,196,164,216>>


% binary_to_list(erlang:md5("foo")).

md5_hex(Data)->
    md5_hex([],binary_to_list(erlang:md5(Data))).

md5_hex(Acc,[]) ->
    lists:concat(Acc);
md5_hex(Acc,[H|T]) ->
    md5_hex([erlang:integer_to_list(H,16)|Acc],T).

%% erlp:md5_hex("foof").
%% => "6A28E1C6B5C7AB4B6B6CA4B54417BC1"

% but this is not the same as echo -n "foof" | md5sum for some reason


% Port = open_port({spawn_executable,"/bin/ls"},[stream,{args,["."]},exit_status,stderr_to_stdout]).
%% send data to a port with port_command(Port,data)
% port_close(Port).


% round(5.5).


%% erlang:send_after(3000,self(),foo).
%% receive
%%     after 10000 -> {error,timed_out}
%%     foo -> {ok,foo}
%%     _ -> {ok,unknown}
%%     end.


%% more from the erlang module

%% create a new term with one value changed:
%% setelement(1,{foo,bar},qux).
%% => {qux,bar}


%% size([1,2,3]). ==> this errors, only works on a tuple or a binary
%% size(<<"three">>). => 5
%% size({three}).  => 1


receivingFunction() ->
    receive
        foo ->
            io:format("received foo message~n"),
            {ok,foo};
        _ ->
            io:format("received unknown message~n"),
            {ok,unknown}
        after 10000 ->
                io:format("ok, nothing received, timed out."),
                {error,timed_out}
    end.

%% Pid = spawn(fun () -> erlp:receivingFunction() end).

%%
%% Pid = spawn( erlp, receivingFunction, [] ).

%% Pid = spawn_link( erlp, receivingFunction, [] ).


%% check if there is anything awaiting in the repl's mailbox...
%% receive
%%     foo -> {ok,foo};
%%     _ -> {ok,unknown}
%%     after 10000 -> {error,timed_out}
%%     end.


%% erlang:system_info(c_compiler_used).
%% => {gnuc,{4,4,1}}

%% erlang:system_info(cpu_topology).
%% => [{processor,{logical,0}}]

%% erlang:system_info(machine).
%% => "BEAM"

%% erlang:system_info(otp_release).
%% => "R13B01"

%% term_to_binary({foo,"bar",3}).
%% => <<131,104,3,100,0,3,102,111,111,107,0,3,98,97,114,97,3>>

%% binary_to_term(<<131,104,3,100,0,3,102,111,111,107,0,3,98,97,114,97,3>>).
%% => {foo,"bar",3}


%% term_to_binary({foo,"bar",3},[compressed]).
%% => <<131,104,3,100,0,3,102,111,111,107,0,3,98,97,114,97,3>>


%% tl([1,2,3]).
%% => [2,3].


%% integer truncation (not rounding)
%% trunc(6.7).
%% => 6

%% tuple_size({a,b,c,{1,2,3,34}}).
%% => 4

%% tuple_to_list({a,b,c,{1,2,3,34}}).
%% =>  [a,b,c,{1,2,3,34}]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% file module

%% NB: this doesn't work on this module (erlp.erl), only on a file of terms
%% file:consult("terms.txt").
%% => {ok,[{person,"Kyle"},{person,"Sydney"}]}

%% file:read_file_info("terms.txt").
%% {ok,{file_info,36,regular,read_write,
%%                {{2010,1,17},{22,27,36}},
%%                {{2010,1,17},{22,27,30}},
%%                {{2010,1,17},{22,27,30}},
%%                33188,1,2049,0,4473652,1000,1000}}


%% file:get_cwd().
%% => {ok,"/home/mortis/personal/projects/sandbox/examples/erlang"}

%% file:list_dir(".").
%% => {ok,["terms.txt",".#erlp.erl","terms.txt~","erlp_ch1.erl",
%%        "boolean.erl","erlp.erl","#erlp.erl#","erlp.beam"]}

%% {ok, FileHandle } = file:open("terms.txt", [read]).

%% file:pid2name(FileHandle).
%% => {ok,"terms.txt"}

%% file:position(FileHandle,cur).
%% => {ok,0}
%% file:position(FileHandle,bof).
%% => {ok,0}
%% file:position(FileHandle,eof).
%% => {ok,36}

%% file:read(FileHandle,1024*1024).
%% => eof

%% file:read_file("terms.txt").
%% {ok,<<"{person,\"Kyle\"}.\n{person,\"Sydney\"}.\n">>}
%% could then do something like binary_to_list to turn it into a string...

%% must be deprecatd :(, wonder where the equivalent is now...
%% file:read_line(FileHandle).

%% read_line, readline: the second argument to io:get_line here is a 'prompt'
%% f().
%% {ok, FileHandle } = file:open("terms.txt", [read]).
%% io:get_line(FileHandle,"").

%% http://www.erlang.org/doc/man/io.html
%% the io manpage lists all of the formatting options for io:format, in great detail

%% where are the IoDevices for stdin, stdout and stderr for the erl
%% process?  can you get to them?

%% yes, you can refer to them with the atom `standard_io', and
%% `standard_error' -- standard_io must referr to both stdin and
%% stdout?

%% chdir,cd, change directory
%% file:set_cwd(directory).


%% flush, fflush, flush buffers, synchronizing to disk
%% file:sync(IoDevice).

%% write data to a file...
%% file:write(IoDevice,Bytes).
%% file:write_file(IoDevice,Bytes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% filename module

%% filename:absname("terms.txt").
%% => "/home/mortis/personal/projects/sandbox/examples/erlang/terms.txt"
%% NB: it does translation based on CWD, so the file might not exist!

%% filename:basename("terms.txt").
%% => "terms.txt"

%% filename:basename(filename:absname("terms.txt")).
%% => "terms.txt"



%% filename:basename("terms.txt",".txt").
%% => "terms"


%% filename:dirname(filename:absname("terms.txt")).
%% => "/home/mortis/personal/projects/sandbox/examples/erlang"

%% filename:extension("terms.txt").
%% => ".txt"

%% filename:join(["usr","local","share","dict","words.txt"]).
%% => "usr/local/share/dict/words.txt"

%% filename:join(["/usr","/local","/share","/dict","/words.txt"]).
%% => "/words.txt"

%% filename:join(["/usr","local","share","dict","words.txt"]).
%% => "/usr/local/share/dict/words.txt"


%% filename:split(filename:join(["/usr","local","share","dict","words.txt"])).
%% ["/","usr","local","share","dict","words.txt"]

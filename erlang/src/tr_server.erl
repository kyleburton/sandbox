-module(tr_server).
-behaviour(gen_server).

-export([
    start_link/1,
    start_link/0,
    get_count/0,
    stop/0
  ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER,?MODULE).
-define(DEFAULT_PORT,1055).
-record(state,{port,lsock,request_count=0}).

start_link(Port) ->
  gen_server:start_link({local,?SERVER},?MODULE,[Port],[]).

start_link() ->
  start_link(?DEFAULT_PORT).

get_count() ->
  gen_server:call(?SERVER,get_count).

stop() ->
  gen_server:cast(?SERVER,stop).


init([Port]) ->
  {ok,LSock} = gen_tcp:listen(Port,[{active,true}]),
  {ok, #state{port=Port, lsock=LSock}, 0}.

handle_call(get_count, _From, State) ->
  io:format("~p:~p:handle_call(get_count): State=~p;~n",
    [?MODULE,self(),state]),
  {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
  io:format("~p:~p:handle_cast(stop): State=~p~n",
    [?MODULE,self(),state]),
  {stop, ok, State}.


handle_info({tcp,Socket,RawData},State) ->
  RequestCount = State#state.request_count,
  io:format("~p:~p:handle_info(tcp): State=~p; RawData:~p~n",
    [?MODULE,self(),state,RawData]),
  try
    % MFA? Module, Function, Argumens?
    MFA = re:replace(RawData, "\r?\n$", "", [{return, list}]),
    io:format("~p:~p:handle_info(tcp): MFA:~p~n",
      [?MODULE,self(),MFA]),
    % re:run("lists:flatten(\"hello\",\"dolly\").", "([^:]+):([^:]+)\\((.+?)\\)\\.",[{capture,[1,2,3],list}, ungreedy]).  
    {match, [M, F, A]} = re:run(MFA, "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
      [{capture,[1,2,3],list}, ungreedy]),
    io:format("~p:~p:handle_info(tcp): M:~p; F:~p; A:~p~n",
      [?MODULE,self(),list_to_atom(M),list_to_atom(F),args_to_terms(A)]),
    Result = apply(list_to_atom(M), list_to_atom(F), args_to_terms(A)),
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
  catch
    _C:E ->
      gen_cp:send(Socket, io_lib:fwrite("~p~n", [E]))
  end,
  {noreply ,State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock=LSock} = State) ->
  io:format("~p:~p:handle_info(timeout): State=~p;~n",
    [?MODULE,self(),state]),
  {ok, _Sock} = gen_tcp:accept(LSock),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  io:format("~p:~p:code_change: State=~p;~n", [?MODULE,self(),state]),
  { ok, State }.

terminate(_Reason, _State) ->
  ok.

args_to_terms([]) ->
  [];
args_to_terms(RawArgs) ->
  { ok, Toks, _Line } = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
  { ok, Args }        = erl_parse:parse_term(Toks),
  Args.


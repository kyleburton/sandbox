%%%---------------------------------------------------------------------
%%% Created : 18 Aug 1998 by tobbe@tornkvist.org
%%% Desc.   : A simple SMTP client (RFC 821).
%%%---------------------------------------------------------------------
-module(smtp_client).

-export([async_deliver/3, async_deliver/4, deliver/3, deliver/4]).

-include_lib("kernel/include/inet.hrl").

-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
					 [?MODULE, ?LINE | Y])).

-record(sk, {
          %% mail from: <from>
          from,                 % Envelope 'From'
          %% rcpt to: <to>
          to,                   % Envelope 'To'
          smtp_host = "",       % SMTP host to be contacted
          sockfd,               % Socket filedesc.
          port=25,              % The SMTP server port number
          snoop=false           % Trace on/off
         }).

%%% --------------------------------------------------------------------
%%% Exported interface. 
%%% --------------------------------------------------------------------

%%% We don't care about weather the delivery 
%%% was successful or not.

async_deliver(From,To,Mail) ->
    spawn(fun() -> deliver(From,To,Mail) end).

async_deliver(From,To,Mail,Opts) ->
    spawn(fun() -> deliver(From,To,Mail,Opts) end).

%%% We return true if we successfully delivered 
%%% the mail, otherwise we return false.

deliver(From,To,Mail) ->
    deliver(From,To,Mail,[]).

deliver(From,To,Mail,Opts) ->
    Self = self(),
    Pid = spawn(fun() -> init(Self,From,To,Mail,Opts) end),
    receive {Pid,Answer} -> Answer end.

%%% --------------------------------------------------------------------
%%% Internal stuff
%%% --------------------------------------------------------------------

init(Pid,From,To,Mail,Opts) ->
    Answer = case catch doit(From,To,Mail,Opts) of
                 {'EXIT',Reason} -> log_error({crashed,Reason}),false;
                 {error,Reason}  -> log_error(Reason),false;
                 {ok,Result}     -> log_result(Result),true
             end,
    Pid ! {self(),Answer}.


%%% -----------------------
%%% Start the SMTP session.
%%% -----------------------

doit(From,To,Mail,Opts) ->
    S = init_session(From,To,Opts),
    connect(S, crlf(lists:flatten(Mail))).

%%%
%%% See also:  http://pobox.com/~djb/docs/smtplf.html
%%%
crlf([$\r,$\n|T]) -> [$\r,$\n | crlf(T)];
crlf([$\n|T])     -> [$\r,$\n | crlf(T)];
crlf([H|T])       -> [H | crlf(T)];
crlf([])          -> [].


%% Initiate the session key

init_session(From,To,Options) ->
    set_options(Options, #sk{from=From,to=To}).

set_options([{snoop,Flag}|T],S) ->
    set_options(T,S#sk{snoop=Flag});
set_options([{smtp_host,Host}|T],S) ->
    set_options(T,S#sk{smtp_host=Host});
set_options([{port,Port}|T],S) ->
    set_options(T,S#sk{port=Port});
set_options([X|_],_) ->
    throw({error,{unknown_option,X}});
set_options([],S) ->
    S.

%%% ----------------------
%%% Connect to SMTP server

connect(S,Mail) ->
    Opts = [{packet,raw},{reuseaddr,true},{active,false}],
    Host = smtp_host(S),
    %% io:format("connect S=~p Host=~p~n", [S,Host]),
    case gen_tcp:connect(Host,S#sk.port,Opts) of
        {ok,Sock} -> get_greeting(S#sk{sockfd=Sock},Mail);
        _         -> throw({error,{connect_failed,Host}})
    end.

smtp_host(S) when S#sk.smtp_host == "" ->
    %% Use loacl host
    {ok,HostName} = inet:gethostname(),
    HostName;
smtp_host(S) -> 
    S#sk.smtp_host.

%%% We are expecting to receive a 220 reply which we
%%% answer with a HELO identification, which in turn
%%% is expected to be replied with a 250 reply.

get_greeting(S,Mail) ->
    {Got,_} = expect(S,"220"),
    if_snoop(S,sender,"220" ++ Got),
    Msg = "HELO " ++ domain(S),
    send(S,Msg),
    if_snoop(S,client,Msg),
    {Got2,_} = expect(S,"250"),
    if_snoop(S,sender,"250" ++ Got2),
    mail_from(S,Mail).

domain(S) -> hd(tl(string:tokens(S#sk.from,"@"))).

mail_from(S,Mail) ->
    Msg = "MAIL FROM: <" ++ S#sk.from ++ ">",
    send(S,Msg),
    if_snoop(S,client,Msg),
    {Got,_} = expect(S,"250"),
    if_snoop(S,sender,"250" ++ Got),
    mail_to(S,Mail).

mail_to(S,Mail) ->
    mail_to_all(string:tokens(S#sk.to, " ,"),S,Mail).

mail_to_all([],S,Mail) -> data(S,Mail) ;
mail_to_all([To | Tos],S, Mail) ->
    Msg = "RCPT TO: <" ++ To ++ ">",
    send(S,Msg),
    if_snoop(S,client,Msg),
    {Got,_} = expect(S,"250"),
    if_snoop(S,sender,"250" ++ Got),
    mail_to_all(Tos,S,Mail).

data(S,Mail) ->
    Msg = "DATA",
    send(S,Msg),
    if_snoop(S,client,Msg),
    {Got,_} = expect(S,"354"),
    if_snoop(S,sender,"354" ++ Got),
    transfer_data(S,Mail).

transfer_data(S,Mail) ->
    send(S,byte_stuff_it(Mail)),
    send(S,"."),
    Msg = string:substr(Mail,1,20) ++ ".....",    
    if_snoop(S,client,Msg),
    {Got,_} = expect(S,"250"),
    if_snoop(S,sender,"250" ++ Got),
    quit(S).

quit(S) ->
    Msg = "QUIT",
    send(S,Msg),
    if_snoop(S,client,Msg),
    {Got,_} = expect(S,"221"),
    if_snoop(S,sender,"221" ++ Got),
    gen_tcp:close(S#sk.sockfd),
    {ok, finished}.

%%% -------------
%%% Misc routines
%%% -------------

%%% ---------------------
%%% Perform byte stuffing
    
byte_stuff_it([$\r,$\n,$.|Mail]) ->
    [$\r,$\n,$.,$.|byte_stuff_it(Mail)];
byte_stuff_it([H|T]) ->
    [H|byte_stuff_it(T)];
byte_stuff_it([]) ->
    [].

%%% -------------------------------------------------
%%% Receive a message. The beginning of the message
%%% must match the 'Expected' string. When the match
%%% is complete, return the rest of the message,
%%% otherwise abort the connection.

expect(S,Expected) ->
    expect(S,Expected,recv(S)).

expect(S,[H|T1],[H|T2]) -> expect(S,T1,T2);
expect(S,[],T)          -> recv_sl(S,T);
expect(S,T,[])          -> expect(S,T,recv(S));
expect(S,Exp,Got)       -> 
    ?elog("expect/3 aborting, Exp=~p , Got=~p~n", [Exp, Got]),
    abort(S).

abort(S) ->
    Msg = "QUIT",
    send(S,Msg),
    if_snoop(S,client,Msg),
    gen_tcp:close(S#sk.sockfd),
    throw({error,abort}).
    
%%% -----------------------------------------------------
%%% Receive a complete single-line (ended by a CRLF pair.
%%% Returns: {Single-Line, Continuation-Characters (Cc) }
%%% Where Cc is the characters next to be processed.

%recv_sl(S) ->
%    recv_sl(S,[]).

recv_sl(S,Cc) ->
    complete_sl(S,Cc,[]).

-define(CR, $\r).
-define(LF, $\n).

complete_sl(S,[?CR|T],Line) ->
    complete_sl_lf(S,T,[?CR|Line]);
complete_sl(S,[H|T],Line) ->
    complete_sl(S,T,[H|Line]);
complete_sl(S,[],Line) ->
    complete_sl(S,recv(S),Line).

complete_sl_lf(_,[?LF|T],Line) ->
    {lists:reverse([?LF|Line]),T};
complete_sl_lf(S,[_|T],Line) ->
    complete_sl(S,T,[?LF|Line]);
complete_sl_lf(S,[],Line) ->
    complete_sl_lf(S,recv(S),Line).

recv(S) ->
    case gen_tcp:recv(S#sk.sockfd,0) of
        {ok,Packet} -> Packet;
        Else        -> exit(Else)
    end.

%%% ---------------------------------------
%%% Print trace info if snoop option is set

if_snoop(S,Who,Msg) when S#sk.snoop==true ->
    ?elog("~s: ~s~n",[who(Who),Msg]);
if_snoop(_,_,_) ->
    true.

who(sender) -> "S";
who(client) -> "C".
%%% -----------------------------
%%% Send a CRLF terminated string

send(S,Msg) -> 
    gen_tcp:send(S#sk.sockfd, Msg ++ "\r\n").


%%% ------------
%%% Log handling

log_error({unknown_option,X}) ->
    ?elog("Unknown option ~p , aborting...~n",[X]);
log_error({connect_failed,Host}) ->
    ?elog("Couldn't connect to host ~s , aborting...~n", [Host]);
log_error(from_field) ->
    ?elog("Couldn't extract a from field !! ~n",[]);
log_error(abort) ->
    ?elog("Aborting... Connection closed !! ~n",[]);
log_error(domain) ->
    ?elog("Couldn't get domain name~n",[]);
log_error({crashed,X}) ->
    ?elog("<INTERNAL ERROR> ,reason was: ~p , aborting...~n",[X]).

log_result({finished,User,Host}) ->
    ?elog("Mail delivered to: ~s at: ~s~n",[User,Host]);
log_result(_) -> 
    ok.



-module(erlp).
-export([area/1,returnsTrue/0,returnsFalse/0,returnsIfTrue/1,returnsNotTrue/1,booleanTests/0,
        factorial/1]).


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

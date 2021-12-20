-module(regex_utils).
-export([split/3]).

splitc(String,Matcher,0,Acc) ->
    Acc;
splitc(String,Matcher,Limit,Acc) ->


split(String,Regex,Limit) ->
    { ok, Matcher } = re:compile(Regex),
    splitc(String,Matcher,Limit,[]);
split(String,Regex,Options,Limit) ->
    { ok, Matcher } = re:compile(Regex,Options),
    splitc(String,Matcher,Limit,[]).

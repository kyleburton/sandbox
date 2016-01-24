minOfList([Head|Tail], Min) :- minOfList(Tail, Head, Min).
minOfList([], Min, Min).
minOfList([Head|Tail], Min0, Min) :-
    min(Head, Min0, Min1),
    minOfList(Tail, Min1, Min).

mysort([],[]).
mysort([H],[H]).
## mysort([H|T],) :- minOfList(mysort(T),H).



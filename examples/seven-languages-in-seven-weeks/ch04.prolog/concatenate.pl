concatenate([], L2, L2).
%% concatenate([Head|[]], L2, [Head|L2]).
concatenate([Head|Tail1], List, [Head|Tail2]) :- concatenate(Tail1, List, Tail2).

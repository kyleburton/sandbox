count(0, []).
count(Count, [Head|Tail]) :- count(TailCount, Tail), Count is TailCount + 1.

sum(0, []).
sum(Total, [Head|Tail]) :- sum(TailSum, Tail), Total is TailSum + Head.

average(Avg, List) :- sum(Sum, List), count(Count, List), Avg is Sum / Count.

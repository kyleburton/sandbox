%% fibonacci list is: 0, 1, 1, 2, 3, 5, 8, 13, 21, ...
%% fibionacci is 1, 1, fib(n-1)+fib(n-1)

%% http://en.literateprograms.org/Fibonacci_numbers_(Prolog)

%% NB: this is is a 'naieve' non-tail recrusive implementation, but
%% easier to understand at first:

%% fib(0, 0).
%% fib(1, 1).
%% fib(N, NF) :-
%%     A is N - 1, B is N - 2,
%%     fib(A, AF),
%%     fib(B, BF),
%%     NF is AF + BF.

%% this is a tail recursive version
fib(0, A, _, A).

fib(N, A, B, F) :-
    N1 is N - 1,
    Sum is A + B,
    fib(N1, B, Sum, F).
fib(N, F) :-
    fib(N, 0, 1, F).

%% fib(5, Res).
%% > fib(5, 0, 1, Res).
%% > fib(N1=4{5-1}, 1, 1{0+1}, Res).
%% > fib(N1=3{4-1}, 1, 2{1+1}, Res).
%% > fib(N1=2{3-1}, 2, 3{1+2}, Res).
%% > fib(N1=1{2-1}, 3, 5{2+3}, Res).
%% > fib(N1=0{1-1}, 5, 8{3+5}, Res). => Res, which is A, which is 5

%% NB: this overflows ... 
%% fib(100,A). 
%% => A = -874975239646953533 ? 

%% fact(N,F,Acc) :- fact(N-1,F2,Acc2), Acc2 = Acc*N.
%% fact(N,F) :-
%%     fact(N, 1, F).
%% fact(N,Acc,F) :- 
%%     N2 = N - 1,
%%     Acc2 = Acc * N,
%%     fact(N2,F2,F).

factorial(N, R) :- 
    factorial(N, 1, R).
factorial(0, R, R) :- 
    !.
factorial(N, Acc, R) :-
    NewN is N - 1,
    NewAcc is Acc * N,
    factorial(NewN, NewAcc, R).

%% real world community using prolog
%% =>


%% An implementation of the Towers of Hanoi. How does it work?

%% What are some of the problems of dealing with “not” expressions? Why
%% do you have to be careful with negation in Prolog?


%% • Reverse the elements of a list.
rev([],[]).
rev([Head],[Head]).
%%rev([First|[Second|[]]],[Second,First]).
rev([Head|Tail], Res) :- rev(Tail, RevTail), append(RevTail, [Head], Res).

%% • Find the smallest element of a list.
min(X,Y,Z) :-
    (X =< Y
     -> Z = X
     ;  Z = Y
    ).
     

%% my failed attempt
%% minOfList([],nil).
%% minOfList([Min],Min).
%% minOfList([X,Y],Min) :- min(X,Y,Min).
%% minOfList([X,[Y|Tail]],M) :- min(X,Y,M1), minOfList([M1|Tail],M).

minOfList([Head|Tail], Min) :- minOfList(Tail, Head, Min).
minOfList([], Min, Min).
minOfList([Head|Tail], Min0, Min) :-
    min(Head, Min0, Min1),
    minOfList(Tail, Min1, Min).

filterForLessThan(_,[],[]).
filterForLessThan(N,[H],[]) :- H < N.
filterForLessThan(N,[H],[]) :- H >= N.
appendIfLessThan(N,V,[],[]) :- V >= N.
appendIfLessThan(N,V,[],[V]) :- V < N.
appendIfLessThan(N,V,L,[V|L]) :- V < N.
appendIfLessThan(N,V,L,L) :- V >= N.
%% filterForLessThan(N,[H|T],Res) :- 
%%     filterForLessThan(N,T,Res2),
%%     .

%% • Sort the elements of a list.
%% http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html
%% http://www.cp.eng.chula.ac.th/~piak/teaching/dsys/2004/quick-prolog.htm
pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G) :-
    X=<H,
    pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]) :-
    X>H,
    pivoting(H,T,L,G).

quick_sort2(List,Sorted) :-
    q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
    pivoting(H,T,L1,L2),
    q_sort(L1,Acc,Sorted1),
    q_sort(L2,[H|Sorted1],Sorted).

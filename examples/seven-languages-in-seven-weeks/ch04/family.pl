father(zeb,         john_boy_sr).
father(john_boy_sr, john_boy_jr).

/* ancestor(X, Y) :- father(X, Y); mother(X, Y).  */
ancestor(X, Y) :- father(X, Y).
ancestor(X, Y) :- father(X, Z), ancestor(Z, Y).

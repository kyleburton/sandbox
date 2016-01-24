cat(lion).
cat(tiger).

dorothy(X, Y, Z) :- X = lion, Y = tiger, Z = bear.

twin_cats(X, Y) :- cat(X), cat(Y).


/* try: dorothy(lion, tiger, bear). */
/* try: dorothy(One, Two, Three).   */
/* try: twin_cats(A, B).            */



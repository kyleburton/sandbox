likes(wallace, cheese).
likes(grommit, cheese).
likes(wendolene, sheep).

/* NB: \+ is logical negation */
/* This is a rule, named friend\/2 
   it has 3 subgoals
 */
friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y, Z).

/* NB: apparently you can't print from --consult-file like this
   print('friend(wallace, grommit).').
   print('friend(wallace, wendolene).').
*/

/* TODO: when we use gprolog --consult-file, how do we have these two statements produce output? */
friend(wallace, grommit).
friend(wallace, wendolene).

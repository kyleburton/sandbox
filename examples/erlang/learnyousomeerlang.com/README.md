# Overview

Repository for examples and scratchpad code for working through: http://learnyousomeerlang.com/

Current Location: http://learnyousomeerlang.com/errors-and-exceptions

Return to:
* http://learnyousomeerlang.com/types-or-lack-thereof -- For Type Junkies

Notes:

```
Note: the boolean operators and and or will always evaluate arguments on both sides of the operator. If you want to have the short-circuit operators (which will only evaluate the right-side argument if it needs to), use andalso and orelse.
```

```
Equality:          =:=
Inequality:        =/=
Pseudo Equality:   ==
Pseudo Inequality: /=
```

```
% true and false are not booleans, they're just atoms in Erlang, though widely used for that specific purpose.
% NB: they can (for some reason) be used in arithematic comparisions:
14> 1 < false_or_anything_else.
true
15> 1 == false_or_anything_else.
false
16> 1 /= false_or_anything_else.
true
```

```
[{X,temp:f2c({farenheight,X})} || X <- [-40,0,32,75,98.6,100,212]].

```


Resources:

* http://learnyousomeerlang.com/
* http://stefanalfbo.github.io/blog/2013/04/23/erlang-shell-cheat-sheet/

TODO: unicode / utf-8 in erlang?
TODO[explore the standard library] gb\_trees
TODO[explore the standard library] gb\_sets
TODO[explore the standard library] arrays
TODO[explore the standard library] calendar - is there a JodaTime or clj-time for Erlang?
TODO[explore the standard library] dict
TODO[explore the standard library] proplists
TODO[explore the standard library] filename
TODO[explore the standard library] filelib
TODO[explore the standard library] maps
TODO[explore the standard library] math
TODO[explore the standard library] rand
TODO[explore the standard library] random
TODO[explore the standard library] queue
TODO[explore the standard library] string
TODO[explore the standard library] zip
TODO: sudoku using processes: one process per cell to track state
TODO: using the binary format + destructuring, pull png, jpeg and gif header info & print it.
TODO: example connecting to an RDBMS (Postgres)
TODO: implement a memoize function (in memory, into a process?, to disk)
TODO: make HTTP requests, eg: call into the AWS API
TODO: work with JSON
TODO: work with XML
TODO: create REST endpoints
TODO: create a single page app w/an Erlang or Elixir REST backend
TODO: work with web-sockets
TODO: is there a challens asbstraction in Erlang?  in-memory Queues?
TODO: mnesia, ets+dets
TODO: How well supported is Erlang across AWS?  (eg: AWSLambda?)

DONE: vim support: vim-erlang-compiler, vim-erlang-omnicomplete, vim-erlang-runtime, vim-erlang-tags erlang-motions.vim
DONE: installed Erlang via apt / brew
DONE: simple math: temperature conversions
DONE: immutable linked list using tuples

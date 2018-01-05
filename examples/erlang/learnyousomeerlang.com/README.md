# Overview

Repository for examples and scratchpad code for working through: http://learnyousomeerlang.com/

Current Location: http://learnyousomeerlang.com/modules

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

TODO: sudoku using processes: one process per cell to track state
TODO: example connecting to an RDBMS (Postgres)
TODO: implement a memoize function (in memory, into a process?, to disk)
TODO: make HTTP requests, eg: call into the AWS API
TODO: work with JSON
TODO: work with XML
TODO: create REST endpoints
TODO: create a single page app w/an Erlang or Elixir REST backend
TODO: work with web-sockets

DONE: vim support: vim-erlang-compiler, vim-erlang-omnicomplete, vim-erlang-runtime, vim-erlang-tags erlang-motions.vim
DONE: installed Erlang via apt / brew

# Overview

Inspried by, draws heavily from: [makengo](https://github.com/remogatto/makengo). Which does not seem to be comptabile with Go 1.1.

Rake-like build scripting example using Go with concurrent execution of task dependencies.


Run:

    GOPATH="$GOPATH:$(pwd)" go run grake.go

# TODO

* documentation, documentation, documentation
* take list of tasks to execute from command line arguments
* support task argument processing: "task1[arg1,arg2]"
* support 'once only' execution of tasks
* support re-enabling and re-execution of tasks
* turn into a library, make the construction of and execution of a 'Grakefile' as simple as possible and 'one step'

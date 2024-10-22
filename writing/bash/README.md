# Overview

Why bash? Why not another shell?

It's 2024, [`bash`](https://en.wikipedia.org/wiki/Bash_%28Unix_shell%29) has been around since 1989 (the year I graduated High-school ...), and is based off of the [`bourne shell`](https://en.wikipedia.org/wiki/Bourne_shell) which was created even earlier in 1979.  Bash has a reputation of being old, terse, confusing, bloated, difficult to use and not a real programming language (which arguably it is not).  There are certainly more modern, [better](https://en.wikipedia.org/wiki/Z_shell), [friendlier](https://en.wikipedia.org/wiki/Fish_(Unix_shell)) and [less bloated](https://en.wikipedia.org/wiki/Almquist_shell) shells, so why bash?

The [Lindy effect](https://en.wikipedia.org/wiki/Lindy_effect) proposes "the longer a period something has survived to exist or be used in the present, the longer its remaining life expectancy. Longevity implies a resistance to change, obsolescence, or competition, and greater odds of continued existence into the future".  [GitHub's Language Rankings](https://madnight.github.io/githut/#/pull_requests/2024/1) put "Shell" at 12th position, ahead of arguably "real" programming languages like Rust (which I am confident will usurp Shell and make its way up the rankings), and Swift.  My personal experience with bash began around 1995 when I got Linux up and running on a PC in my apartment.  It has remained one of the most constant and enduring software engineering tools I've used.  The primary programming language I use has changed over time as I've moved between companies and between teams and projects.  Most of my work has targeted Linux systems, now containers.  Those environments often have bash installed and often use it for control and automation purposes.  CI/CD systems are often similar: driven by tools like Make and combinations command line tools coordinated via bash.  Development environments follow a similar pattern, using the same core tool-sets and utilities that are ultimately leveraged in CI/CD.  This is more often than not a shell and that shell is more often than not bash.  Having a good working knowledge of bash has been valuable to myself and my teams, using it as my standard shell means I remain practiced, knowledgeable and up to date.  The GNU Utilities (eg: grep, sort) are highly optimized and [can be faster than using a database or rolling your own solution](https://github.com/kyleburton/large-data-and-clojure/blob/master/large-data.pdf).

I have noticed more and more that software engineers I work with find bash to be opaque and not the easiest to get started with or learn.  As I've shared my own experiences with them I have noticed a few common topics and patterns that have helped them improve, these posts are my attempt at capturing and summarizing those learning's.

# Posts

* [Fundamental's, Warts and Oddities: String Substitution](20241020-fundamental-warts-and-oddities-stringsub.md)
* [Co-opting Patterns from other languages: Case Statements](20241019-patterns-from-other-langs.md)



# Future Post Ideas
```
TODO: best practices, shellcheck, personal idioms
      * set -eEuo pipefail, every time <- this + shellcheck assuaged a significant amount of "bash is terrible"
      * use a 'main'
      * function arguments: use shift, pass along argv "$@" and "$*"
      * use defaults "${foo:-default}"
TODO: test, `[` and `[[`, crucially: how to get help
TODO: my most common text processing utilities and patterns
TODO: Co-opting patterns from other langs: annotations
TODO: Flags and options, easier than you might think
TODO: bash arrays, how to "accumulate" command line args to pass to another command
TODO: How to implement your own custom command line completion
TODO: re-use: logging library
TODO: trapping EXIT - https://www.putorius.net/using-trap-to-exit-bash-scripts-cleanly.html
TODO: foreground, background, multi-processing and wait
```

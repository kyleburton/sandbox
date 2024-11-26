# Overview

Why bash? Why not another shell?

It's 2024, [`bash`](https://en.wikipedia.org/wiki/Bash_%28Unix_shell%29) has been around since 1989 (the year I graduated High-school ...), and is based off of the [`bourne shell`](https://en.wikipedia.org/wiki/Bourne_shell) which was created even earlier in 1979.  Bash has a reputation of being old, terse, confusing, bloated, difficult to use and not a real programming language (which arguably it is not).  There are certainly more modern, [better](https://en.wikipedia.org/wiki/Z_shell), [friendlier](https://en.wikipedia.org/wiki/Fish_(Unix_shell)) and [less bloated](https://en.wikipedia.org/wiki/Almquist_shell) shells, so why bash?

The [Lindy effect](https://en.wikipedia.org/wiki/Lindy_effect) proposes "the longer a period something has survived to exist or be used in the present, the longer its remaining life expectancy. Longevity implies a resistance to change, obsolescence, or competition, and greater odds of continued existence into the future".  [GitHub's Language Rankings](https://madnight.github.io/githut/#/pull_requests/2024/1) put "Shell" at 12th position, ahead of arguably "real" programming languages like Rust (which I am confident will usurp Shell and make its way up the rankings), and Swift.  My personal experience with bash began around 1995 when I got Linux up and running on a PC in my apartment.  It has remained one of the most constant and enduring software engineering tools I've used.  The primary programming language I use has changed over time as I've moved between companies and between teams and projects.  Most of my work has targeted Linux systems, now containers.  Those environments often have bash installed and often use it for control and automation purposes.  CI/CD systems are often similar: driven by tools like Make and combinations command line tools coordinated via bash.  Development environments follow a similar pattern, using the same core tool-sets and utilities that are ultimately leveraged in CI/CD.  This is more often than not a shell and that shell is more often than not bash.  Having a good working knowledge of bash has been valuable to myself and my teams, using it as my standard shell means I remain practiced, knowledgeable and up to date.  The GNU Utilities (eg: grep, sort) are highly optimized and [can be faster than using a database or rolling your own solution](https://github.com/kyleburton/large-data-and-clojure/blob/master/large-data.pdf).

I have noticed more and more that software engineers I work with find bash to be opaque and not the easiest to get started with or learn.  As I've shared my own experiences with them I have noticed a few common topics and patterns that have helped them improve, these posts are my attempt at capturing and summarizing those learning's.

# Posts

* [Fundamentals, my Idioms and Some Best Practices](202410270-idioms-and-practices.md)
* [Fundamentals, Warts and Oddities: String Substitution](20241020-fundamental-warts-and-oddities-stringsub.md)
* [Co-opting Patterns from other languages: Case Statements](20241019-patterns-from-other-langs.md)

# Resources & References

These are resources that I used to learn or that I have found to be inspiring:

* [Julia Evans "Wizard Zines" - Bite Size Bash](https://wizardzines.com/zines/bite-size-bash/), and [a preview](https://wizardzines.com/comics/environment-variables/).  All of Julia's writing is excellent and all of the 'zines are great introductions to Unix and Linux.
* [Linux Documentation Project - Bash Beginner's Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/Bash-Beginners-Guide.html)
* [O'Reilly's Learning the Bash shell](https://www.oreilly.com/library/view/learning-the-bash/0596009658/)
* [O'Reilly's Linux in a Nutshell](https://www.oreilly.com/library/view/linux-in-a/9780596806088/)
* [10 Must Have Books for System Administrators](https://www.adminschoice.com/10-must-have-oreilly-books-for-system-administrators)
* [O'Reilly's Bash Cookbook](https://www.oreilly.com/library/view/bash-cookbook-2nd/9781491975329/) by JP Vossen (who I've had the pleasure of meeting in person)

# Future Post Ideas

* [test, `[` and `[[`, crucially: how to get help](20241029-test-and-how-to-get-help.md)
* my most common text processing utilities and patterns
* logical forms, loops (for, while, read)
* Co-opting patterns from other langs: annotations
* [Flags and options, easier than you might think](20241029-flags-and-options.md)
* bash arrays, how to "accumulate" command line args to pass to another command
* How to implement your own custom command line completion
* re-use: logging library
* trapping EXIT - https://www.putorius.net/using-trap-to-exit-bash-scripts-cleanly.html
* foreground, background, multi-processing and wait
* congruency between SQL and command line utilities
* history hacks: staying in place, the '#' trick for composing commands
* the '#' trick for pasting possibly dangerous commands
* find, grep, xargs for manipulating sets of files
* cooked, uncooked, clear and reset (blocking vs non-blocking stdin!)
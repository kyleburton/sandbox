While shell scripting with can be a gentle ramp up from just using the command line (you can literally take what you type at the command line and put it into a `*.sh` file as the starting basis for your scripts), there are a few practices that I've found to eliminate or reduce surprises, gotchas and save me from having to spend time debugging.

The first is adding the common [shebang](https://www.tutorialspoint.com/using-shebang-hash-in-linux-scripts). at the top of your script:

## The "Shebang"

```
#!/bin/bash
```

Doing this is helpful in a few ways:

* on a Unix-like system, if you make your script executable (`chmod +x`), it sets the interpreter used to execute the script
* it will inform your IDE or editor that it is a shell script (on Unix-like systems, while file extensions often match a file's type, the [first few bytes of the file](https://en.wikipedia.org/wiki/File_(command)) are also used to define the file type, see `man file`)
* this tells [`shellcheck`](https://github.com/koalaman/shellcheck) what version of the shell your script is (i.e. `bash` or the [POSIX bourne shell](https://en.wikipedia.org/wiki/Bourne_shell) `sh`).

I cannot stress how valuable [`shellcheck`](https://github.com/koalaman/shellcheck) is, it will catch innumerable bugs and enforce best practices as you are learning - it will accelerate your learning.  Vidar Holen (aka `koalaman`, `shellcheck`'s creator, chose consistent and googlable error codes that make it much easier to learn about the warnings in your scripts and fix them.

## Enable Error checking

The shell ostensibly has two use cases: one as an interactive shell; the other as a scripting tool.  There are behaviors and settings that make one easier and the other harder.  `bash` uses `set` to change these shell options, the ones I use in every script I write are:

```bash
set -e               Exit immediately if a command exits with a non-zero status.
set -E               If set, the ERR trap is inherited by shell functions.
set -u               Treat unset variables as an error when substituting.
set -o pipefail      the return value of a pipeline is the status of
                     the last command to exit with a non-zero status,
                     or zero if no command exited with a non-zero status
```

You can enable all of these in a single line, which brings me to the opening stanza of 99% of all of my shell scripts:

```bash
#!/bin/bash
set -eEuo pipefail
```

### `set -e`

This enables a "strict" mode for your scripts, they will terminate at the first command that fails (the opposite of what most folks want from their interactive terminal).  This is similar to ow a language like Java will use a thrown exception to abort a function call or program.  If you are writing a script where you have a command that is ok to have fail, there are a few approaches to support it:

```bash
# an example command that could fail
rm ./some-file.o

# test if the file exists _before_ attempting to delete it, note that these are equivalent
[[ -e ./some-file.o ]] && rm ./some-file.o
test -e ./some-file.o  && rm ./some-file.o

# test if the file exists _before_ attempting to delete it, using an "if/then" statement
if [[ -e ./some-file.o ]] ; then
    rm ./some-file.o
fi

# allow the command to fail with a logical OR followed by a command you know will succeed,
# eg: `true`
rm ./some-file.o || true

# a single colon is the "null" command, it does nothing and always succeeds, often
# used as a shorthand and equivalent to `true`
rm ./some-file.o || :

# while overly verbose for simple commands, you can use a full "if/then/else" statement
if rm ./some-file.o; then
    echo "ok: removed ./some-file.o"
else
    echo "ignoring error: failed to remove ./some-file.o"
fi

```

### `set -u`

This sets a second type of "strict" mode, where the use of a variable before it has been declared is an error.  This has often saved me time debugging when I typo variable names in my scripts.

```bash
#!/bin/bash
set -eEuo pipefail

objectfile="./some-file.o"

rm "$objcetfile"  # <-- will fail before running rm, which would have otherwise been passed an empty string
```

### `set -E`

This causes the `ERR` trap to be inherited by shell functions.  Using an `ERR` trap is a method for error handling in the shell (you can think of it as catching an exception).  We'll get to what the `ERR` trap is and some ways you may want to use it in a subsequent post, for now please humor me and use this.

## `set -o pipefail`

`pipefail` extends the strict error checking to each part of a shell pipeline.  [`pipeline`s](https://www.gnu.org/software/bash/manual/html_node/Pipelines.html) allow you to chain commands together, sending the output of one into the input of another.  As an example, lets say we want to find all the object files under the current directory, filter away any that have `/build-cache/` in their name and then remove what we found, we could imagine the following:

```bash
#!/bin/bash
set -eEu
find ./build-dir -name "*.o" | grep "/build-cache/" | xargs rm
```

Without the `set -o pipefail` (though with `set -e`) this would only fail if the `rm` failed.  With `pipefail`, this will terminate if the `find` fails (which could be due to `./build-dir` not existing, or no files found), it will terminate if the `grep` fails (no input lines were matched), or if the `rm` fails (because no files were passed).  `set -o pipefail` has helped me avoid having to debug pipelines in my scripts.

## use a `main`

While I will "upgrade" my scripts to a more capable programming language when I feel the time is right, I follow the same convention in my shell scripts by using functions and explicitly a main:

```bash
#!/bin/bash
set -eEuo pipefail

cmd-api () {
    # do something with the API ...
}

show-help () {
    echo "$0 api - make api calls ..."
}

main () {
  local cmd
  cmd="${1:-}"

  case "$cmd" in
    api) shift;  cmd-api   "$@" ;;
    *)           show-help "$@" ;;
  esac
}

main "$@"
```

Briefly, you set a bash variable by assigning to it by name, eg: `cmd="$1"`, while you use a dollar sign in front of the variable to get it's value: `$cmd`.  Bash also supports "dollar curly-braces" for getting the value: `${cmd}` which is slightly more verbose though supports other useful features like default values, removing prefixes and suffixes.  It also allows you correctly concatenate: `X$cmdX` vs `X${cmd}X`, the former likely being an error (because it uses the variable `cmdX` vs `cmd`, the latter being explicit.

There are few new things here, first is defaulting an empty or unset variable: `"${1:-}"` uses the "birdface" to provide a default (I think I first heard `:-` called a "bird face" from [Jessitron](https://jessitron.com/2015/02/10/fun-with-optional-typing-cheap-mocking/)).  The variable in this case is `$1`, the first argument to the `main` function, what follows after the `:-` is the default, in this case the empty string.

This syntax `${1:-}` is very useful when using the `set -u` option, allowing you to handle unset variables by allowing them to be an empty string (vs them being an error).

[`shift`](https://www.gnu.org/software/bash/manual/bash.html#index-shift) is the next new idea here.  In shell scripts a common idiom is to process some arguments in one function and then pass the remainder to another function or command.  Bash uses `$1`, `$2`, `$3` and so on, for the arguments to your script or to your functions.  `shift` "pops" the first argument off, shifting `$2` into `$1`, `$3` into `$2` and so on.  This allows the script above to shift off the command `api` (in the first clause of the case statement) and then pass the remaining arguments `$@` to the `cmd-api` function.

`$@` is related to `$*`, both of which represent the arguments passed to a script or function, and are explained well in [this Unix& Linux Stackexchange post](https://unix.stackexchange.com/questions/129072/whats-the-difference-between-and).

`case` is a `bash` syntax for pattern matching an argument against [glob patterns](https://tldp.org/LDP/abs/html/globbingref.html).  glob patterns are often used for matching  strings and files at the command line, eg: `ls *.txt` is a glob for matching all file names in the current directory that end with a `.txt` suffix.  Each entry in a case statement is terminated with a `;;`.  In the example above, I have separated multiple commands on the same line with a `;`, you can do this anywhere in your scripts or even in your terminal, for example:

```bash
> ls -ltrh; date
```

Prints the current directory in long format (`-l`), sorted by time (`-t`), reversed (`-r`), with files sizes in "human readable" format (`-h`) and also prints the current datetime.

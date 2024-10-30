If you haven't encountered `[`, `[[` or `test` yet, well, now you have :)

`[` and `test` are actually one and the same, with `[` acting as a shorthand for `test`.  They are used to evaluate logical expressions and interact with files.

To start, you can get help with `[` and `test` through the `man` utlity:

```bash
man [
# or
man test
```

While [`test` has been around for a long time](https://en.wikipedia.org/wiki/Test_(Unix)), it been [improved upon with `[[`](https://stackoverflow.com/questions/3427872/whats-the-difference-between-and-in-bash).

While I won't go through every feature, I will show examples of how I commonly use it.

```bash

# if a file does not exist '-e', download it
[[ -e "./bake" ]] || curl -o "./bake" "https://stackoverflow.com/questions/3427872/whats-the-difference-between-and-in-bash"

# test if a variable is an empty string
if [[ -z "${1:-}" ]]; then
    echo "Error: you must pass an argument"
    return 1
fi

# check if an argument starts with `--` (looks like a command line parameter)
while [[ "${1:-}" == --* ]]; do
  case "$1" in
    --verbose)  shift; VERBOSE=yes          ;;
    --input=*)  shift; INPUT_FILE="${1#*=}" ;;
    --format=*) shift; FORMAT="${1#*=}"     ;;
    *)
      echo "Error: unrecognized option: '$1'"
      return 1
  esac
done
```

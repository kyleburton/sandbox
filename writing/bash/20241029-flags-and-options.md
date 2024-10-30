Most programming languages offer libraries for command line option parsing (including [`getopts`](https://en.wikipedia.org/wiki/Getopts) for bash), you can use `[[` and it's pattern matching combined with variable substitution to recognize and parse options:

```bash
#!/bin/bash
# flags.sh
set -Eeuo pipefail

VERBOSE=""
INPUT_FILE=""
OVERWRITE="no"

show-help () {
    cat<<END
flags.sh

--verbose          be verbose
--input=<file>     set the input file name
--format=<format>  set the output format
--help             show this help text
END
}

log () {
    [[ "$VERBOSE" == "yes" ]] && echo "$*"
}

main () {
    while [[ "${1:-}" == --* ]]; do
        case "$1" in
            --verbose)   shift; VERBOSE=yes          ;;
            --input=*)   shift; INPUT_FILE="${1#*=}" ;;
            --overwrite) shift; OVERWRITE="yes"      ;;
            --help)      shift; show-help            ;;
            *)
                echo "Error: unrecognized option: '$1'"
                return 1
        esac
    done

    if [[ -z "$INPUT_FILE" ]]; then
        echo "Error: you must supply an input file with --input=<file>"
        return 1
    fi

    if [[ -e "$INPUT_FILE" && "$OVERWRITE" != "yes" ]]; then
        echo "Error: input file '$INPUT_FILE' exists and --overwrite was not passed"
        return 1
    fi

    log "processing input file: $INPUT_FILE"
}

main "$@"
```

```bash
bash flags.sh

```

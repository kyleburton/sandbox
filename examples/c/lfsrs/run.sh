set -e
set -u
set -x

trap "stty sane" INT TERM EXIT

F="$1"
shift

if [ -e $F ]; then
  SRC=$F
  F=$(basename $F .c)
else
  SRC=$F.c
fi

function compile () {
  gcc -O2 -Werror -Wall -o $2 $1
}

compile $SRC $F
stty raw
./$F "$@"
rm $F

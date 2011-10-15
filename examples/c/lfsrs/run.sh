set -e
set -x

trap "stty sane" INT TERM EXIT

gcc -O2 -Wall snd.c
stty raw
./a.out


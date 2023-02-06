#!/usr/bin/env bash

cat > gcov.makefile <<EOF
P=dict_test
objects=keyval.o dict.o
CFLAGS=-g \
-std=gnu11 \
-O0 \
-fprofile-arcs \
-ftest-coverage \
-v \
\$(shell /usr/bin/pkg-config --cflags gsl) \
\$(shell /usr/bin/pkg-config --cflags glib-2.0) \
-I \$(HOME)/code/github.com/b-k/21st-Century-Examples
LDLIBS=\$(shell /usr/bin/pkg-config --libs glib-2.0)
CC=gcc

\$(P):\$(objects)

dict.c:
	cp \$(HOME)/code/github.com/b-k/21st-Century-Examples/dict.c ./

dict_test.c:
	cp \$(HOME)/code/github.com/b-k/21st-Century-Examples/dict_test.c ./

EOF

make -f gcov.makefile
./dict_test
for i in *gcda; do gcov $i; done
grep -C3 '####' ./*.c.gcov

SRCS=gsl_erf.c
OBJECTS=$(subst .c,.o,$(SRCS))
# https://embeddedartistry.com/blog/2017/06/07/warnings-weverything-and-the-kitchen-sink/
# -Wall
# -Werror
# -Wpedantic
# -Weverything
# -Wunused-result
CFLAGS=-g -Weverything -std=gnu11 -v $(shell /usr/bin/pkg-config --cflags gsl)
LDLIBS=$(shell /usr/bin/pkg-config --libs gsl)
CC=/usr/bin/clang

.PHONY: all clean
# all=gsl_erf

# $@ full target name
# $* target file w/suffix removed
# $< name of dependency causing target to be rebuilt

gsl_erf: $(OBJECTS)

clean:
	$(RM) ./*.o ./gsl_erf

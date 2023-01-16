SRCS=gsl_erf.c
OBJECTS=$(subst .c,.o,$(SRCS))
CFLAGS=-g -Wall -Werror -std=gnu11 -v $(/usr/bin/pkg-config --cflags gsl)
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

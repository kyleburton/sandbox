MAIN=stddev_bugged
SRCS=${MAIN}.c
OBJECTS=$(subst .c,.o,$(SRCS))
CFLAGS=-g \
-Weverything \
-std=gnu11 \
-v \
$(shell /usr/bin/pkg-config --cflags gsl) \
$(shell /usr/bin/pkg-config --cflags glib-2.0) \
-I $(HOME)/code/github.com/b-k/21st-Century-Examples \
--system-header-prefix=glib # disable warnings for headers starting with 'glib'

LDLIBS=$(shell /usr/bin/pkg-config --libs gsl) $(shell /usr/bin/pkg-config --libs glib-2.0)
CC=/usr/bin/clang

.PHONY: all clean config
# all=gsl_erf

# $@ full target name
# $* target file w/suffix removed
# $< name of dependency causing target to be rebuilt

$(MAIN): $(OBJECTS)

$(MAIN).c:
	cp $(HOME)/code/github.com/b-k/21st-Century-Examples/stddev_bugged.c ./

clean:
	$(RM) ./*.o ./$(MAIN)

config:
	@echo MAIN=$(MAIN)
	@echo SRCS=$(SRCS)
	@echo OBJECTS=$(OBJECTS)
	@echo CFLAGS=$(CFLAGS)
	@echo LDLIBS=$(LDLIBS)
	@echo CC=$(CC)

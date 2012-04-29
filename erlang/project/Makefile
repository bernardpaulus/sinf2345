SHELL = /bin/sh

ERLC=erlc
CFLAGS= -Wall -Werror

SRCS=$(wildcard *.erl)
OBJS=$(patsubst %.erl, %.beam, $(SRCS))


.PHONY: all
all: $(OBJS)

%.beam: %.erl
	$(ERLC) $(CFLAGS) $+

.PHONY: debug
debug: CFLAGS += +debug_info
debug: $(OBJS)

.PHONY: clean
clean:
	rm *.beam

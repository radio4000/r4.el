EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L .
SOURCES = $(wildcard r4*.el)

.PHONY: all compile test lint clean

all: compile test

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)

test:
	$(BATCH) -L test -l test/r4-test.el -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) $(foreach f,$(SOURCES),--eval '(checkdoc-file "$(f)")')

clean:
	rm -f *.elc test/*.elc

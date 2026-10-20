EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L .
SOURCES = $(wildcard r4*.el)

.PHONY: all compile test lint clean

all: compile test

# Compile each file in a fresh Emacs, like package-vc, to catch missing requires.
compile:
	@for f in $(SOURCES); do \
	  $(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $$f || exit 1; \
	done

test:
	$(BATCH) -L test -l test/r4-test.el -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) $(foreach f,$(SOURCES),--eval '(checkdoc-file "$(f)")')

clean:
	rm -f *.elc test/*.elc

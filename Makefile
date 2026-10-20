EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L .
SOURCES = $(wildcard r4*.el)

.PHONY: all compile test lint site clean

all: compile test

# Compile each file in a fresh Emacs, like package-vc, to catch missing requires.
compile:
	@for f in $(SOURCES); do \
	  $(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $$f || exit 1; \
	done

TESTS = $(wildcard test/r4-test-*.el)

test:
	$(BATCH) -L test $(foreach f,$(TESTS),-l $(f)) -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) $(foreach f,$(SOURCES),--eval '(checkdoc-file "$(f)")')

site:
	$(EMACS) -Q --batch -l publish.el

clean:
	rm -rf *.elc test/*.elc public

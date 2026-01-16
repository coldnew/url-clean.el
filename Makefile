.PHONY: test lint clean all

EMACS ?= emacs
LOAD_PATH := -L .

all: test

test:
	$(EMACS) --batch $(LOAD_PATH) \
		--eval "(require 'url-clean)" \
		-l url-clean-test.el \
		--eval "(ert-run-tests-batch-and-exit)"

clean:
	rm -f *.elc

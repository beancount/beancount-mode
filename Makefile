EMACS ?= emacs
SRC = beancount.el
TESTS = beancount-tests.el

compile: $(SRC)
	$(EMACS) -Q -batch -f batch-byte-compile $<

test:
	$(EMACS) -Q -batch -L . -l ert -l $(TESTS) -f ert-run-tests-batch-and-exit

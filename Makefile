EMACS ?= emacs

.PHONY: all compile test info clean

all: compile test info

compile:
	$(EMACS) --batch -Q -L . -f batch-byte-compile wordwise.el wordwise-common-words.el

test:
	$(EMACS) --batch -Q -L . -l wordwise-test.el -f ert-run-tests-batch-and-exit

# The built manual (wordwise.info) and its dir entry are committed so
# that package-vc checkouts and load-path installs get C-h R support
# without needing texinfo installed.
info: wordwise.info

wordwise.info: wordwise.texi
	makeinfo wordwise.texi
	install-info wordwise.info dir

clean:
	rm -f *.elc wordwise.info dir

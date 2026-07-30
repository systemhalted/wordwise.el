EMACS ?= emacs

.PHONY: all compile info clean

all: compile info

compile:
	$(EMACS) --batch -Q -L . -f batch-byte-compile wordwise.el wordwise-common-words.el

# The built manual (wordwise.info) and its dir entry are committed so
# that package-vc checkouts and load-path installs get C-h R support
# without needing texinfo installed.
info: wordwise.info

wordwise.info: wordwise.texi
	makeinfo wordwise.texi
	install-info wordwise.info dir

clean:
	rm -f *.elc wordwise.info dir

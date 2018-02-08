EMACS ?= emacs
CASK ?= cask

all: test

travis: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc


test: clean-elc
	${MAKE} all-tests
	${MAKE} compile
	${MAKE} all-tests
	${MAKE} clean-elc

all-tests:
	${MAKE} unit
	${MAKE} file-test

unit:
	${CASK} exec ert-runner test/cmake-ide-test.el test/utils-test.el

file-test:
	${CASK} exec ert-runner test/file-test.el

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile cmake-ide.el

clean-elc:
	rm -f *.elc

.PHONY:	all test unit

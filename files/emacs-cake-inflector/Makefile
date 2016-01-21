EMACS ?= emacs
CASK ?= cask
OUTPUT = /tmp/.el-expectations

all:
				${MAKE} clean
				${MAKE} test
				${MAKE} clean
test:
				${CASK} exec ${EMACS} -Q --batch	-L . -L t/extlib/ -l el-expectations -l cake-inflector -f batch-expectations ${OUTPUT} ./cake-inflector.el
				ret=$? 
				cat ${OUTPUT}
				exit ${ret}
clean:
				rm -f ${OUTPUT}

.PHONY: all test clean

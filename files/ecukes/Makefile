.PHONY : all test unit-test clean clean-elc ecukes

EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el reporters/*.el))
ELC = $(SRC:.el=.elc)
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
FEATURES = $(wildcard features/*.feature features/reporters/*.feature)

all: test

test: clean-elc
	$(MAKE) unit-test
	$(MAKE) ecukes
	$(MAKE) compile

unit-test: elpa
	$(CASK) exec ert-runner

elpa: $(PKG_DIR)
$(PKG_DIR): Cask
	$(CASK) install
	touch $@

compile: $(ELC)
%.elc: %.el
	@$(CASK) exec $(EMACS) -Q --script ecukes-byte-compile.el $<

clean: clean-elc
	rm -rf $(PKG_DIR)

clean-elc:
	rm -rf *.elc test/*.elc reporters/*.elc

ecukes: features/projects/super-project/.cask
	$(CASK) exec ecukes --script $(FEATURES)

features/projects/super-project/.cask:
	cd features/projects/super-project && cask

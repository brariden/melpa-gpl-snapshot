CASK = cask
EMACS = emacs
EMACSFLAGS = -L . soundklaus.el

all: package

.PHONY: clean test

.cask:
	$(CASK) install

checkdoc:
	$(CASK) exec $(EMACS) --batch --eval="(checkdoc)" -Q soundklaus.el

clean:
	@rm -rf dist
	$(CASK) clean-elc

compile: .cask
	$(CASK) exec $(EMACS) --batch --eval="(batch-byte-compile)" -Q soundklaus.el

distclean: clean
	@rm -rf .cask

lint: .cask
	$(CASK) exec $(EMACS) --batch --eval="(elint-file \"soundklaus.el\")" -Q

package: test checkdoc
	$(CASK) package

test: .cask
	$(CASK) exec ert-runner

PACKAGE = org2jekyll
VERSION = $$(grep "^;; Version: " $(PACKAGE).el | cut -f3 -d' ')
ARCHIVE = $(PACKAGE)-$(VERSION).tar
EMACS ?= emacs
CASK ?= cask

pr:
	hub pull-request -b ardumont:master

.PHONY: clean

deps:
	${CASK}

build:
	${CASK} build


clean-dist:
	rm -rf dist/


clean: clean-dist
	rm -rf *.tar
	${CASK} clean-elc

install:
	${CASK} install

test: clean
	${CASK} exec ert-runner

pkg-el:
	${CASK} package

package: clean pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

info:
	${CASK} info

release:
	./release.sh $(VERSION) $(PACKAGE)

version:
	@echo -e "application $(PACKAGE): $(VERSION)\npackage: $(ARCHIVE)"

emacs-install-clean: package
	~/bin/emacs/emacs-install-clean.sh ./$(ARCHIVE)

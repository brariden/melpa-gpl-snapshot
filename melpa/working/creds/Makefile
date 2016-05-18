VERSION=$$(grep "^;; Version: " creds.el | cut -f3 -d' ')
PACKAGE_FOLDER=creds-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar
USER=ardumont

pr:
	hub pull-request -b ardumont:master

clean-dist:
	rm -rf dist/

clean: clean-dist
	rm -rf *.tar $(PACKAGE_FOLDER)

init:
	cask init

deps:
	cask

tests:
	cask exec emacs -Q -batch \
			-l ert \
			-l ./test/launch-tests.el \
			-f ert-run-tests-batch-and-exit

pkg-file:
	cask pkg-file

pkg-el:
	cask package

package: clean pkg-file pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

release:
	./release.sh $(VERSION) $(USER)

info:
	cask info

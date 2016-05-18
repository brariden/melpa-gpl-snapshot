EMACS=emacs
SRC=flymake-gjshint.el

test: test/el-mock.el
	emacs -batch -Q -l test/run-test.el

test-interactively: test/el-mock.el jshint gjslint
	${EMACS} -Q -L . -l test/run-test.el

checkdoc-batch: test/checkdoc-batch.el
	$(EMACS) --batch -L test -l checkdoc-batch.el -f checkdoc-batch-commandline $(SRC) | grep -e "$(SRC):[1-9]" && exit 1 || exit 0

check-package-format:
	emacs $(SRC) --batch -l package -f package-buffer-info

test/el-mock.el:
	wget http://www.emacswiki.org/emacs/download/el-mock.el -O $@

test/checkdoc-batch.el:
	wget ftp://download.tuxfamily.org/user42/checkdoc-batch.el -O $@

jshint:
	which jshint

gjslint:
	which gjslint

emacs-version:
	$(EMACS) --version

travis-ci: emacs-version test checkdoc-batch check-package-format

bump-version: $(SRC) check-package-format checkdoc-batch test
	@if [ "$(NEW_VERSION)" = "" ]; then \
	  echo NEW_VERSION argument not provided.; \
	  echo Usage: make bump-version NEW_VERSION=0.4.1; \
	  exit 1; \
	fi
	sed -i "" -e 's/^;; Version: .*/;; Version: $(NEW_VERSION)/' $(SRC)
	echo "Bump version to $(NEW_VERSION)"
	git commit -am "Bump version to $(NEW_VERSION)"
	git tag -a $(NEW_VERSION) -m $(NEW_VERSION)

.PHONY: test bump-version

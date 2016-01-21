MAGIT_DIR ?=
DASH_DIR ?=
LOAD_PATH = -L $(DASH_DIR) -L $(MAGIT_DIR)
BATCH = emacs -Q --batch $(LOAD_PATH)

all: magit-annex.elc magit-annex-autoloads.el

.PHONY: test
test: magit-annex.elc
	@$(BATCH) -L . -l magit-annex-tests.el \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

.PHONY: clean
clean:
	$(RM) magit-annex.elc magit-annex-autoloads.el

%.elc: %.el
	@$(BATCH) -f batch-byte-compile $<

%-autoloads.el: %.el
	@$(BATCH) --eval \
	"(let (make-backup-files) \
	  (update-file-autoloads \"$(CURDIR)/$<\" t \"$(CURDIR)/$@\"))"

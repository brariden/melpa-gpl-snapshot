EMACS ?= emacs
EMACSFLAGS =
CASK = cask
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export Emacs to recipes
export EMACS

SRCS = frame-restore.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: compile
compile : $(OBJECTS)

.PHONY: clean-all
clean-all : clean clean-pkgdir

.PHONY: clean
clean :
	rm -f $(OBJECTS)

.PHONY: clean-pkgdir
clean-pkgdir:
	rm -rf $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<

$(PKGDIR) : Cask
	$(CASK) install

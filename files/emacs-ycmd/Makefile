EMACS = emacs
EMACSFLAGS =
CASK = cask
ERTSELECTOR = t
YCMDPATH=../ycmd/ycmd
CONVERT = convert
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = ycmd.el third-party/ycmd-request.el third-party/ycmd-request-deferred.el contrib/ycmd-next-error.el
OBJECTS = $(SRCS:.el=.elc)

DISTDIR = dist
BUILDDIR = build

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY: deps compile dist clean clean-elc \
	clobber clobber-dist clobber-deps test

# Build targets
compile : $(OBJECTS)

dist :
	$(CASK) package

# Test targets
test : $(OBJECTS)
	$(EMACSBATCH) --script test/run.el '$(YCMDPATH)' '$(ERTSELECTOR)'

# Support targets
deps : $(PKGDIR)

# Cleanup targets
clean : clean-elc
clobber: clobber-dist clobber-deps

clean-elc :
	rm -rf $(OBJECTS)

clobber-dist :
	rm -rf $(DISTDIR)

clobber-deps :
	rm -rf .cask/

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) \
		--eval "(add-to-list 'load-path \"${PWD}/third-party\")" \
		-L . -f batch-byte-compile $<

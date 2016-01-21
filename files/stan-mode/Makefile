PACKAGES := stan-mode ac-stan stan-snippets

all: build compile

build :
	$(foreach pkg,$(PACKAGES),make -C $(pkg) build-src ; )

compile :
	$(foreach pkg,$(PACKAGES),make -C $(pkg) compile ; )

dist : 
	$(foreach pkg,$(PACKAGES),make -C $(pkg) dist ; )

deps :
	$(foreach pkg,$(PACKAGES),make -C $(pkg) deps ; )

clean : clean-dist clean-deps clean-elc

clean-dist : 
	$(foreach pkg,$(PACKAGES),make -C $(pkg) clean-dist ; )

clean-deps : 
	$(foreach pkg,$(PACKAGES),make -C $(pkg) clean-deps ; )

clean-elc : 
	$(foreach pkg,$(PACKAGES),make -C $(pkg) clean-elc ; )

.PHONY: snippets
snippets:
	-mkdir -p snippets/stan-mode
	cp -v stan-snippets/snippets/stan-mode/.yas-compiled-snippets.el snippets/stan-mode
	cp -v stan-snippets/snippets/stan-mode/.yas-make-groups snippets/stan-mode
	cp -v stan-snippets/snippets/stan-mode/.yas-parents snippets/stan-mode


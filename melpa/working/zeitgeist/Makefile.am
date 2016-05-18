SUBDIRS = $(PLUGINS)
EXTRA_DIST = $(EXTRA_SUBDIRS) MAINTAINERS NEWS README TODO COPYING

local-install:
	for d in $(PLUGINS); \
	do \
		$(MAKE) --directory=$$d $@; \
	done

local-uninstall:
	for d in $(PLUGINS); \
	do \
		$(MAKE) --directory=$$d $@; \
	done


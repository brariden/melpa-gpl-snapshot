SUBDIRS = extension

# identifier for the TB profile in which 'make install' installs this extension
TARGET_PROFILE = *default*

local-install: all
	$(MAKE) -C extension $@
	cp xpcom-thunderbird\@zeitgeist-project.com.xpi ~/.thunderbird/$(TARGET_PROFILE)/extensions/

local-uninstall:
	$(MAKE) -C extension $@
	-rm -rf ~/.thunderbird/$(TARGET_PROFILE)/extensions/xpcom-thunderbird\@zeitgeist-project.com.xpi


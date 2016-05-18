SUBDIRS = extension

# identifier for the FF profile in which 'make install' installs this extension
TARGET_PROFILE = *default*

local-install: all
	$(MAKE) -C extension $@
	cp xpcom-firefox@zeitgeist-project.com.xpi ~/.mozilla/firefox/$(TARGET_PROFILE)/extensions/

local-uninstall:
	$(MAKE) -C extension $@
	-rm -rf ~/.mozilla/firefox/$(TARGET_PROFILE)/extensions/xpcom-firefox@zeitgeist-project.com.xpi


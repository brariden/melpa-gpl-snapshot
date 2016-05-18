SUBDIRS = extension

# identifier for the FF profile in which 'make install' installs this extension
TARGET_PROFILE = *default*

local-install: all
	$(MAKE) -C extension $@
	cd ~/.mozilla/firefox/$(TARGET_PROFILE)/ && mkdir -p extensions
	cp ZeitgeistDataprovider.xpi ~/.mozilla/firefox/$(TARGET_PROFILE)/extensions

local-uninstall:
	$(MAKE) -C extension $@
	-rm -rf ~/.mozilla/firefox/$(TARGET_PROFILE)/extensions/xpcom_firefox\@zeitgeist-project.com
	-rm -rf ~/.mozilla/firefox/$(TARGET_PROFILE)/extensions/ZeitgeistDataprovider.xpi


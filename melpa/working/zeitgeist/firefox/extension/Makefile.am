SUBDIRS = 

extensiondir = $(datadir)/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}
EXTENSIONS_SYMLINK = $(extensiondir)

xul_extdir = $(datadir)/xul-ext-zeitgeist
dist_xul_ext_DATA = \
	license.txt \
	chrome.manifest \
	install.rdf \
	$(NULL)

contentdir = $(xul_extdir)/chrome/content
dist_content_DATA = \
	chrome/content/zeitgeist.xul \
	chrome/content/zeitgeist.png \
	chrome/content/event.js \
	chrome/content/module.js \
	$(NULL)

prefsdir = $(xul_extdir)/defaults/preferences
dist_prefs_DATA = \
	defaults/preferences/zeitgeist.js \
	$(NULL)

install-exec-hook:
	$(MKDIR_P) $(DESTDIR)$(extensiondir)
	test -h "$(DESTDIR)$(EXTENSIONS_SYMLINK)" || $(LN_S) -f "$(DESTDIR)$(xul_extdir)" "$(DESTDIR)$(EXTENSIONS_SYMLINK)"

# we want to allow also local install
EXTENSION_CONTENT = \
	$(dist_xul_ext_DATA) \
	defaults \
	chrome \
	$(NULL)

local-install: all
	zip -r ../xpcom-firefox@zeitgeist-project.com.xpi $(EXTENSION_CONTENT)

local-uninstall:

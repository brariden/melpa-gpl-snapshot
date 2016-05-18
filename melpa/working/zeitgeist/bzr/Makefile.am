EXTRA_DIST = bzr-icon-64.png

bzrplugindir = $(datadir)/pyshared/bzrlib/plugins/zeitgeist
dist_bzrplugin_DATA = __init__.py hooks.py

bzricondir = $(datadir)/pixmaps
bzricon_DATA = bzr-icon-64.png

# we want to allow also local install
BZR_PLUGIN_HOME_DIR = ~/.bazaar/plugins/zeitgeist
BZR_ICON_DIR = ~/.local/share/icons

local-install:
	mkdir -p $(BZR_PLUGIN_HOME_DIR)
	cp __init__.py $(BZR_PLUGIN_HOME_DIR)
	cp hooks.py $(BZR_PLUGIN_HOME_DIR)
	mkdir -p $(BZR_ICON_DIR)
	cp bzr-icon-64.png $(BZR_ICON_DIR)

local-uninstall:
	-rm -rf $(BZR_PLUGIN_HOME_DIR)
	-rm -rf $(BZR_ICON_DIR)


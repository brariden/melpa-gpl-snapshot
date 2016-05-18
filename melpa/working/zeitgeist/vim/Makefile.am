PLUGIN_FILES = zeitgeist.vim

vimplugindir = $(datadir)/vim/vim72/plugin
dist_vimplugin_DATA = $(PLUGIN_FILES)

# we want to allow also local install
VIM_PLUGIN_HOME_DIR = ~/.vim/plugin

local-install:
	mkdir -p $(VIM_PLUGIN_HOME_DIR)
	cp $(PLUGIN_FILES) $(VIM_PLUGIN_HOME_DIR)

local-uninstall:
	rm $(VIM_PLUGIN_HOME_DIR)/$(PLUGIN_FILES)


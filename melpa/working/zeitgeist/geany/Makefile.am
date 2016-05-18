geanyplugindir = $(libdir)/geany
geanyplugin_LTLIBRARIES = zeitgeist_dataprovider.la

zeitgeist_dataprovider_la_SOURCES = \
        zeitgeist_dataprovider.c
zeitgeist_dataprovider_la_CFLAGS = \
        -fPIC -Wall -std=c99 \
        $(ZEITGEIST_CFLAGS) \
        $(GEANY_CFLAGS) \
        $(NULL)

zeitgeist_dataprovider_la_LIBADD = \
        $(ZEITGEIST_LIBS) \
        $(GEANY_LIBS) \
        $(NULL)
zeitgeist_dataprovider_la_LDFLAGS = \
        -module -avoid-version \
        $(NULL)

# we want to allow also local install
GEANY_PLUGIN_HOME_DIR = ~/.config/geany/plugins

PLUGIN_FILES = zeitgeist_dataprovider.so

local-install:
	mkdir -p $(GEANY_PLUGIN_HOME_DIR)
	cp $(PLUGIN_FILES) $(GEANY_PLUGIN_HOME_DIR)

local-uninstall:
	for f in $(PLUGIN_FILES); \
	do \
		echo remove $$f; \
		rm -f $(GEANY_PLUGIN_HOME_DIR)/$$f; \
	done


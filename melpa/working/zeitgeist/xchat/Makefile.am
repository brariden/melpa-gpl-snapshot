EXTRA_DIST = xchat-plugin.h

xchatpluginbinarydir = $(libdir)/xchat/plugins
xchatpluginbinary_LTLIBRARIES = zeitgeist_dataprovider.la

zeitgeist_dataprovider_la_SOURCES = \
        zeitgeist-dataprovider.c 
zeitgeist_dataprovider_la_CFLAGS = \
        -Wl,--export-dynamic -Wall -O1 -shared -fPIC \
        $(ZEITGEIST_CFLAGS) \
        $(NULL)

zeitgeist_dataprovider_la_LIBADD = \
        $(ZEITGEIST_LIBS) \
        $(NULL)
zeitgeist_dataprovider_la_LDFLAGS = \
        -module -avoid-version \
        $(NULL)

# we want to allow also local install
XCHAT_PLUGIN_HOME_DIR = ~/.xchat2

PLUGIN_FILES = ./.libs/zeitgeist_dataprovider.so

local-install:
	mkdir -p $(XCHAT_PLUGIN_HOME_DIR)
	cp $(PLUGIN_FILES) $(XCHAT_PLUGIN_HOME_DIR)

local-uninstall:
	for f in $(PLUGIN_FILES); \
	do \
		echo remove $$f; \
		rm -f $(XCHAT_PLUGIN_HOME_DIR)/$$f; \
	done


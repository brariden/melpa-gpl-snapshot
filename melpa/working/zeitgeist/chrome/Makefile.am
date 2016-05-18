chromeresourcedir = $(datadir)/opt/google/chrome/resources/zeitgeist_plugin
dist_chromeresource_DATA = \
	background.html \
	content_script.js \
	manifest.json \
	zeitgeist.js

pluginbinarydir = $(chromeresourcedir)/plugin
pluginbinary_LTLIBRARIES = libzeitgeist-npapi-plugin.la

libzeitgeist_npapi_plugin_la_SOURCES = \
	$(top_srcdir)/npapi-plugin/np-zeitgeist.cc \
	$(NULL)

libzeitgeist_npapi_plugin_la_CXXFLAGS = \
	-pedantic -fPIC -Wall -std=c++0x -O2 \
	$(ZEITGEIST_CFLAGS) \
	-I$(top_srcdir)/npapi-plugin \
	-I$(top_srcdir)/npapi-plugin/third_party \
	-I$(top_srcdir)/npapi-plugin/third_party/npapi \
	$(NULL)

libzeitgeist_npapi_plugin_la_LIBADD = \
	$(ZEITGEIST_LIBS) \
	$(NULL)

libzeitgeist_npapi_plugin_la_LDFLAGS = \
	-Wl,--no-undefined \
	-module -avoid-version \
	$(NULL)
	
EXTRA_DIST = \
	$(top_srcdir)/npapi-plugin/third_party/webkit/glue/plugins/nphostapi.h \
	$(top_srcdir)/npapi-plugin/third_party/npapi/bindings/npapi.h \
	$(top_srcdir)/npapi-plugin/third_party/npapi/bindings/npruntime.h \
	$(top_srcdir)/npapi-plugin/base/port.h \
	$(top_srcdir)/npapi-plugin/base/basictypes.h \
	$(top_srcdir)/npapi-plugin/build/build_config.h \
	$(NULL)

# we want to allow also local install
local-install: all
	ln -s $(abs_srcdir)/.libs/libzeitgeist-npapi-plugin.so $(top_srcdir)/npapi-plugin/libzeitgeist-npapi-plugin.so

local-uninstall: all

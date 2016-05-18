noinst_LTLIBRARIES = libzeitgeist-npapi-plugin.la

libzeitgeist_npapi_plugin_la_SOURCES = \
	np-zeitgeist.cc \
	$(NULL)

libzeitgeist_npapi_plugin_la_CXXFLAGS = \
	-pedantic -fPIC -Wall -std=c++0x -O2 \
	$(ZEITGEIST_CFLAGS) \
	-I./third_party -I./third_party/npapi -I./
	$(NULL)
	
libzeitgeist_npapi_plugin_la_LIBADD = \
	$(ZEITGEIST_LIBS) \
	$(NULL)
libzeitgeist_npapi_plugin_la_LDFLAGS = \
	-Wl,--no-undefined \
	-module -avoid-version \
	$(NULL)


local-install: all

local-uninstall: all


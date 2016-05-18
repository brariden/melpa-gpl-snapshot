zeitgeistmoduledir = $(libdir)/xul-ext-zeitgeist
zeitgeistmodule_LTLIBRARIES = libzeitgeist_xpcom.la
zeitgeistmodule_DATA = zeitgeist.xpt

xul_extdir = $(datadir)/xul-ext-zeitgeist

libzeitgeist_xpcom_la_SOURCES = \
	ZeitgeistComponent.cpp \
	ZeitgeistModule.cpp \
	$(NULL)

libzeitgeist_xpcom_la_CXXFLAGS = \
	-include "xpcom-config.h" \
	$(ZEITGEIST_CFLAGS) \
	$(LIBXUL_1_9_2_CFLAGS) \
	$(NULL)

libzeitgeist_xpcom_la_LIBADD = \
	$(ZEITGEIST_LIBS) \
	$(LIBXUL_1_9_2_LIBS) \
	$(NULL)

libzeitgeist_xpcom_la_LDFLAGS = \
        -module -avoid-version \
        $(NULL)

BUILT_SOURCES = \
	zeitgeist-library.h \
	zeitgeist.h \
	zeitgeist.xpt \
	$(NULL)
	
EXTRA_DIST = \
	zeitgeist.idl \
	zeitgeistextend.h \
	$(NULL)
	
DISTCLEANFILES = \
	$(BUILT_SOURCES) \
	$(NULL)
	

zeitgeist.h: zeitgeist.idl
	$(AM_V_GEN)$(XPIDL_BIN) -m header -I $(XUL_IDLDIR) -o zeitgeist $<

zeitgeist.xpt: zeitgeist.idl
	$(AM_V_GEN)$(XPIDL_BIN) -m typelib -I $(XUL_IDLDIR) -o zeitgeist $<

zeitgeist-library.h:
	$(AM_V_GEN)$(LN_S) "`$(PKG_CONFIG) --variable includedir zeitgeist-1.0`/zeitgeist-1.0/zeitgeist.h" $@

install-exec-hook:
	test -d $(DESTDIR)$(xul_extdir) || $(MKDIR_P) $(DESTDIR)$(xul_extdir)
	test -h $(DESTDIR)$(xul_extdir)/components || $(LN_S) -f $(DESTDIR)$(zeitgeistmoduledir) $(DESTDIR)$(xul_extdir)/components

# small helper with local install
local-install: all
	cp .libs/libzeitgeist_xpcom.so ./

local-uninstall: ;

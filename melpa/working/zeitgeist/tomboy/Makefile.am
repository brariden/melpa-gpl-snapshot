CSFLAGS = 		\
	-debug 		\
	-define:DEBUG 	\
	-target:library

ASSEMBLIES = 					\
	$(TOMBOY_ADDINS_LIBS)			\
	$(GTK_SHARP_LIBS)			\
	$(ZEITGEIST_SHARP_LIBS)     \
	-r:Mono.Posix

ADDIN_NAME = ZeitgeistDataprovider
TARGET = $(ADDIN_NAME).dll
CSFILES = \
	$(srcdir)/Zeitgeist.cs     \
	$(srcdir)/ZeitgeistAddin.cs     \
	$(srcdir)/NoteHandler.cs     \
	$(srcdir)/AssemblyInfo.cs
RESOURCES = \
	-resource:$(srcdir)/$(ADDIN_NAME).addin.xml

$(TARGET): $(CSFILES)
	$(DMCS) -out:$@ $(CSFLAGS) $(ASSEMBLIES) $(CSFILES) $(RESOURCES)


addinsdir = $(libdir)/tomboy/addins
addins_DATA = 	$(TARGET)

EXTRA_DIST =            	\
	$(CSFILES) \
	$(srcdir)/$(ADDIN_NAME).addin.xml
	
DISTCLEANFILES = \
	$(TARGET) \
	$(TARGET).mdb

TOMBOY_ADDIN_HOME_DIR = ~/.config/tomboy/addins

local-install: $(addins_DATA)
	mkdir -p $(TOMBOY_ADDIN_HOME_DIR)
	cp $(addins_DATA) $(TOMBOY_ADDIN_HOME_DIR)

local-uninstall:
	for f in $(addins_DATA); \
	do \
		echo remove $$f; \
		rm -f $(TOMBOY_ADDIN_HOME_DIR)/$$f; \
	done

CSFLAGS = 		\
	-debug 		\
	-define:DEBUG 	\
	-target:library

ASSEMBLIES = 					\
	$(MONODEVELOP_LIBS)			\
	$(ZEITGEIST_SHARP_LIBS)     		\
	$(MONO_ADDINS_LIBS)     		\
	-r:Mono.Posix

ADDIN_NAME = ZeitgeistDataprovider
TARGET = $(ADDIN_NAME).dll
CSFILES = \
	$(srcdir)/StartupHandler.cs     \
	$(srcdir)/TrackedDocument.cs     \
	$(srcdir)/ZeitgeistClient.cs     \
	$(srcdir)/AssemblyInfo.cs
RESOURCES = \
	-resource:$(srcdir)/MonoDevelop.Zeitgeist.addin.xml

$(TARGET): $(CSFILES)
	$(DMCS) -out:$@ $(CSFLAGS) $(ASSEMBLIES) $(CSFILES) $(RESOURCES)


addinsdir = $(libdir)/monodevelop/Addins
addins_DATA = 	$(TARGET)

EXTRA_DIST =            	\
	$(CSFILES) \
	$(srcdir)/MonoDevelop.Zeitgeist.addin.xml
	
DISTCLEANFILES = \
	$(TARGET) \
	$(TARGET).mdb

#
# Copyright (c) 2003-2005 The University of Wroclaw.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#    1. Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#    3. The name of the University may not be used to endorse or promote
#       products derived from this software without specific prior
#       written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
# NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# Include configuration determined by configure script.
include config.mak

############################################################
# VARIABLES
############################################################

svn2log = LC_ALL=pl_PL.utf-8 $(PYTHON) misc/svn2log.py changelog.xml -u misc/users
nemroot = /nemerle/(trunk|(branches|tags)/[^/]+)

# files that contain System.Reflection.AssemblyVersion attribute
version_files = \
	ncc/misc/AssemblyInfo.n \
	macros/AssemblyInfo.n \
	lib/AssemblyInfo.n \
	tools/cs2n/AssemblyInfo.{n,cs} \
	ncc/main.n \
	tools/nemerlish/eval.n \
	tools/nemerlish/main.n \

############################################################
# OUTPUT
############################################################

MKDIR = @echo MKDIR $1
TAR   = @echo TAR   $1
CP    = @echo CP    $1
INST  = @echo INSTALL $1

############################################################
# TARGETS
############################################################

# This is the default target.  It bootstraps compiler, and
# builds standard library.
all:
#	$(Q)$(MAKE) -C ncc boot
#	$(Q)$(MAKE) -C tools all
	$(MSBUILD) $(NPROJ) /p:TargetFrameworkVersion=v$(FRAMEWORK) /t:$(TARGET) /p:Configuration=$(CONFIG) /tv:$(TOOLSVER) 


# This is necessary to make sure, that configuration file
# has been generated, and it is up to date.
config.mak: configure
	./configure

send: dist send-dist

changelog:
	svn up
	@echo Logging...
	@last=`perl -e '$$_ = <>; /\[r(\d+-)?(\d+)\]/ and print "$$2\n"' ChangeLog 2>/dev/null`; \
	if [ X$$last = X ] ; then last=0 ; fi ; \
	last=$$(($$last + 1)) ; \
	if [ `svn info | grep '^Revision:' | sed -e 's/.* //'` -le $$last ] ; then \
           echo "Nothing to log." ; \
       	else \
           echo "Loggin from $$last."; \
           svn log -r HEAD:$$last -v --xml > changelog.xml  && \
	   echo Running the script. && \
           $(svn2log) -o ChangeLog.new -p '$(nemroot)' -r npc/ncc=ncc && \
           cat ChangeLog > ChangeLog.old 2>/dev/null; \
           cat ChangeLog.new ChangeLog.old > ChangeLog && \
           rm -f ChangeLog.old ChangeLog.new changelog.xml ; \
        fi

#sync:
#	$(MAKE) -C tools/msbuild-task all sync-impl
#	$(MAKE) -C ncc boot sync-impl
        
#sync-boot: sync
#	svn commit -m "Sync for release." boot/

dist: changelog tarball

tarball:
	$(Q)rm -rf $(PACKAGE)-$(VERSION).*
	svn export . $(PACKAGE)-$(VERSION).$(REVISION)
	-cp ChangeLog $(PACKAGE)-$(VERSION).$(REVISION)
	$(Q)$(MAKE) -C tools/cs2n dist
	$(Q)$(MAKE) -C $(PACKAGE)-$(VERSION).$(REVISION) dist-cleaner
	$(Q)cp tools/cs2n/*.cs $(PACKAGE)-$(VERSION).$(REVISION)/tools/cs2n/
	$(TAR) $(PACKAGE)-$(VERSION).$(REVISION).tar.gz 
	@tar zcf $(PACKAGE)-$(VERSION).$(REVISION).tar.gz $(PACKAGE)-$(VERSION).$(REVISION)
	$(TAR) $(PACKAGE)-$(VERSION).$(REVISION).tar.bz2
	@tar jcf $(PACKAGE)-$(VERSION).$(REVISION).tar.bz2 $(PACKAGE)-$(VERSION).$(REVISION)
	$(Q)rm -rf $(PACKAGE)-$(VERSION).$(REVISION)

get-static-copies:
	@echo Getting static copies of the wiki.
	rm -rf doc/wiki
	mkdir doc/wiki
	cd doc/wiki && lftp http://nemerle.org/static/ -e 'mget *.html' < /dev/null
	rm -f doc/wiki/*.txt

dist-cleaner:
	@echo Setting up html doc.
	$(Q)$(MAKE) -C doc dist-cleaner
	@echo Cleaning non-dist junk.
	$(Q)$(MAKE) clean
	$(Q)rm -rf doc/images doc/presentation
	$(Q)rm -f config.mak configure.log
	$(Q)$(MAKE) get-static-copies
	mv -f doc/wiki/*.html doc/html/
	rm -rf doc/wiki/
	rm -f nemerle.pc config.mak configure.log

prepareinstall:
	@echo Copying files to $(OUTPUTDIR)/lib/nemerle...
	@mkdir -p $(OUTPUTDIR)/bin $(OUTPUTDIR)/lib/nemerle
	@cp $(OUTPUTDIR)/Stage4/*.dll $(OUTPUTDIR)/lib/nemerle
	@cp -n $(OUTPUTDIR)/Stage4/*.exe $(OUTPUTDIR)/lib/nemerle
	@cp -n $(OUTPUTDIR)/Stage4/Nemerle.MSBuild.targets $(OUTPUTDIR)/lib/nemerle
	@cp -n $(OUTPUTDIR)/Linq/*.dll $(OUTPUTDIR)/lib/nemerle
	@cp -n $(OUTPUTDIR)/PowerPack/*.dll $(OUTPUTDIR)/lib/nemerle
	@cp -n $(OUTPUTDIR)/TestFramework/*.dll $(OUTPUTDIR)/lib/nemerle
	@cp -n $(OUTPUTDIR)/Unsafe/*.dll $(OUTPUTDIR)/lib/nemerle
	@echo "#!/bin/sh" > $(OUTPUTDIR)/bin/ncc && \
	echo '$(NET_ENGINE) $(LIBDIR)/ncc.exe "$$@"' >> $(OUTPUTDIR)/bin/ncc && \
	chmod 775 $(OUTPUTDIR)/bin/ncc
	@rm -f $(OUTPUTDIR)/lib/nemerle/policy*.dll

install: prepareinstall
	$(Q)install -d $(DESTDIR)$(PKGCONFIGDIR)
	$(Q)$(MAKE) -C doc install
	$(Q)mkdir -p $(DESTDIR)$(BINDIR) $(DESTDIR)$(LIBDIR)
	$(Q)for file in $(OUTPUTDIR)/lib/nemerle/*; do \
	  echo INSTALL "["$(DESTDIR)$(LIBDIR)"]" `basename $$file`; \
	  install -m 775 $$file $(DESTDIR)$(LIBDIR); \
	done
	$(INST) "["$(DESTDIR)$(BINDIR)"]" ncc
	$(Q)install -m 775 $(OUTPUTDIR)/bin/ncc $(DESTDIR)$(BINDIR)
	$(Q)echo -n Installing components to GAC...
	$(Q)for file in $(OUTPUTDIR)/lib/nemerle/*.dll; do \
	  $(GACUTIL_COMMAND)  $$file 2>/dev/null 1>&2; true; \
	done
	$(Q)echo done
	$(INST) "["$(PKGCONFIGDIR)"]" nemerle.pc
	$(Q)install -m 644 nemerle.pc $(DESTDIR)$(PKGCONFIGDIR)/nemerle.pc

uninstall:
	@echo -n Removing components from GAC... 
	@for file in $(DESTDIR)$(LIBDIR)/*.dll; do \
	  $(GACUTIL_UNINSTALL_COMMAND) $$file 2>/dev/null 1>&2; true; \
	done
	@echo done
	@echo RM -rf $(DESTDIR)$(LIBDIR) 
	$(Q)rm -rf $(DESTDIR)$(LIBDIR)
	@echo RM $(DESTDIR)$(BINDIR)/ncc
	$(Q)rm $(DESTDIR)$(BINDIR)/ncc
	$(Q)$(MAKE) -C doc  uninstall
	$(Q)rm -f $(PKGCONFIGDIR)/nemerle.pc

check: all
	$(Q)$(MAKE) -C ncc  tests
	$(Q)$(MAKE) -C snippets clean
	$(Q)$(MAKE) -C snippets all
	$(Q)$(MAKE) -C lib/tests all
	$(Q)$(MAKE) -C tools/nemerlish check
	$(Q)$(MAKE) -C ncc/testsuite/frommcs all
	$(Q)$(MAKE) -C ncc/testsuite/ clean

clean:
	rm -rf bin/ obj/
	rm -f config.mak configure.log

set-version: config.mak
	@echo -n "Setting version to $(VERSION).$(REVISION) "
	$(Q)perl -p -i -e 's/(\"Nemerle Compiler \(ncc\) version) [0-9.]+ \([^\)]+\)\\n"/$$1 $(VERSION).$(REVISION) (MARK-SET-VER)\\n"/' ncc/main.n
	$(Q)[ `grep -c MARK-SET-VER ncc/main.n` = 1 ] || \
		{ echo "Failed to set text version on ncc/main.n"; exit 1; }
	$(Q)kind=`if echo $(REVISION) | grep -q 99 ; then echo SVN ; else echo release ; fi` ; \
	    echo -n "($$kind) " ; \
	    perl -p -i -e "s/MARK-SET-VER/$$kind/" ncc/main.n
	@:
	$(Q)perl -p -i -e 's/(\"C# to Nemerle translator \(cs2n\) version) [0-9.]+ \([^\)]+\)\\n"/$$1 $(VERSION).$(REVISION) (MARK-SET-VER)\\n"/' tools/cs2n/cs2n.n
	$(Q)[ `grep -c MARK-SET-VER tools/cs2n/cs2n.n` = 1 ] || \
		{ echo "Failed to set text version on tools/cs2n/cs2n.n"; exit 1; }
	$(Q)kind=`if echo $(REVISION) | grep -q 99 ; then echo SVN ; else echo release ; fi` ; \
	    perl -p -i -e "s/MARK-SET-VER/$$kind/" tools/cs2n/cs2n.n
	@:
	$(Q)for f in $(version_files) ; do \
		echo -n "."; \
		perl -p -i -e \
		 '$$r="$(REVISION)"; $$r =~ s/99$$/SVN/; s/AssemblyVersion(FromSVN)?\("[^"]+"\)/AssemblyVersion$$1("$(VERSION).$$r")MARK-SET-VER/' \
		 $$f ; \
		: check if we succeed ; \
		grep -q MARK-SET-VER $$f || { echo "Failed to set version on $$f"; exit 1; } ; \
		: clean up ; \
		perl -p -i -e 's/MARK-SET-VER//' $$f ; \
	done
	@perl -p -i -e \
		's/(property name="msi.version" value=")[^"]*"/$${1}$(VERSION).$(REVISION)"/' \
		misc/packages/msi/msi.build
	@echo .

snapshot:
	$(MAKE) tarball REVISION=$(REVISION).`svn info | grep 'Revision:'|sed -e 's/.*://'|xargs`

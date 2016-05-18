AC_INIT([zeitgeist-datasources],
	[0.8.1],
	[https://bugs.launchpad.net/zeitgeist-datasources/])

AC_PREFIX_DEFAULT([/usr])

AM_INIT_AUTOMAKE([1.9 foreign])

# Enable silent build when available (Automake 1.11)
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

dnl pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test "x$PKG_CONFIG" = "xno"; then
	AC_MSG_ERROR([You need to install pkg-config])
fi

AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_CXX
AC_PROG_LIBTOOL
AC_STDC_HEADERS

dnl ==============================================
dnl Check for vala
dnl ==============================================
AC_ARG_WITH([vala],
	AS_HELP_STRING([--without-vala],
		[Disables Vala plugins]),
	[],[with_vala=yes])

if test "$with_vala" != "no"; then
  AM_PROG_VALAC([0.7.10])
fi
AM_CONDITIONAL(ENABLE_VALA, test "x$with_vala" = "xyes")

dnl ==============================================
dnl Check for Mono
dnl ==============================================

AC_ARG_WITH([mono],
	AS_HELP_STRING([--without-mono],
		[Disables Mono plugins]),
	[],[with_mono=yes])

AM_CONDITIONAL(ENABLE_MONO, test "x$with_mono" = "xyes")


dnl ==============================================
dnl Check commonly used packages
dnl ==============================================

PKG_CHECK_MODULES(ZEITGEIST, zeitgeist-1.0, [HAVE_LIBZG=yes], [HAVE_LIBZG=no])
PKG_CHECK_MODULES(GTK, gtk+-2.0, [HAVE_GTK=yes], [HAVE_GTK=no])

dnl ==============================================
dnl Enable building only supported plugins
dnl ==============================================
AC_ARG_ENABLE([all-plugins],
	[AS_HELP_STRING([--enable-all-plugins],[Enable building of all plugins without checking for their support])],
        [all_plugins=$enableval], [all_plugins=no])

AC_MSG_NOTICE([Requested to enable all plugins: ${all_plugins}])

# The full list of plugins
allowed_plugins="bzr chrome geany vim emacs tomboy telepathy xchat firefox firefox-36-libzg monodevelop thunderbird"
# currently disabled = "epiphany"

# npapi-plugin has a template Makefile.am, but don't use it directly
#   - see chrome's Makefile.am for how to compile it

plugin_error_or_ignore()
{
	if test "${error_on_bad_plugin}" = "1" ; then
		AC_MSG_ERROR([$1])
	else
		AC_MSG_WARN([$1 (disabling plugin)])
	fi
}

used_plugins="${allowed_plugins}"
used_plugins2=""
if test "x${all_plugins}" = "xyes" ; then
	error_on_bad_plugin="1"
else
	error_on_bad_plugin="0"
fi

AC_PATH_PROG(DMCS, dmcs, no)

# Check for plugin-specific requirements and error if necessary
for plugin in ${used_plugins}; do
	add_plugin="1"

	case ${plugin} in
		bzr)
		;;
		chrome)
			if test "${HAVE_LIBZG}" != "yes" ; then
				plugin_error_or_ignore "libzeitgeist not found"
				continue
			fi
		;;
		epiphany)
			# not supported atm
			continue
		;;
		firefox)
		;;
		firefox-36-libzg)
			if test "${HAVE_LIBZG}" != "yes" ; then
				plugin_error_or_ignore "libzeitgeist not found"
				continue
			fi
			PKG_CHECK_MODULES(LIBXUL_1_9_2, [libxul >= 1.9.2 libxul < 2.0],
				[HAVE_LIBXUL=yes], [HAVE_LIBXUL=no])
			if test "${HAVE_LIBXUL}" != "yes" ; then
				plugin_error_or_ignore "libxul 1.92 for firefox-36-libzg not found"
				continue
			fi
			XPIDL_BIN="`$PKG_CONFIG --variable=sdkdir libxul`/bin/xpidl"
			AC_SUBST(XPIDL_BIN)
			XUL_IDLDIR="`$PKG_CONFIG --variable=idldir libxul`"
			AC_SUBST(XUL_IDLDIR)
		;;
		geany)
			if test "${HAVE_LIBZG}" != "yes" ; then
				plugin_error_or_ignore "libzeitgeist not found"
				continue
			fi
			if test "x$HAVE_GTK" = "xno"; then
				plugin_error_or_ignore "You need to install gtk development headers"
				continue
			fi
			PKG_CHECK_MODULES(GEANY, geany,
				[HAVE_GEANY=yes], [HAVE_GEANY=no])
			if test "${HAVE_GEANY}" != "yes" ; then
				plugin_error_or_ignore "geany not found"
				continue
			fi
		;;
		monodevelop)
			if test "${with_mono}" != "yes" ; then
				continue
			fi
			PKG_CHECK_MODULES(ZEITGEIST_SHARP, zeitgeist-sharp,
				ENABLE_ZEITGEIST_SHARP=yes, ENABLE_ZEITGEIST_SHARP=no)
			PKG_CHECK_MODULES(MONODEVELOP, monodevelop, 
				HAS_MONODEVELOP=yes, HAS_MONODEVELOP=no)
			PKG_CHECK_MODULES(MONO_ADDINS, mono-addins, 
				HAS_MONO_ADDINS=yes, HAS_MONO_ADDINS=no)

			if test "${ENABLE_ZEITGEIST_SHARP}" = "no" ; then
				plugin_error_or_ignore "zeitgeist-sharp package not found"
				continue
			fi
			if test "x$HAS_MONODEVELOP" = "xno"; then
				plugin_error_or_ignore "You need to install monodevelop"
				continue
			fi
			if test "x$HAS_MONO_ADDINS" = "xno"; then
				plugin_error_or_ignore "You need to install mono-addins"
				continue
			fi
			AC_SUBST(MONODEVELOP_LIBS)
			AC_SUBST(MONO_ADDINS_LIBS)
		;;
		npapi-plugin)
			if test "${HAVE_LIBZG}" != "yes" ; then
				plugin_error_or_ignore "libzeitgeist not found"
				continue
			fi
		;;
		tomboy)
			if test "${with_mono}" != "yes" ; then
				continue
			fi
			PKG_CHECK_MODULES(ZEITGEIST_SHARP, zeitgeist-sharp,
				ENABLE_ZEITGEIST_SHARP=yes, ENABLE_ZEITGEIST_SHARP=no)
			PKG_CHECK_MODULES(TOMBOY_ADDINS, tomboy-addins,
				HAS_TOMBOY_ADDINS=yes, HAS_TOMBOY_ADDINS=no)
			PKG_CHECK_MODULES(GTK_SHARP, gtk-sharp-2.0, 
				HAS_GTK_SHARP=yes, HAS_GTK_SHARP=no)

			if test "${ENABLE_ZEITGEIST_SHARP}" = "no" ; then
				plugin_error_or_ignore "zeitgeist-sharp package not found"
				continue
			fi

			AC_PATH_PROG(TOMBOY, tomboy, no)
			if test "x$TOMBOY_ADDINS" = "xno"; then
				plugin_error_or_ignore "You need to install tomboy"
				continue
			fi
			if test "x$DMCS" = "xno"; then
				plugin_error_or_ignore "You need to install dmcs"
				continue
			fi
			if test "x$HAS_GTK_SHARP" = "xno"; then
				plugin_error_or_ignore "You need to install gtk-sharp"
				continue
			fi
			AC_SUBST(ZEITGEIST_SHARP_LIBS)
		;;
		thunderbird)
		;;
		vim)
		;;
		emacs)
			AM_PATH_LISPDIR
		;;
		telepathy)
		;;
		xchat)
			if test "${HAVE_LIBZG}" != "yes" ; then
				plugin_error_or_ignore "libzeitgeist not found"
				continue
			fi
		;;
		*)
			plugin_error_or_ignore "${plugin} is not handled"
			continue
		;;
		esac

	# Add the specified plugin
	used_plugins2="${used_plugins2} ${plugin}"
done

ALL_PLUGINS=$allowed_plugins
PLUGINS=$used_plugins2

AC_MSG_CHECKING([which plugins to compile])
AC_MSG_RESULT([$PLUGINS])

AC_SUBST([PLUGINS])

AC_SUBST([AM_CPPFLAGS])
AC_SUBST([AM_CFLAGS])
AC_SUBST([AM_CXXFLAGS])
AC_SUBST([AM_LDFLAGS])

AC_CONFIG_FILES([Makefile])

AC_MSG_NOTICE([Zeitgeist-dataproviders was configured with the following options:])

extra_subdirs=""

if test "x${PLUGINS}" != "x" ; then
        for allowed_plugin in ${ALL_PLUGINS}; do
                for plugin in ${PLUGINS}; do
                        case ${allowed_plugin} in
                                ${plugin})
                                        AC_MSG_NOTICE([** ${allowed_plugin} plugin enabled])
                                        case ${allowed_plugin} in
                                        	bzr)
                                        		AC_CONFIG_FILES([bzr/Makefile])
                                        		continue 2
                                        	;;
                                        	chrome)
                                        		AC_CONFIG_FILES([chrome/Makefile])
                                        		extra_subdirs="${extra_subdirs} npapi-plugin"
                                        		continue 2
                                        	;;
                                        	firefox)
                                        		AC_CONFIG_FILES([
                                        			firefox/Makefile
                                        			firefox/extension/Makefile
                                        		])
                                        		continue 2
                                        	;;
                                        	firefox-36-libzg)
                                        		AC_CONFIG_FILES([
                                        			firefox-36-libzg/Makefile
                                        			firefox-36-libzg/extension/Makefile
                                        			firefox-36-libzg/extension/components/Makefile
                                        		])
                                        		continue 2
                                        	;;
                                        	geany)
                                        		AC_CONFIG_FILES([geany/Makefile])
                                        		continue 2
                                        	;;
                                        	tomboy)
                                        		AC_CONFIG_FILES([tomboy/Makefile])
                                        		continue 2
                                        	;;
                                        	thunderbird)
                                        		AC_CONFIG_FILES([
                                        			thunderbird/Makefile
                                        			thunderbird/extension/Makefile
                                        		])
                                        		continue 2
                                        	;;
						                    monodevelop)
                                        		AC_CONFIG_FILES([monodevelop/Makefile])
                                        		continue 2
                                        	;;
                                        	vim)
                                        		AC_CONFIG_FILES([vim/Makefile])
                                        		continue 2
                                        	;;
                                        	emacs)
                                        		AC_CONFIG_FILES([emacs/Makefile])
                                        		continue 2
                                        	;;
                                        	telepathy)
                                        		AC_CONFIG_FILES([telepathy/Makefile])
                                        		continue 2
                                        	;;
                                        	xchat)
                                        		AC_CONFIG_FILES([xchat/Makefile])
                                        		continue 2
                                        	;;
                                        	*)
                                        		AC_MSG_ERROR([cannot configure ${allowed_plugin}])
                                        		continue 2
                                        	;;
                                        esac
                                        continue 2
                                ;;
                                *);;
                        esac
                done
                AC_MSG_NOTICE([   ${allowed_plugin} plugin disabled])
        done
else
        AC_MSG_NOTICE([   No plugins enabled])
fi

EXTRA_SUBDIRS=$extra_subdirs
AC_SUBST([EXTRA_SUBDIRS])
AC_OUTPUT

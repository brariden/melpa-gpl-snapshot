
# Melpa GPLed snapshot

This is just a point in time snapshot of the ELPA packages that meet the following criteria:

* MELPA source code matched the text search "General...Public...License"  and then "Version 2" or "Version 3"
* No files in the repo contain the words "under the terms of the GNU Affero".  This is to excludes AGPL files.
* Mirrors of GNU ELPA (https://elpa.gnu.org/packages/) and Orgmode (http://orgmode.org/elpa/) websites, which are both under GPL.
* Additional MELPA packages that spacemacs requires that were not found in the text search but still had free licenses:

 * clean-aindent-mode: Public Domain - https://github.com/pmarinov/clean-aindent-mode/blob/master/LICENSE.txt
 * edn - MIT - https://github.com/expez/edn.el/blob/master/LICENSE
 * evil-args - MIT - https://github.com/wcsmith/evil-args/blob/master/LICENSE.txt
 * helm-gitignore - MIT - https://github.com/jupl/helm-gitignore/blob/master/LICENSE
 * highlight - GPLv2 - footer on emacs wiki: http://www.emacswiki.org/emacs/highlight.el
 * highlight-numbers - BSD - https://github.com/Fanael/highlight-numbers/blob/master/highlight-numbers.el
 * parent-mode - BSD - https://github.com/Fanael/parent-mode/blob/master/parent-mode.el
 * magit-gitflow - GPLv3 - https://github.com/jtatarik/magit-gitflow/blob/master/magit-gitflow.el
 * elisp-slime-nav - GPLv3 - https://github.com/purcell/elisp-slime-nav/blob/master/elisp-slime-nav.el
 * evil-surround - GPLv3 - https://github.com/timcharper/evil-surround/blob/master/evil-surround.el
 * magit - GPLv3 - https://github.com/magit/magit/blob/master/COPYING
 * orgit - GPLv3 - https://github.com/magit/orgit/blob/master/orgit.el
 * gntp - BSD - https://github.com/tekai/gntp.el/blob/master/gntp.el
 * magit-popup - GPLv3 - https://github.com/magit/magit/blob/master/lisp/magit-popup.el
 * evil-magit - GPLv3 - https://github.com/justbur/evil-magit/blob/master/evil-magit.el
 * xterm-color - BSD - https://github.com/atomontage/xterm-color
 * rainbow-identifiers - BSD - https://github.com/Fanael/rainbow-identifiers/blob/master/rainbow-identifiers.el
 * orgmode's packages - GPLv3 http://orgmode.org/cgit.cgi/org-mode.git/tree/COPYING
 * evil-mc - MIT - https://github.com/gabesoft/evil-mc/blob/master/LICENSE

Obviously I'm not claiming ownership of this code; the original licensing information for each package applies.  Look to MELPA.org for current versions of each package.

# Source code
There is no real source code but instead there is a simple bash script to pull down melpa, check for the correct license, and then mirror elpa/orgmode.org.



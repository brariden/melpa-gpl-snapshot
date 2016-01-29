
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

Obviously I'm not claiming ownership of this code; the original licensing information for each package applies.  Look to MELPA.org for current versions of each package.

# Source code
There is no real source code but below are the steps to reproduce the distribution.  Sorry the sed regexps are not the best.  Also ran this in fish shell so it may not be the same in bash.
The intermediate files are stored in the repo if you want to check the names of the packages included.

```
# clone melpa
git clone git@github.com:milkypostman/melpa.git
cd melpa
make

# Find which packages are GPLed:
# (using pcregrep instead of grep for searching across newlines... sometimes developers cite the GPL as part of comments in source code which span lines)
pcregrep -i -r -l --buffer-size=1000000 --exclude-dir=.git -M "general[\s|;]+public[\s|;]license" * | xargs grep -l -r -i -P "Version\s+[2|3]" | sed "s|/.*||" | sort | uniq > ../GPLed-projects.txt

# Remove any Affero licensed packages
pcregrep -r -i -l --buffer-size=1000000 --exclude-dir=.git -M "under[\s|;]+the[\s|;]+terms[\s|;]+of[\s|;]+the[\s|;]+GNU[\s|;]+Affero" * | sed "s|/.*||" | uniq > ../Affero.txt
cat ../Affero.txt | xargs -L 1 -I @ sh -c "ls -1 @*" |  xargs -L 1 echo rm 

# BTW, I found the following in Affero.txt and doublechecked that they were removed correctly
cat Affero.txt
> artbollocks-mode
> ghc
> lice
> mediawiki
> ocp-indent
> stack-mode


# Keep ONLY the GPLed packages!
#!/bin/bash
for filename in packages/*; do

 prefix=`echo $(basename $filename) | sed "s/\(.*\)-.*/\1/"`
 gpled=`grep $prefix GPLed-projects.txt`
 echo "gpled:" $gpled "prefix:" $prefix
 if [ ! -z "$gpled" ]; then
      echo "keeping" $filename >> package-status.txt
 else
      echo "deleting" $filename >> package-status.txt
      rm $filename
 fi
done

# So now you can see what happened in the package-status file based on if it was GPLed or not.

# Finally generate the json for the MEPLA website:
make json

```



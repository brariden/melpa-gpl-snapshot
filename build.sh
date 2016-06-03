#!/bin/bash
# clone melpa
git clone git@github.com:milkypostman/melpa.git
cd melpa
make clean
make

# lots of .git files..
find . -name .git -print0 | xargs -0 rm -rf
rm .gitignore

cd working
echo "Step 1"
# Find which packages are GPLed:
# (using pcregrep instead of grep for searching across newlines... sometimes developers cite the GPL as part of comments in source code which span lines)
pcregrep -I -i -r -l --buffer-size=1000000 --exclude-dir=.git -M "general[\s|;]+public[\s|;]license" * | xargs grep -l -r -i -P "Version\s+[2|3]" | sed "s|/.*||" | sort | uniq > ../ok-projects.txt

echo "Step 2"
# Add public domain
pcregrep -I -i -r -l --buffer-size=1000000 --exclude-dir=.git -M "released[\s|;]+into[\s|;]+the[\s|;]+public[\s|;]+domain" * | sed "s|/.*||" | sort | uniq >> ../ok-projects.txt

echo "Step 3"
# Remove any Affero licensed packages
pcregrep -I -r -i -l --buffer-size=1000000 --exclude-dir=.git -M "under[\s|;]+the[\s|;]+terms[\s|;]+of[\s|;]+the[\s|;]+GNU[\s|;]+Affero" * | sed "s|/.*||" | uniq > ../Affero.txt
cd ..
cat Affero.txt | xargs -L 1 -I @ sh -c "ls -1 packages/@*" | xargs -L 1 echo rm
cat Affero.txt | xargs -L 1 -I @ sh -c "ls -1 packages/@*" | xargs -L 1 rm
echo "PAST 4"

# Add in the special packages (needed for spacemacs):
echo clean-aindent-mode >> ok-projects.txt # Public Domain - https://github.com/pmarinov/clean-aindent-mode/blob/master/LICENSE.txt
echo edn >> ok-projects.txt # - MIT - https://github.com/expez/edn.el/blob/master/LICENSE
echo evil-args >> ok-projects.txt # - MIT - https://github.com/wcsmith/evil-args/blob/master/LICENSE.txt
echo helm-gitignore >> ok-projects.txt # - MIT - https://github.com/jupl/helm-gitignore/blob/master/LICENSE
echo highlight >> ok-projects.txt # - GPLv2 - footer on emacs wiki: http://www.emacswiki.org/emacs/highlight.el
echo highlight-numbers >> ok-projects.txt # - BSD - https://github.com/Fanael/highlight-numbers/blob/master/highlight-numbers.el
echo parent-mode >> ok-projects.txt # - BSD - https://github.com/Fanael/parent-mode/blob/master/parent-mode.el
echo magit-gitflow >> ok-projects.txt # - GPLv3 - https://github.com/jtatarik/magit-gitflow/blob/master/magit-gitflow.el
echo elisp-slime-nav >> ok-projects.txt # - GPLv3 - https://github.com/purcell/elisp-slime-nav/blob/master/elisp-slime-nav.el
echo evil-surround >> ok-projects.txt # - GPLv3 - https://github.com/timcharper/evil-surround/blob/master/evil-surround.el
echo magit >> ok-projects.txt # - GPLv3 - https://github.com/magit/magit/blob/master/COPYING
echo orgit >> ok-projects.txt # - GPLv3 - https://github.com/magit/orgit/blob/master/orgit.el
echo gntp >> ok-projects.txt # - BSD - https://github.com/tekai/gntp.el/blob/master/gntp.el
echo magit-popup >> ok-projects.txt # - GPLv3 - https://github.com/magit/magit/blob/master/lisp/magit-popup.el
echo evil-magit >> ok-projects.txt # - GPLv3 - https://github.com/justbur/evil-magit/blob/master/evil-magit.el
echo xterm-color >> ok-projects.txt # - BSD - https://github.com/atomontage/xterm-color
echo rainbow-identifiers >> ok-projects.txt # - BSD - https://github.com/Fanael/rainbow-identifiers/blob/master/rainbow-identifiers.el
echo evil-mc >> ok-projects.txt # - MIT - https://github.com/gabesoft/evil-mc/blob/master/LICENSE


# Keep ONLY the ok-licensed packages!
#!/bin/bash
for filename in packages/*; do

 prefix=`echo $(basename $filename) | sed "s/\(.*\)-.*/\1/"`
 oked=`grep $prefix ok-projects.txt`
 echo "checking:" $oked "prefix:" $prefix
 if [ ! -z "$oked" ]; then
      echo "keeping" $filename >> package-status.txt
 else
      echo "deleting" $filename >> package-status.txt
      rm $filename
 fi
done

make clean-json
make json
rm -rf working
cd ..

wget --mirror --convert-links --adjust-extension --page-requisites --no-parent -R "*org-2014*","*org-2013*","*org-2015*tar","*org-plus-contrib-2015*" http://orgmode.org/elpa/

wget --mirror --convert-links --adjust-extension --page-requisites --no-parent -R "*org-2015*","*org-2014*","*org-2013*","*org-2012*" http://elpa.gnu.org/
wget --mirror --convert-links --page-requisites --no-parent -R "*org-2014*","*org-2013*","*org-2012*" http://elpa.gnu.org/packages/archive-contents



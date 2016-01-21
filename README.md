
# Melpa GPLed snapshot

This is just a point in time snapshot of the MELPA packages that meet the following criteria:

* The MELPA recipe points to a github repo
* Files in the github repo matched the text search "General...Public...License"  and then "Version 2" or "Version 3"
* No files in the repo contain the words "under the terms of the GNU Affero".  This is to exclude AGPL files.

Obviously I'm not claiming ownership of this code; the original licensing information for each package applies.  Look to MELPA.org for current versions of each package.

# Source code
There is no real source code but below are the steps to reproduce the distribution.  Sorry the sed regexps are not the best.  Also ran this in fish shell so it may not be the same in bash.
The intermediate files are stored in the repo if you want to check the names of the packages included.

```
# clone melpa
mkdir ~/git-test
cd git-test
git clone git@github.com:milkypostman/melpa.git

# extract from the recipe the repo: "reponame" part and put all of those in a "gitrepos" file:
cd melpa/recipes
mkdir ~/git-test/all-repos
grep -l "github" *  | xargs grep ":repo" | sed "s/.*:repo\s*\"\(.*\)/\1/" | sed "s/\".*//" > ~/git-test/all-repos/gitrepos
 
# clone each of the github repos in gitrepos:
cd ~/git-test/all-repos
cat gitrepos | xargs -I '{}' git clone --depth=1 https://github.com/'{}'.git

# remove git tracking
find . | grep /.git$ | xargs rm -rf

# Find all directories with GPL text:
grep -l -r -i -P "General.*Public.*License" * | xargs grep -l -r -i -P "Version\s+[2|3]" | sed "s|/.*||" | uniq > GPLed.txt

# Remove anything with AGPL:
pcregrep -r -l --buffer-size=1000000 --exclude-dir=.git -M "under[\s|;]+the[\s|;]+terms[\s|;]+of[\s|;]+the[\s|;]+GNU[\s|;]+Affero" * | sed "s|/.*||" | uniq > Affero.txt
cat Affero.txt  | xargs -L 1 rm -rf 

# BTW, I found the following in Affero.txt and doublechecked that they were removed correctly
cat Affero.txt
>artbollocks-mode
>ghc-mod
>lice-el
>mediawiki-el
>ocp-indent

# now add all directories referenced in GPLed.txt that weren't removed
mkdir ../melpa-gpl-packages
cat GPLed.txt | xargs -I '{}' mv '{}' ../melpa-gpl-packages

```




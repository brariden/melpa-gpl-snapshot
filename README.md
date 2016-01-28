
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




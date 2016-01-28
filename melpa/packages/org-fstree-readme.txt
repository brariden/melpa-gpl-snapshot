org-fstree inserts the filesystem subtree for a given directory.
Each file/directory is formatted as a headline, provides links back
to all headlines that are associated with it (by containing links to the file)
and is assigned their tags.

Installation:
  - put this file into your load-path
  - insert "(require 'org-fstree)" into ~/.emacs

Usage:
  - enter a line containing "#+BEGIN_FSTREE: <dir>" into an org buffer,
    where <dir> is the directory, that is to be inserted.
  - while the cursor is in the line mentioned, press "C-c C-c"

Options:
  Specify options in the form:
  "#+BEGIN_FSTREE: <dir> :<optionname1> <optionvalue1> :<optionname2> <optionvalue2>  ...
  Options are:
    - :non-recursive t , to suppress recursion into directories
    - :exclude-regexp-name <list of regexp strings> , exclude file/directory names matching either
                                                 of the given regexp expressions
      Examples:
        :exclude-regexp-name (".*\\.pdf$" ".*\\.zip$"), excludes files/directories ending with either ".pdf" or ".zip"
        :exclude-regexp-name ("^\\.git$") , excludes files/directories named ".git"

    - :exclude-regexp-fullpath <list of regexp strings>, same as :exclude-regexp-name but matches absolute path to file/directory
    - :relative-links t , generates relative instead of absolute links
    - :show-only-matches t , only files that are being linked to show up
    - :only-directories t , only directories are listed
    - :only-regular-files t , only regular files are listed
    - :dynamic-update t , [EXPERIMENTAL] dynamically update a subtree on visibility cycling.
    - :links-as-properties t, sets the links as properties Link1, Link2,... for use in column view [Does not work with dynamic-update!]
    - :no-annotations t, suppresses the search and display of file annotations

Limitations and warnings:

  - when triggering an update (by pressing "C-c C-c" while in the line mentioned above)
    the COMPLETE REGION BETWEEN "#+BEGIN_FSTREE" AND "#+END_FSTREE" IS REPLACED.
  - speed

Code:

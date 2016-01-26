Snakemake mode provides support for editing Snakemake [1] files.  It
builds on Python mode to provide fontification, indentation, and
imenu indexing for Snakemake's rule blocks.

If Snakemake mode is installed from MELPA, no additional setup is
required.  It will be loaded the first time a file named 'Snakefile'
is opened.

Otherwise, put snakemake-mode.el in your `load-path' and add

    (require 'snakemake-mode)

to your initialization file.

[1] https://bitbucket.org/snakemake/snakemake/wiki/browse/

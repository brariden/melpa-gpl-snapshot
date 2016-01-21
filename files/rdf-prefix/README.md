RDF-prefix
==========

RDF-prefix is a package for Emacs which aims to simplify a common task in the
work of RDF developers: remembering and looking up URI prefixes.

## Getting started

RDF-prefix can be installed by downloading `rdf-prefix.el` and
[setting up your load path](http://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html).

Then the package can be loaded by adding the following line to your init
file:

```lisp
(require 'rdf-prefix)
```

Alternatively, it can be installed via [MELPA](https://melpa.org/).

## Usage

This package provides the `rdf-prefix-insert` command, which is used to look
up- and insert URIs based on a prefix.

The prefix list is pulled regularly from http://prefix.cc/.

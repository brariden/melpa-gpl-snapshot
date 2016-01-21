latex-extra [![Build Status](https://travis-ci.org/Bruce-Connor/latex-extra.svg?branch=master)](https://travis-ci.org/Bruce-Connor/latex-extra)
=======

Defines extra commands and keys for LaTeX-mode. To activate, install
from melpa and add the following to your _init.el_ file:

    (add-hook 'LaTeX-mode-hook #'latex-extra-mode)

_Note, this will move the key binds **C-c C-q**, **C-c C-f**, and
**C-c C-p**. To understand why (or disable it) look into the variables
`latex/override-font-map`, `latex/override-fill-map`, and
`latex/override-preview-map`._

The additions of this package fall into the following categories:

## 1-Key Compilation ##

Tired of hitting `C-c C-c RET` 4 times (latex, bibtex, latex, view)
for the document to compile? That's 12 keys! This defines a much
needed command that does **everything** at once, and even handles
compilation errors!

- `C-c C-a` **=>** `latex/compile-commands-until-done`

## Content Folding ##

Similar to how org-mode hides and displays of subtrees, if you hit
**TAB** on a section header _latex-extra_ will hide the contents of
that section for you. Hitting tab twice will expand it again.  
This will **not** interfere with whatever with other keybinds you have
set for **TAB**, such as yasnippet or auto-completion.

**Shift-TAB** will do the same for the entire buffer.

Of course, the same goes for chapters, subsections, etc.

## Navigation ##

Five new keybindings are defined for navigating between
sections/chapters. These are meant to be intuitive to people familiar
with `org-mode`.

- `C-c C-n` **=>** `latex/next-section`  
Goes forward to the next section-like command in the buffer (\part,
\chapter, \(sub)section, or \(sub)paragraph, whichever comes first).
- `C-c C-u` **=>** `latex/up-section`  
Goes backward to the previous section-like command containing this
one. For instance, if you're inside a subsection it goes up to the
section that contains it.
- `C-c C-f` **=>** `latex/next-section-same-level`  
Like `next-section`, except it skips anything that's "lower-level"
then the current one. For instance, if you're inside a subsection it
finds the next subsection (or higher), skipping any subsubsections or
paragraphs.
- `C-M-f` **=>** `latex/forward-environment`  
Skip over the next environment, or exit the current one, whichever
comes first. 
- `C-M-e` **=>** `latex/end-of-environment`  
Exit the current environment, and skip over some whitespace
afterwards. (Like `LaTeX-find-matching-end`, but a little more useful.)
- `C-M-b` **=>** `latex/backward-environment`  
- `C-M-a` **=>** `latex/beginning-of-environment`  
- `C-c C-p` **=>** `latex/previous-section`  
- `C-c C-b` **=>** `latex/previous-section-same-level`  
Same as above, but go backward.

## Whitespace Handling ##

`latex-extra.el` improves `auto-fill-mode` so that it only applies to
text, not equations. To use this improvement, just activate
`auto-fill-mode` as usual.

It also defines a new command:  

- `C-c C-q` **=>** `latex/clean-fill-indent-environment`  
  Completely cleans up the entire current environment. This involves:
  1. Removing extraneous spaces and blank lines.
  2. Filling text (and only text, not equations).
  3. Indenting everything.

## Small User Experience Improvements ##

The buffer used to display LaTeX errors is typically a regular text
buffer in `fundamental-mode`. *latex-extra* switches it to
`special-mode` and adds some colors to the display.

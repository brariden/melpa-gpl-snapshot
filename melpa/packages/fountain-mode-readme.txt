Fountain Mode
=============

Fountain Mode aims to be a full-featured screenwriting environment for GNU Emacs
using the Fountain markup format. For more information on the Fountain markup
format, visit <http://fountain.io>.

Features
--------

- Support for Fountain 1.1 specification
- WYSIWYG auto-align elements (display only, does not modify file contents)
  specific to script format, e.g. screenplay, stageplay or user-defined format
- Export to HTML, LaTeX, Final Draft (FDX), Fountain, or user-defined formats
- Export to standalone document or snippet
- Integration with `outline` to toggle/cycle visibility of sections and scenes
- Integration with `imenu` (sections, scene headings, notes)
- Add/remove automatic continuation string to successively speaking characters
- Navigation by section, scene, or character name
- 3 levels of element syntax highlighting
- Support for both official and legacy commenting (boneyard) syntax
- Include or omit a title page
- Emphasis (bold, italic, underlined text)
- Toggle visibility of emphasis delimiters and syntax characters
- Templates for inserting synopses, notes and metadata
- Everything customizable

Most common features are accessible from the menu. For a full list of functions
and key-bindings, type C-h m.

See the [Wiki][] on GitHub for ways to extend Fountain Mode.

[wiki]: https://github.com/rnkn/fountain-mode/wiki "Fountain Mode wiki"

Requirements
------------

- Emacs 24.4
- [s.el][], the long lost Emacs string manipulation library.
- LaTeX packages for PDF export: `geometry`, `titling`, `fontspec`, `fancyhdr`,
  `marginnote`, `ulem`, `xstring`, `oberdiek`

[s.el]: https://github.com/magnars/s.el "s.el"

Installation
------------

*For users on OS X with no experience with Emacs, see the
[Absolute Beginner's Guide (OS X)][guide].*

The latest stable release of Fountain Mode is available via [MELPA-stable][].

Alternately, download the [latest release][], move the files into your
`load-path` and add the following line to your `.emacs` or `init.el` file:

    (require 'fountain-mode)

If you prefer the latest but perhaps unstable version, install via
[MELPA][], or clone the repository into your `load-path` and require as
above:

    git clone https://github.com/rnkn/fountain-mode.git

To load Fountain Mode whenever you open a `.fountain` file, also add the
following:

    (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

[guide]: https://github.com/rnkn/fountain-mode/wiki/Absolute-Beginner's-Guide-(OS-X) "Absolute Beginner's Guide (OS X)"
[melpa]: http://melpa.org/#/fountain-mode "MELPA"
[melpa-stable]: http://stable.melpa.org/#/fountain-mode "MELPA-stable"
[latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

Bugs and Feature Requests
-------------------------

Please raise an issue on the [Issues][] page on GitHub.

[issues]: https://github.com/rnkn/fountain-mode/issues "Fountain Mode issues"

Roadmap
-------

See [Milestones][] on GitHub.

[milestones]: https://github.com/rnkn/fountain-mode/milestones "Fountain Mode milestones"

History
-------

See [Releases][] on GitHub.

[releases]: https://github.com/rnkn/fountain-mode/releases "Fountain Mode releases"

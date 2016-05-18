=================
Emacs Chapel Mode
=================

.. image:: https://img.shields.io/badge/license-GPL_3-green.svg
           :target: https://www.gnu.org/licenses/gpl-3.0.txt
.. image:: https://melpa.org/packages/chapel-mode-badge.svg
           :target: https://melpa.org/#/chapel-mode
.. image:: https://stable.melpa.org/packages/chapel-mode-badge.svg
           :target: https://stable/melpa.org/#/chapel-mode

This directory contains Emacs e-lisp files that perform simple formatting of Chapel code.  Support for
Chapel formatting with these files is far from perfect, but far better than having no formatting at all (we
find).  Thanks to Steve Balensiefer for providing the initial version of the Chapel mode.

The sources in this directory are licensed under the GPL version 3.  See the gpl-3.0.txt file in this
directory for more information.

Emacs 24 has a packaging system and this mode is available at MELPA. Users of Emacs 23 should install the
package module to achieve the same installation method.

Add MELPA (or MELPA Stable if you want the formal releases rather than a rolling release based on Git
commits).

For those who want rolling releases as they happen use MELPA:

.. code:: elisp

     (require 'package)
     (add-to-list 'package-archives
         '("melpa" . "https://melpa.org/packages/") t)
     (package-initialize)


For those who want only formal, tagged releases use MELPA Stable:

.. code:: elisp

     (require 'package)
     (add-to-list 'package-archives
         '("melpa-stable" . "https://stable.melpa.org/packages/") t)
     (package-initialize)


and then use M-x package-list-package to get to the package listing and install from there. MELPA tracks
this Git repository and updates relatively soon after each commit or formal release. For more detail on
setting up see `MELPA Getting Started <https://melpa.org/#/getting-started>`_.

To use this mode without packaging put the chapel-mode.el file into your ``.emacs.d`` directory and add the
following in your ``.emacs.d/init.el`` file:

.. code:: elisp

   (autoload 'chapel-mode "chapel-mode" "Chapel enhanced cc-mode" t)
   (add-to-list 'auto-mode-alist '("\\.chpl$" . chapel-mode))

This software is licenced using GNU General Public Licence version 3.

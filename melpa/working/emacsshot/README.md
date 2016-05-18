<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#org8897cb0">1. What</a></li>
<li><a href="#orga55783b">2. Usage</a>
<ul>
<li><a href="#org1d6d205">2.1. Quick</a></li>
<li><a href="#org1afdb29">2.2. Hints and Detais</a></li>
<li><a href="#org18c2ad1">2.3. Hide the mode-line</a></li>
</ul>
</li>
<li><a href="#org99848b3">3. Install</a>
<ul>
<li><a href="#org4abbace">3.1. Emacs Package</a></li>
<li><a href="#org3b7c261">3.2. Direct Install</a></li>
</ul>
</li>
<li><a href="#org1ab37be">4. Dependencies</a></li>
<li><a href="#org9f443f4">5. Development</a>
<ul>
<li><a href="#org99510c4">5.1. Lentic Literate Style</a></li>
<li><a href="#org5ec7836">5.2. Ideas, Contributions, Bugs</a></li>
</ul>
</li>
<li><a href="#orgf8277ca">6. Related</a></li>
<li><a href="#org194d4b2">7. History</a></li>
</ul>
</div>
</div>

# What<a id="org8897cb0"></a>

`emacsshot` provides a few commands to take a screenshot of
Emacs from within Emacs.

![img](./emacsshot.png)

# Usage<a id="orga55783b"></a>

## Quick<a id="org1d6d205"></a>

With `emacsshot` there are

-   `M-x emacsshot-snap-frame`
-   `M-x emacsshot-snap-window`
-   `M-x emacsshot-snap-window-exclude-modeline`

for creating a shot of Emacs.

## Hints and Detais<a id="org1afdb29"></a>

With the default settings `M-x emacsshot-snap-frame` creates file
'~/emacsshot.png' which is a snapshot of the current Emacs-frame
with all its windows.

There is also `M-x emacsshot-snap-window` which is for creating a
snapshot of the current Emacs-window (i.e. the window which contains
the active cursor.)

Further there is function `emacsshot-snap-window-exclude-modeline`
which does as `emacsshot-snap-window` but excludes the modeline when
taking the shot.  See also section [Hide the mode-line](#org18c2ad1).

The filenames are configurable.  Hint: `M-x customize-group emacsshot`.

It's also possible to add a timestamp to the filename as postfix.  See
`M-x customize-variable emacsshot-with-timestamp`.

It might be a good idea to bind the functions to a key.  This can
make the usage more convenient.  Further the binding is a way to
avoid images which contain the command that has been used to create
the image e.g. "M-x emacsshot-snap-frame" in the minibuffer.
Beware of the heisenshot!

Concretely the print-key could trigger the shot.  Evaluation of

    (global-set-key [print] 'emacsshot-snap-frame)

yields this behavior.

Or evaluate

    (global-set-key [print]
     (lambda (&optional current-window)
      (interactive "P")
      (if current-window (emacsshot-snap-window)
        (emacsshot-snap-frame))))

to be able to snap the frame by pressing the print-key and to snap the
current window by prefixing the keypress with C-u.

Note that emacsshot currently trys to overwrite any existing file with
the target name without asking.

## Hide the mode-line<a id="org18c2ad1"></a>

If you don't want the mode-line in your emacsshot you can switch it
off with `hidden-mode-line-mode` from Bastien Guerry available at
<http://bzg.fr/emacs-hide-mode-line.html>.

# Install<a id="org99848b3"></a>

## Emacs Package<a id="org4abbace"></a>

When emacsshot has been installed as elpa-package
[![img](http://melpa.org/packages/emacsshot-badge.svg)](http://melpa.org/#/emacsshot) then the functions
are available without need of further action.

## Direct Install<a id="org3b7c261"></a>

Activate this program by loading it into Emacs and evaluate it with
`M-x eval-buffer`.

Automatically activate this program at Emacs start by adding the lines

    (add-to-list 'load-path "/...path to this program...")
    (require 'emacsshot)

to your .emacs or whatever you use for Emacs intitialization.

# Dependencies<a id="org1ab37be"></a>

-   Emacs is running under X.
-   The programm `convert` of the ImageMagick-suite is available.

`convert` actually creates the snapshots.

# Development<a id="org9f443f4"></a>

## Lentic Literate Style<a id="org99510c4"></a>

This program is written in Emacs Lisp in lentic style based on the
'lentic' package [![img](http://melpa.org/packages/lentic-badge.svg)](http://melpa.org/#/lentic).

This means the that this file can be regarded just as an Emacs Lisp
file.  But actually this file contains extra comments which allow the
interpretation of the file as Org file.  Lentic-mode makes it easy to
write this style.

A possible initialization of lentic is this:

    (global-lentic-start-mode)

Find more about lentic at
[![img](http://melpa.org/packages/lentic-badge.svg)](http://melpa.org/#/lentic).

## Ideas, Contributions, Bugs<a id="org5ec7836"></a>

Contributions, ideas and bug-reports are welcome.

Please use the infrastructure of github for communication.  See
<https://github.com/marcowahl/emacsshot/issues>.

# Related<a id="orgf8277ca"></a>

There is elpa-package 'screenshot' which allows to pick windows
with the mouse, even windows from non-Emacs (!) programs.  See
<http://melpa.org/#/screenshot>.  BTW 'screenshot' has even more!

emacsshot only takes images of Emacs.

# History<a id="org194d4b2"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-right">201501071941</td>
<td class="org-left">New function to take snapshot of a window</td>
</tr>


<tr>
<td class="org-right">201505162319</td>
<td class="org-left">Optionally add timestamp to save-filename</td>
</tr>
</tbody>
</table>

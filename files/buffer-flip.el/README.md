buffer-flip.el
=================

This package streamlines the operation of switching between recent
buffers, with an emphasis on minimizing keystrokes.  Inspired by the
Alt-Tab convention in Windows, it keeps the most recently used buffers
on the top of the stack.  Depends on
[`key-chord`](https://melpa.org/#/key-chord).

Installation
------------

[![MELPA](https://melpa.org/packages/buffer-flip-badge.svg)](https://melpa.org/#/buffer-flip)

To install from melpa,

    M-x package-install RET buffer-flip RET

Then add this to your config:

    (key-chord-mode 1) ;; if you're not already enabling key-chord-mode
    (buffer-flip-mode)

Key Bindings
-------------

By default, the key chord to begin flipping through buffers is
<kbd>u8</kbd>.  You can customize this.

<kbd>u</kbd> and <kbd>8</kbd> are roughly analogous to <kbd>Alt</kbd>
and <kbd>Tab</kbd>, respectively.  To begin cycling through the
buffers, press <kbd>u</kbd> and <kbd>8</kbd> at the same time or in
rapid succession, `key-chord` style.  This begins the flipping process
by switching to the most recently used buffer.  At this point,
pressing <kbd>8</kbd> by itself will continue to cycle through the
buffer stack, more recent buffers first.  Pressing <kbd>*</kbd>
(`shift-8` on an English keyboard) will cycle in the opposite
direction.  Just begin working in the current buffer to stop cycling.
Doing so places the current buffer on top of the stack.
<kbd>C-g</kbd> cancels cycling and restores the buffer you were in
before, analagous to <kbd>Esc</kbd> when cycling in Windows.

| Pressing                                               | Does this                                                                         |
|--------------------------------------------------------|-----------------------------------------------------------------------------------|
| <kbd>u8</kbd>                                          | Alternates between the two most recent buffers                                    |
| <kbd>u8</kbd> <kbd>8</kbd>                             | Flip to third most recent buffer                                                  |
| <kbd>u8</kbd> <kbd>8</kbd> <kbd>8</kbd> <kbd>C-g</kbd> | Start flipping through buffers and then cancel, returning to the original buffer. |
| <kbd>u8</kbd> <kbd>8</kbd> <kbd>*</kbd>                | Flips forward through the two most recent buffers, then flips backward one buffer.              |

To customize the keys, use

    M-x customize-variable RET buffer-flip-keys RET

or place this in your configuration:

    (buffer-flip-set-keys 'buffer-flip-keys "u8*")

The first two characters form the key-chord that begins buffer
cycling.  They are automatically registered with `key-chord` when set
using one of the above methods.  The second character pressed on its
own continues cycling in the forward direction.  The third character
cycles in the backward direction.  This would typically be the shifted
version of the second character.  These may not be modifier keys, and
because of a restriction in key-chord, they must be characters between
32 and 126.  Choose a key combination not likely to be used in
succession in normal editing.

The (Non) UI
-------------

Or, "Why don't you have a screenshot?"  This package streamlines the
operation of switching between the most recent buffers, a common
operation in my workflow.  Many buffer management systems display a
list of buffer names for you to select from.  Extra ui elements like
that often come at the cost of additional keystrokes.  This package
doesn't display a list of buffers, it simply changes the current
buffer as you cycle.  Once you are looking at the buffer you want,
just start working and the cycling automatically exits.  Pressing C-g
during cycling will take you back to where you started.

This package is not efficient for switching to a deeply-buried buffer.
There are
[other](http://tuhdo.github.io/helm-intro.html#ID-0386c827-7f5d-4056-bf4d-8d0fc01fc1ab)
[tools](http://www.gnu.org/software/emacs/manual/html_mono/ido.html)
for that.

Motivation
-----------

The [Alt-Tab](https://en.wikipedia.org/wiki/Alt-Tab) convention for
switching windows was one thing Microsoft got right.  Because it keeps
the most recently-used things on the top of the stack, it is often
very fast to switch to the thing you want.  There are
[similar Emacs packages](http://www.emacswiki.org/emacs/ControlTABbufferCycling)
out there, but many are too heavyweight for my needs, involve new UI
elements, or are not stack-based.  `buffer-flip` is very simple and
lightweight -- less than 40 lines of actual code.

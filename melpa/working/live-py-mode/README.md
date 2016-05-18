live-py-plugin
==============

Live coding in Python implemented as an Eclipse plugin or an Emacs minor mode.

To see how to use it, watch the [demo video][video] or read the 
[blog post][blog]. You might also find some useful examples in the 
[tools folder][tools]. To learn more, read about [how it works][how].

![Screenshot of a star diagram][screenshot]

Special thanks to [Antti Kaihola][akaihola] and [Christoph Paulik][cpaulik] for
contributing the Emacs support.

If you like this project, check out some of my [other projects][projects].

[how]: http://donkirkby.github.io/live-py-plugin/howitworks
[screenshot]: https://raw.githubusercontent.com/donkirkby/live-py-plugin/master/screenshot.png
[akaihola]: https://github.com/akaihola
[cpaulik]: https://github.com/cpaulik

Installing the Eclipse plugin
-----------------------------

1. Install the [PyDev plugin][pydev] and Eclipse if you don't already have them.
   It's been tested with PyDev 4.5.5, Eclipse 4.4, Python 2.7 and 3.4. It
   seems to be particularly sensitive to changes in PyDev.
2. In Eclipse, choose Help: Eclipse Marketplace... from the menu.
3. Search for Live Coding in Python, and install it.
4. Restart Eclipse.
5. Open any Python file, and from the Live Coding menu, choose Start Live Coding.
   You should see an extra panel on the right that shows the results of running
   your code.
6. To try the turtle graphics features, open the Window menu, and choose 
   Show View: Other.... Then under PyDev, click Live Coding Canvas and click OK.

If you don't want to use the Eclipse marketplace, you can also install from the
[update site][update].

[update]: http://donkirkby.github.io/live-py-plugin/update

Uninstalling the Eclipse plugin
-------------------------------

1. In Eclipse, choose Help: Installation Details from the menu.
2. Select Live Coding in Python, and click the Uninstall... button.
3. Restart Eclipse.

Installing the Emacs mode
-------------------------
It's probably easiest to use the MELPA package archive, as described in this
section, but the next section gives instructions for installing without MELPA.

1. Install [GNU Emacs][emacs] if you don't already have it.
2. [Install MELPA][melpa]. You probably want to follow the instructions
    for the stable version, instead of the default bleeding-edge version.
3. Launch the package installer with `M-x package-list-packages`.
4. Find the package with `C-s live-py-mode`.
5. Mark the package to install with `i`, then execute the installation with
    `x`.
6. Open any Python file, and activate live-py-mode with `M-x live-py-mode`.
   You should see an extra window on the right that shows the results of running
   your code.

Installing the Emacs mode without MELPA
---------------------------------------
1. Install [GNU Emacs][emacs] if you don't already have it.
2. Clone the latest version of the live-py Emacs mode:

        git clone https://github.com/donkirkby/live-py-plugin.git

3. Copy the Emacs Lisp file and the supporting Python files into a directory
   which is in your Emacs `load-path`. For example:

        cd live-py-plugin
        cp emacs-live-py-mode/live-py-mode.el plugin/PySrc/*.py ~/.emacs.d/

   Add ~/.emacs.d/ to your `load-path` in `~/.emacs.d/init.el` or `~/.emacs`:

        (add-to-list 'load-path "~/.emacs.d")
4. Load the Lisp library in your `~/.emacs.d/init.el` or `~/.emacs`:

        (require 'live-py-mode)
5. Restart Emacs.
6. Open any Python file, and activate live-py-mode with `M-x live-py-mode`.
   You should see an extra window on the right that shows the results of running
   your code.

[melpa]: https://melpa.org/#/getting-started

Uninstalling the Emacs mode
---------------------------
If you installed with MELPA, just use it to uninstall. If not, follow these
steps:

1. Remove the files you copied into `~/.emacs.d/`:
2. Revert additions to `~/.emacs.d/init.el` or `~/.emacs`.
3. Restart Emacs.

Working Features
----------------
- turtle graphics, including filled polygons (Eclipse only)
- local variable assignments
- looping
- function calls, and multiple calls.
- compile errors
- runtime exceptions
- infinite loops halted.
- print statements work.

If you find the project useful, help us [make it better][contributing].

[pydev]: http://pydev.org/download.html
[video]: http://www.youtube.com/watch?v=LV3aFRHlAEQ
[blog]: http://donkirkby.blogspot.ca/2012/11/live-coding-in-python-v2.html
[emacs]: http://www.gnu.org/software/emacs/
[tools]: https://github.com/donkirkby/live-py-plugin/tree/master/test/PySrc/tools
[projects]: http://donkirkby.github.io/
[contributing]: https://github.com/donkirkby/live-py-plugin/blob/master/CONTRIBUTING.md

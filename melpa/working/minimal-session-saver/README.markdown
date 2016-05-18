[![Build Status](https://secure.travis-ci.org/rolandwalker/minimal-session-saver.png?branch=master)](http://travis-ci.org/rolandwalker/minimal-session-saver)

# Overview

Very lean session saver for Emacs.

 * [Quickstart](#quickstart)
 * [Explanation](#explanation)
 * [See Also](#see-also)
 * [Compatibility and Requirements](#compatibility-and-requirements)

## Quickstart

```elisp
(require 'minimal-session-saver)
 
(minimal-session-saver-install-aliases)
 
;; execute M-x mss-store RET
 
;; quit and restart Emacs
 
;; execute M-x mss-load RET
```

## Explanation

The only information stored by this library is a list of visited
files.  Not window configuration, nor point position.

Giving a universal prefix argument to any of the interactive
session-management commands causes a prompt for the session-state
file location, allowing minimal-session-saver to be used as a
(very) minimal project manager.

When `frame-bufs.el` is present, the session associated with a
particular frame can be stored and recovered.

Several interactive commands are provided to manage sessions:

	minimal-session-saver-store
	minimal-session-saver-load
	minimal-session-saver-store-frame      ; requires frame-bufs.el
	minimal-session-saver-load-frame       ; requires frame-bufs.el
	minimal-session-saver-add-buffer
	minimal-session-saver-remove-buffer
	minimal-session-saver-mark-stored-buffers

without keybindings.

An additional command

	minimal-session-saver-install-aliases

installs shorter command aliases for the above, and can
be run at autoload-time through a setting in `customize`.

## See Also

 * <kbd>M-x</kbd> <kbd>customize-group</kbd> <kbd>RET</kbd> <kbd>minimal-session-saver</kbd> <kbd>RET</kbd>

## Compatibility and Requirements

	GNU Emacs version 24.4-devel     : yes, at the time of writing
	GNU Emacs version 24.3           : yes
	GNU Emacs version 23.3           : yes
	GNU Emacs version 22.2           : yes, with some limitations
	GNU Emacs version 21.x and lower : unknown

Uses if present: [frame-bufs.el](https://github.com/alpaker/Frame-Bufs)

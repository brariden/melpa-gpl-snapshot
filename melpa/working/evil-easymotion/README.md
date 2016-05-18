evil-easymotion
===============
[![MELPA](http://melpa.org/packages/evil-easymotion-badge.svg)](http://melpa.org/#/evil-easymotion)

In which we give [Lokaltog/**vim-easymotion**](https://github.com/Lokaltog/vim-easymotion) a streak of malevolence.

Suppose you're a proud user of `evil`, but you have a nasty habit: when you want to move ten lines down, you spam <kbd>j</kbd> ten times instead of using the ergonomically superior <kbd>10</kbd><kbd>j</kbd>. With `evil-easymotion` you can invoke <kbd>SPC</kbd><kbd>j</kbd>, and this plugin will put a target character on every possible position. Type the character on the target and **_wham_**! you have teleported there.

**Obligatory screencast**

![screencast](img/evil-easymotion-demo.gif)

Currently most motions are supported, and it's easy to define your own easymotions. Easymotions can also be composed with operators e.g. <kbd>d</kbd><kbd>SPC</kbd><kbd>j</kbd> to delete a bunch of lines.

Notice
======
The `evil-easymotion` API has changed to use keyword arguments. The required changes (if any) to your config should be pretty self-explanatory. This should allow me to add more options in the future without breaking your config.

Basic Usage
===========
To define easymotions for all motions that evil defines by default, add
```emacs
(evilem-default-keybindings "SPC")
```
This binds all motions under the prefix `SPC` in `evil-motion-state-map`. This is not done by default for motions defined manually. You will need to supply the prefix.

Or, if you prefer more granular control:
```emacs
(evilem-define (kbd "SPC w") 'evil-forward-word-begin)
```

You can always drop by [the wiki](https://github.com/PythonNut/evil-easymotion/wiki) for more tips.


Building Your Own Motions
=========================

### Collectors

Where present, `collectors` is used as the motion specification for generating jump points. It may be an (optionally sharp) quoted function name, a lambda, or a list of any of the above. If multiple functions are provided, the collected points are merged and presented at once.

### Macros

`evil-easymotion` provides four user-facing macros:


* `(evilem-make-motion-plain name collectors &key ...)`

Produce a function, named `name`, from `collectors`.

* `(evilem-make-motion name collectors &key ...)`

Like `evilem-make-motion-plain`, but produce an evil motion produced with `evil-define-motion` instead of a plain `defun`.

* `(evilem-create-plain collectors &key ...)`

Like `evil-make-motion-plain`, but with an automatically generated `name`.

* `(evilem-create collectors &key ...)`

`evilem-create` : `evil-evilem-make-motion` :: `evilem-create-plain` : `evilem-make-motion-plain`


* `(evilem-define key collectors)`

Like `evilem-create`, but also bind the generated function to `key` in the relevant maps (either `evil-normal-state` or `evil-motion-state`, depending on other flags).

### Keyword arguments

In addition, various keyword arguments may be used to modify the behavior of the easymotion.

* `:pre-hook expr`

Code to run before the easymotion executes. `expr` may either be an (optionally sharp) quoted function name, a lambda, or a bare sexp, which is implictly wrapped in a lambda.

* `:post-hook expr`

Like `:pre-hook expr`, but code is run after the motion finishes.

* `:bind forms`

A list of forms to bind around the entire easymotion. `forms` may be any bindings accepted by [`cl-letf*`](http://www.gnu.org/software/emacs/manual/html_node/cl/Modify-Macros.html).

* `:scope object`

An object to limit the scope of an easymotion. `object` may be any *thing* understood by `thing-at-point`. In practice, `object` will often be `'line`.

* `:all-windows expr`

If `expr` is non-`nil`, the motion will be executed in all visible windows simultaneously. Because evil motions do not make sense across windows, `evil-define-command` is used instead of `evil-define-motion` and `evil-normal-state-map` is used instead of `evil-motion-state-map`.

* `:initial-position callable`

When specified, `(goto-char (funcall callable))` is run before the motion is executed. For example, use this to jump to the BOL of each line as in easymotion with `:initial-position #'point-at-bol`. Unlike in `:pre-hook`, `callable` is run once per window when `:all-windows` is specified.

Credits
=======
I'm deeply indebted to:
* [abo-abo](https://github.com/abo-abo) for authoring [`avy`](https://github.com/abo-abo/avy), on which this package depends.
* [Lokaltog](https://github.com/Lokaltog) for authoring [`vim-easymotion`](https://github.com/Lokaltog/vim-easymotion) and creating the paradigm which this package emulates.

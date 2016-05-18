# ansi.el [![Build Status](https://api.travis-ci.org/rejeep/ansi.el.png?branch=master)](http://travis-ci.org/rejeep/ansi.el) [![Coverage Status](https://img.shields.io/coveralls/rejeep/ansi.el.svg)](https://coveralls.io/r/rejeep/ansi.el)

This package defines functions that turns simple strings to ansi
strings. Turning a string into an ansi string can be to add color to a
text, add color in the background of a text or adding a style, such as
bold, underscore or italic.

## Installation

Add `ansi` to your [Cask](https://github.com/rejeep/cask.el) file:

```lisp
(depends-on "ansi")
```

## Usage

All colors, background colors and styles can be accessed via a
function with the `ansi-` prefix. For example:

```lisp
;; Color text
(ansi-red "foo")
(ansi-blue "bar")

;; Color background
(ansi-on-red "foo")
(ansi-on-blue "bar")

;; Add style
(ansi-bold "foo")
(ansi-blink "bar")
```

It can become quite cumbersome to use those function names if the
coloring is a little bit more advanced. To simplify this, there's a
DSL, which makes this much more pleasant. If within a `with-ansi`
block, the `ansi-` prefix is not necessary anymore. This is the same
as the above.

```lisp
;; Color text
(with-ansi
 (red "foo")
 (blue "bar"))

;; Color background
(with-ansi
 (on-red "foo")
 (on-blue "bar"))

;; Add style
(with-ansi
 (bold "foo")
 (blink "bar"))
```

You can also use `ansi-apply` if you only have the name of the color:

```lisp
(ansi-apply 'red "foo %s" "bar")
```

### CSI

Ansi supports CSI codes, for example moving the cursor:

```lisp
(ansi-up)
(ansi-down 3)
(ansi-csi-apply 'forward)
(ansi-csi-apply "J" 10)
```

### Nesting

Lets say you want to make a text bold, red and blinking. You can do
this with nesting.

```lisp
;; Without DSL
(ansi-bold
 (ansi-red (ansi-blink "foo bar")))

;; Using DSL
(with-ansi
 (bold (red (blink "foo bar"))))
```

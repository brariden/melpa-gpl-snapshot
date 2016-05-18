# FIFO-CLASS

Stands for first in first out.
An eieio abstract class class to provide FIFO methods to slots.

## Requriements

* Emacs 24

## Usage

A demo class:

```elisp
(require 'fifo-class)
;; Require this package

(defclass testclass (fifo-class)
  ((testlist
    :initform nil)))
;; A testclass have only one slot

(defvar my-object (make-instance 'testclass))
;; Make an instance of testclass and store in my-object
```

### `(fifo-class-push object slot value)`

To push `value` into `slot` quene of `object`.

```elisp
(fifo-class-push my-object 'testlist 0)
;; testlist = '(0)

(fifo-class-push my-object 'testlist 1)
;; testlist = '(0 1)

(fifo-class-push my-object 'testlist 2)
;; testlist = '(0 1 2)

(fifo-class-push my-object 'testlist 3)
;; testlist = '(0 1 2 3)
```

### `(fifo-class-first object slot)`

Get the fisrt element in slot without changing the slot.

```elisp
(fifo-class-first my-object 'testlist)
;; ==> 0
;; testlist = '(0 1 2 3)

(fifo-class-first my-object 'testlist)
;; ==> 0
;; testlist = '(0 1 2 3)
```

### `(fifo-class-pop object slot)`

Remove the first element in `slot` and return that item.

```elisp
(setq a (fifo-class-pop my-object 'testlist))
;; a = 0
;; testlist = '(1 2 3)

(setq b (fifo-class-pop my-object 'testlist))
;; b = 1
;; testlist = '(2 3)

(setq c (fifo-class-pop my-object 'testlist))
;; c = 2
;; testlist = '(3)

(setq d (fifo-class-pop my-object 'testlist))
;; d = 3
;; testlist = nil

(setq e (fifo-class-pop my-object 'testlist))
;; d = nil
;; testlist = nil
```

_______________________________
<br>

## Contacts

mola@molamola.xyz

If you find any bugs or have any suggestions, you can make a pull request or send me an email.

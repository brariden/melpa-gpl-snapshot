(defpackage #:slynk-hello-world
  (:use :cl #:slynk-api)
  (:export
   #:hello-world))
(in-package #:slynk-hello-world)

(defslyfun hello-world ()
  "Provide hello-worldish functionality for the Emacs side of SLY"
  (let ((random-feature (nth (random (length *features*)) *features*)))
    (list
     (format nil "Hello world, did you know your ~a supports ~a?"
             (lisp-name)
             random-feature)
     random-feature)))

(slynk-backend:definterface lisp-name ()
  "Identify the current lisp for hello-wordish purposes"
  (format nil "lisp"))

#+sbcl
(slynk-backend:defimplementation lisp-name () "SBCL")

#+allegro
(slynk-backend:defimplementation lisp-name () "ALLEGRO")

(provide 'slynk-hello-world)

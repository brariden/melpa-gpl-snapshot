;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, Phillip Lord, Newcastle University
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
;; for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

(ns tawny.fixture
  (:use [tawny.owl])
  (:require [tawny.reasoner :as r])
  (:require [clojure.test]))

;; not technically a fixture, but make with-probe-axioms print out nice
(defmethod clojure.test/assert-expr 'tawny.owl/with-probe-axioms [msg form]
  `(tawny.owl/with-probe-axioms
     ;; the test ontology
     ~(second form)
     ;; the vector
     ~(nth form 2)
     ;; the test
     ~(clojure.test/assert-expr
       (str "Using with-probe-axioms:" msg)
       (nth form 3))))

(defmethod clojure.test/assert-expr 'tawny.owl/with-probe-entities [msg form]
  `(tawny.owl/with-probe-entities
     ~(second form)
     ~(nth form 2)
     ~(clojure.test/assert-expr
       (str "Using with-probe-entities:" msg)
       (nth form 3))))

(defmethod clojure.test/assert-expr 'let [msg form]
  `(let
     ~(nth form 1)
     ~(clojure.test/assert-expr
       (str "Using let:" msg)
       (cons 'do
             (drop 2 form)))))

(defmethod clojure.test/assert-expr 'do [msg form]
  `(do
     ~(butlast form)
     ~(clojure.test/assert-expr
       (str "Using do:" msg)
       (last form))))

(defn reasoner
  "Fixture which sets up the reasoner factory to be used, and makes
turns the progress monitor off."
  [reasoner]
  (fn [tests]
    (r/reasoner-factory reasoner)
    (binding [r/*reasoner-progress-monitor*
              (atom r/reasoner-progress-monitor-silent)]
      (tests))))

(defn ontology-and-reasoner
  "Returns a fixture which sets o as the current ontology
and defines the reasoner factory to use."
  [o reasoner]
  (fn [tests]
    ;; this is manipulating global state
    (ontology-to-namespace o)
    ((tawny.fixture/reasoner reasoner) tests)))

(defn namespace-and-reasoner
  "Returns a fixture which sets the ontology from the namespace ns and defines
the reasoner factory to use. ns should be a symbol"
  ([ns reasoner]
     (fn [tests]
       (let [o (get @ontology-for-namespace (find-ns ns))]
         ((ontology-and-reasoner o reasoner) tests)))))

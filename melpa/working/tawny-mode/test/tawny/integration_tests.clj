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

(ns
    ^{:doc "Tests which cut across tawny and could be anywhere"}
  tawny.integration-tests
  (:use [clojure.test])
  (:require [tawny.reasoner :as r]
            [tawny.owl :as o]
            [tawny.fixture]))


(use-fixtures :once (tawny.fixture/reasoner :jfact))

(deftest only-equivalent
  (is
   (do
     (let [ont
           (o/ontology :iri "http://onlyness")
           A
           (o/owl-class ont "A")
           B
           (o/owl-class ont "B")
           R
           (o/object-property
            ont "R" :domain A :range B)
           C
           (o/owl-class
            ont "C" :equivalent (o/only R B))]
       (r/iequivalent-class? ont C (o/owl-thing))))))

(deftest bottom
  (is
   (let [ont (o/ontology :iri "http://bottom")
         bottom (o/individual ont "bottom" :type (o/owl-nothing))]
     (not
      (r/consistent? ont)))))

;; The contents of this file are subject to the LGPL License, Version 3.0

;; Copyright (C) 2013, 2014, Phillip Lord, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

(ns tawny.lookup-test
  (:use [clojure.test])
  (:require
   [tawny.owl :as o]
   [tawny.lookup :as l]))

(def lookup-test-namespace (create-ns 'tawny.lookup-test-test-namespace))

(defn test-ontology
  ([]
   (let [o (o/ontology :iri "http://iri/" :prefix "test:")]
     o)))

;; test it don't crash -- all I can do
(deftest all-iri-to-var
  (is (do
        (l/all-iri-to-var)
        true)))

(deftest iri-to-var
  (is
   (= 1
      (let [o
            (test-ontology)]
        (try
          (o/intern-owl-string lookup-test-namespace "a"
                               (o/owl-class o "a"))
          (count (l/iri-to-var-no-ontology lookup-test-namespace))
          (finally
            (ns-unmap lookup-test-namespace 'a)
            (.removeOntology (tawny.owl/owl-ontology-manager) o)
            (ns-unmap lookup-test-namespace 'a-test-ontology)))))))

(deftest iri-to-var-2
  (is
   (= 1
      (let [o (test-ontology)]
        (try
          (o/intern-owl-string lookup-test-namespace "a"
                               (o/owl-class o "a"))
          (count (l/iri-to-var lookup-test-namespace))
          (finally
            (ns-unmap lookup-test-namespace 'a)
            (.removeOntology (tawny.owl/owl-ontology-manager) o)
            (ns-unmap lookup-test-namespace 'a-test-ontology)))))))

(deftest resolve-entity
  (is
   (= "tawny.lookup-test-test-namespace/hello"
      (try
        (let [o (test-ontology)]
          (tawny.owl/intern-owl-string lookup-test-namespace
                                       "hello" (o/owl-class o "test"))
          (l/resolve-entity (o/owl-class o "test")
                            (l/iri-to-var lookup-test-namespace)))
        (finally
          (ns-unmap lookup-test-namespace 'hello))))))

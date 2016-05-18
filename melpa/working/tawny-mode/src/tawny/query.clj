;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, 2014, Phillip Lord, Newcastle University
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
    ^{:doc "Enable querying over ontologies."
      :author "Phillip Lord"}
    tawny.query
  (:use [tawny.owl])
  (:use [tawny.render])
  (:require [tawny.util])
  (:require [clojure.core.logic :as l]))

(defn into-map
  "Translates an owl object into a clojure map.
The map is similar to the form used to define an entity. Keys are the keywords
used by tawny.owl (:subclass, :domain, etc). Value are sets. Each element of
the set is either an OWL object, or, if it is a restriction, similar to that
used to define a restriction but with OWLObjects instead of clojure symbols.
In addition a :type key is added which describes the type of the object."
  [owlobject]
  (let [render
        (as-form owlobject :keyword :object)]
    (apply hash-map
           (concat
            [:type (list (first render))]
            (drop 2 render)))))

(defn into-map-with
  "As into-map but merges result from other entities retrieved by (f entity).
For example (into-map-with superclasses A) will retrieve all data for A and
its superclasses. No attempt is made to ensure that the semantics of this data
makes sense; for instance, while object restrictions which apply to a
superclass also apply to the subclass, annotations do not; both will be
present in the final map, however."
  [f entity]
  (apply merge-with clojure.set/union
         (map #(dissoc (into-map %) :type)
              (filter iriable?
                      (conj (f entity)
                            entity)))))



(defn frameo
  "Searches a specific frame in a rendered OWL entity to see if query matches
partially. So, for example, we might search the :type frames to see if it
contains at least the list [type] where type is specific or a logic var.
Normally, matcher would be used in preference."
  [entity query frame]
  (l/fresh [a]
         (l/featurec
          entity
          {frame a})
         (l/everyg
          #(l/membero %1 a)
          ;; this get isn't going to work in general because we could have a
          ;; logic var
          (get query frame))))

(defn matcher
  "Returns a goal which matches a rendered OWL entity against the query.
Matchs happen if the entity contains AT LEAST one entity in frame
values (which are themselves lists) to match ALL frames in the query.
The keys of the query cannot be logic vars but everything else can."
[entity query]
  (l/everyg
   #(frameo entity query %1)
   ;; this keys isn't going to work because we could have a logic var
   (keys query)))


(defn typeo
  "Returns a goal which matches entity on type."
  [entity type]
  (matcher
   entity
   {:type [type]}))

(defn supero
  "Returns a goal which matches entity superclasses."
  [entity super]
  (matcher
   entity
   {:super [super]}))

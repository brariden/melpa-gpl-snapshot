(ns nrepl-profile.plugin
  (:require [clojure.java.io :as io]))

(defn- version []
  (let [v (-> (io/resource "META-INF/leiningen/thunknyc/nrepl-profile/project.clj")
              slurp
              read-string
              (nth 2))]
    (assert (string? v)
            (str "Something went wrong, version is not a string: "
                 v))
    v))

(defn middleware [project]
  (-> project
      (update-in [:dependencies]
                 (fnil into [])
                 [['thunknyc/nrepl-profile (version)]])
      (update-in [:repl-options :nrepl-middleware]
                 (fnil into [])
                 '[nrepl-profile.core/wrap-profile])))

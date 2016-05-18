(ns leiningen.ritz-in
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [leiningen.ritz :as ritz])
  (:import (java.security MessageDigest)))

(def ^:private payloads-file-name "ritz_elisp_payloads.clj")

(defn elisp-payload-files []
  ;; TODO: this may not work with lein2 plugins
  (->> (.getResources (.getContextClassLoader (Thread/currentThread))
                      payloads-file-name)
       (enumeration-seq)
       (map (comp read-string slurp))
       (apply concat)
       (set)))

(defn hex-digest [file]
  (format "%x" (BigInteger. 1 (.digest (MessageDigest/getInstance "SHA1")
                                       (-> file io/resource slurp .getBytes)))))

(defn loader [resource]
  (let [feature (second (re-find #".*/(.*?).el$" resource))
        checksum (subs (hex-digest resource) 0 8)
        filename (format "%s-%s" feature checksum)
        basename (-> (System/getProperty "user.home")
                     (io/file ".emacs.d" "swank" filename)
                     (.getAbsolutePath)
                     (.replaceAll "\\\\" "/"))
        elisp (str basename ".el")
        bytecode (str basename ".elc")
        elisp-file (io/file elisp)]
    (when-not (.exists elisp-file)
      (.mkdirs (.getParentFile elisp-file))
      (with-open [r (.openStream (io/resource resource))]
        (io/copy r elisp-file))
      (with-open [w (io/writer elisp-file :append true)]
        (.write w (format "\n(provide '%s-%s)\n" feature checksum))))
    (format "(when (not (featurep '%s-%s))
               (if (file-readable-p \"%s\")
                 (load-file \"%s\")
               (byte-compile-file \"%s\" t)))"
            feature checksum bytecode bytecode elisp)))

(defn ritz-in
  "Jack in to a ritz backed Clojure SLIME session from Emacs.

This task is intended to be launched from Emacs using M-x clojure-jack-in. You
will need to customise the clojure-swank-command variable in clojure-mode to
use the ritz-in task, rather than jack-in."
  [project port]
  (println ";;; Bootstrapping bundled version of SLIME; please wait...\n\n")
  (let [loaders (string/join "\n" (map loader (elisp-payload-files)))
        colors? (.contains loaders "slime-frame-colors")]
    (println loaders)
    (println "(sleep-for 0.1)")         ; TODO: remove
    (println "(run-hooks 'slime-load-hook) ; on port" port)
    (println ";;; Done bootstrapping.")
    (ritz/ritz project port "localhost" ":colors?" (str colors?)
               "--message" "\";;; proceed to jack in\"")))

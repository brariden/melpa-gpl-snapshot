(ns ritz.debugger.executor
  "Executor for ritz debugger"
  (:require
   [ritz.logging :as logging])
  (:import
   java.io.IOException
   (java.util.concurrent
    Callable Future ExecutorService Executors TimeUnit
    ThreadFactory
    CancellationException ExecutionException TimeoutException)))

(def ritz-executor-group-name "ritz-executor")
(def ritz-executor-group (delay (ThreadGroup. ritz-executor-group-name)))
(def ritz-control-group (delay (ThreadGroup. "ritz-control")))

(defn thread-factory
  [^ThreadGroup thread-group prefix]
  (proxy [ThreadFactory] []
     (newThread [^Runnable r]
       (let [thread (Thread. thread-group r)]
         (.setName thread (str prefix "-" (.getId thread)))
         (.setDaemon thread true)
         thread))))

(defonce ^{:private true
           :doc "Use own pool to prevent any interaction with user pools"
           :tag ExecutorService}
  executor
  (Executors/newCachedThreadPool
   (thread-factory @ritz-control-group "ritz-ctrl")))

(defn execute
  [^Callable f]
  (.submit executor f))


(defonce ^{:private true
           :doc "request handler pool"
           :tag ExecutorService}
  request-executor
  (Executors/newCachedThreadPool
   (thread-factory @ritz-executor-group "ritz")))

(defn ^Future execute-request
  [^Callable f]
  (.submit request-executor f))


(defn- default-exception-handler
  [^java.lang.Throwable cause name]
  (when-not (or (instance? IOException cause)
                (instance? java.lang.InterruptedException cause)
                (instance? java.nio.channels.ClosedByInterruptException cause))
    (format
     "%s: exception occured: %s %s"
     name (pr-str cause)
     (with-out-str
       (.printStackTrace cause *out*)))))

(defn- root-cause
  "Return the deepest root cause of a Throwable"
  [throwable]
  (loop [^Throwable cause throwable]
    (if-let [cause (.getCause cause)]
      (recur cause)
      cause)))

(defn ^Future execute-loop
  "Execute a loop continuously. Catch exceptions and return a formatted
  exception message with exception-f."
  [f & {:keys [name exception-f final-fn]
        :or {exception-f default-exception-handler
             name (pr-str f)}}]
  (execute
   (fn []
     (.setName (Thread/currentThread) name)
     (if-let [msg (try
                    (logging/trace
                     "executor/execute-loop %s %s"
                     name (.getName (Thread/currentThread)))
                    (f)
                    (logging/trace "executor/execute-loop %s did one" name)
                    nil
                    (catch Exception exception
                      (exception-f (root-cause exception) name)))]
       (do
         (if final-fn
           (final-fn)
           (logging/trace "executor/execute-loop exiting %s" name))
         msg)
       (recur)))))

(defn daemon-thread-run
  [name ^Runnable f]
  (doto (Thread. f)
    (.setDaemon true)
    (.setName name)
    (.start)))

(defmacro daemon-thread
  [name & body]
  `(daemon-thread-run ~name (fn [] ~@body)))

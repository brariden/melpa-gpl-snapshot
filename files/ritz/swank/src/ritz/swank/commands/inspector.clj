(ns ritz.swank.commands.inspector
  (:require
   [ritz.debugger.break :as break]
   [ritz.swank.connection :as connection]
   [ritz.swank.inspect :as inspect]
   [ritz.swank.messages :as messages])
  (:use
   [ritz.debugger.connection :only [vm-context]]
   [ritz.debugger.inspect :only [reset-inspector]]
   [ritz.swank.commands :only [defslimefn]]))


(defslimefn init-inspector [connection string]
  (let [inspector (connection/inspector connection)
        vm-context (vm-context connection)]
    (reset-inspector connection)
    (inspect/inspect-object inspector (eval (read-string string)))
    (messages/inspector (inspect/display-values vm-context inspector))))

(defslimefn inspect-nth-part [connection index]
  (let [inspector (connection/inspector connection)
        vm-context (vm-context connection)]
    (inspect/inspect-object
     inspector (inspect/nth-part vm-context inspector index))
    (messages/inspector (inspect/display-values vm-context inspector))))

(defslimefn inspector-range [connection from to]
  (let [inspector (connection/inspector connection)
        vm-context (vm-context connection)]
    (inspect/content-range vm-context inspector from to)))

(defslimefn inspector-call-nth-action [connection index & args]
  (let [inspector (connection/inspector connection)
        vm-context (vm-context connection)]
    (when (inspect/call-nth-action vm-context index args)
      (messages/inspector (inspect/display-values vm-context inspector)))))

(defslimefn inspector-pop [connection]
  (let [inspector (inspect/pop-inspectee (connection/inspector connection))]
    (when (inspect/inspecting? inspector)
      (let [[level-info level] (break/break-level-info
                                connection (:request-thread connection))
            vm-context (vm-context connection)
            thread (or (:thread level-info) (:control-thread vm-context))
            vm-context (assoc vm-context :current-thread thread)]
        (messages/inspector (inspect/display-values vm-context inspector))))))

(defslimefn inspector-next [connection]
  (messages/inspector
   (inspect/next-inspectee
    (connection/inspector connection))))

(defslimefn inspector-reinspect [connection]
  (let [inspector
        (connection/inspector connection)]
    (messages/inspector (inspect/reinspect inspector))))

(defslimefn quit-inspector [connection]
  (reset-inspector connection)
  nil)

(defslimefn describe-inspectee [connection]
  (inspect/describe-inspectee (connection/inspector connection)))

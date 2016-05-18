(ns ritz.swank.socket-server
  "Socket server."
  (:require
   [ritz.debugger.executor :as executor]
   [ritz.logging :as logging]
   [ritz.swank.rpc-socket-connection :as rpc-socket-connection]
   clojure.main)
  (:import
   java.net.ServerSocket
   java.net.InetAddress
   (java.io
    File
    Reader InputStreamReader BufferedReader PushbackReader StringReader
    Writer OutputStreamWriter BufferedWriter PrintWriter StringWriter
    IOException)))

(def default-announce-msg "Swank server listening on local port")

(defn announce-port-to-out
  ([port msg]
     (println (or msg default-announce-msg) port))
  ([port]
     (announce-port-to-out port default-announce-msg)))

(defn announce-port-to-file
  "Writes the given port number into a file."
  ([port ^String file]
     (with-open [out (new java.io.FileWriter file)]
       (doto out
         (.write (str port "\n"))
         (.flush)))))

(defn accept-connection
  [f ^ServerSocket socket options]
  (logging/trace "accept-connection")
  (if (.isClosed socket)
    (throw (IllegalArgumentException. "Accept requested on closed socket"))
    (let [connection (.accept socket)]
      (logging/trace "accept-connection: starting connection")
      (executor/execute
       #(do
          (.setName
           (Thread/currentThread)
           (str "Accept-Connection-" (.getId (Thread/currentThread))))
          (f (rpc-socket-connection/create connection options) options))))))

(defn ^ServerSocket server-socket
  "Open the server socket"
  [{:keys [port backlog host] :or {port 0 backlog 0}}]
  (logging/trace "socket-server/server-socket port %d" port)
  (ServerSocket.
   port backlog
   (when host (if (instance? InetAddress host)
                host
                (InetAddress/getByName host)))))

(def acceptor-atom (atom nil))
(def acceptor-port (atom nil))

(defn start-server
  "Start the server and write the listen port number to
   PORT-FILE. This is the entry point for Emacs."
  [connection-f port-file {:keys [announce join log-level]
                           :or {join true announce :default}
                           :as options}]
  (logging/set-level (when-let [level (:log-level options)]
                       (keyword level)))
  (logging/trace "socket-server/start-server")
  (logging/trace "*compile-path* %s" *compile-path*)
  (when *compile-path*
    (alter-var-root #'*compile-path* (constantly *compile-path*)))
  (when (nil? connection-f)
    (throw (IllegalArgumentException. "Null connection function")))
  (let [socket (server-socket options)
        closer #(when (not (.isClosed socket))
                  (logging/trace "closing acceptor socket")
                  (.close socket))
        port (reset! acceptor-port (.getLocalPort socket))
        connection-handler (connection-f)]
    (logging/trace "Socket acceptor on port %s" port)
    (try
      (let [acceptor (executor/execute-loop
                      #(accept-connection connection-handler socket options)
                      :name "Accept loop"
                      :final-fn closer)]
        (reset! acceptor-atom acceptor)
        (or (and announce (announce port))
            (and (= announce :default)
                 (do
                   (announce-port-to-file port port-file)
                   (announce-port-to-out port (:message options)))))
        [socket acceptor]
        (when join
          (.get acceptor)))
      (finally
       (when join
         (closer)
         (logging/trace "socket server closed"))))))

(defn start
  "Start the given server wrapped in a repl. Use this to embed swank in your
   code."
  [{:keys [port encoding server-ns backlog host port-file]
    :or {server-ns 'ritz.swank.proxy}
    :as options}]
  (logging/trace "socket-server/start")
  @executor/ritz-executor-group
  @executor/ritz-control-group
  (let [stop (atom false)
        options (-> options
                    (update-in
                     [:encoding]
                     #(or %
                          (System/getProperty "swank.encoding")
                          "iso-latin-1-unix"))
                    (dissoc :server-ns))]
    (require server-ns)
    (clojure.main/repl
     :read (fn [rprompt rexit]
             (if @stop
               rexit
               (do (reset! stop true)
                   `(start-server
                     (ns-resolve
                      '~server-ns
                      (symbol "serve-connection"))
                     (or ~port-file
                         (-> "java.io.tmpdir"
                             (System/getProperty)
                             (File. "slime-port.txt")
                             (.getCanonicalPath)))
                     '~options))))
     :need-prompt (constantly false))))

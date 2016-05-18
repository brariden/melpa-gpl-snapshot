;;; thread.el ---  -*- lexical-binding: t; -*- 

(require 'fifo)
(require 'sign)
(require 'thread-packet)
(require 'thread-socket)

(defgroup thread nil
  "Group for thread."
  :group nil)

(defcustom thread-limit 100
  "Maximum number of threads."
  :group 'thread)

(defconst thread--proc "thread"
  ;; Process name for thread server.
  "Private variable. Modifying it may cause serious problem.")

(defvar thread--record
  ;; Save a record of threads whether they are occupied or free
  ;; format '((0 . <process0>) (1 . nil) (2 . <process3>)....)
  (mapcar 'list
          (number-sequence 0 (- thread-limit 1)))
  "Private variable. Modifying it may cause serious problem.")




(defvar thread--outbound-connect
  ;; Initialize a connection to outbound of thread.socket
  (and (sign-connect :sign 'thread-socket--outbound-signal
                     :worker 'thread--process-outbound-data)
       'thread--process-outbound-data)
  "Private variable. Modifying it may cause serious problem.")

(defvar thread--inbound-connect
  ;; Initialize a connection to inbound of thread.socket
  (and (sign-connect :sign 'thread-socket--inbound-signal
                     :worker 'thread--process-inbound-data)
       'thread--process-inbound-data)
  "Private variable. Modifying it may cause serious problem.")




(defconst thread--local-listener-proc-name "thread-listener"
  ;; Process name for thread local listener.
  "Private variable. Modifying it may cause serious problem.")

(defvar thread--local-listener nil
  ;; Store the local network subprocess
  ;; The listener is a loacl server
  ;; that used to receive data from child threads.
  "Private variable. Modifying it may cause serious problem.")

(defvar thread--local-port nil
  ;; Port number of the thread--local-listener.
  "Private variable. Modifying it may cause serious problem.")

(defvar thread--listener-buffer '(0)
  ;; Buffer for imcomplete received data
  ;; Minimum need to have a list for nconc 
  "Private variable. Modifying it may cause serious problem.")





(defcustom thread-debug-buffer-name "*thread log*"
  "The buffer name for echo from thread.
This is for debug purpose."
  :group 'thread)

(defcustom thread-debug-p nil
  ;; It should be nil when release!!!
  "Whether messages from thread are printed to buffer."
  :group 'thread)

(defvar thread--debug-print-inbound-packet nil
  ;; It should be nil when release!!!
  ;; A variable that have no effect after compiled.
  ;; It is used by maintainer to debug the inbound packet before compiled.
  ;; If this is t, inbound packet will be printed to thread log.
  "A variable that have no effect after compiled.")

(defvar thread--debug-print-outbound-packet nil
  ;; It should be nil when release
  ;; A variable that have no effect after compiled.
  ;; It is used by maintainer to debug the packet before compiled.
  ;; If this is t, outbound packet will be printed to thread log.
  "A variable that have no effect after compiled.")

(cl-defmacro thread--debug-print-packet (packet &key inbound)
  ;; It is used by maintainer to debug the inbound packet before compiled.
  "A macro that is useless after compiled."
  
  (when (or (and thread--debug-print-inbound-packet inbound)
            (and thread--debug-print-outbound-packet (null inbound)))
    `(thread-debug-print
      (format "thread%d~%s~~ %s"
              (thread.getid thread)
              (or (and ,inbound "IN") "OUT")
              (prin1-to-string ,packet)))))





(defcustom thread-kill-emacs-close-thread-delay 5
  "The time waited for threads to quit safely before closing emacs."
  :group 'thread)


(defsign thread--kill-emacs-signal
  ;; Singal to be emitted after kill-emacs has been invoked.
  ;; More accurately, kill-emacs is adviced around by thread--kill-emacs
  ;; and the signal is emitted by thread--kill-emacs.
  "Private signal. Modifying it may cause serious problem.")



(defconst thread--child-thread-quene-folder
  (file-name-as-directory (concat (file-name-as-directory user-emacs-directory) "thread"))
  ;; This is shit!!!!
  ;; If there is other methods please let me know.
  ;; In the finally state of development, I tested every functions.
  ;; I found that when multiple threads tried to send large data back to the parent,
  ;; there is packet lost.
  ;; It means that different child thread need to quene up their sending.
  ;; The only way I could think of quening child threads is by using files.....
  ;; ╭∩╮（￣.￣）╭∩╮
  
  "Private signal. Modifying it may cause serious problem.")










(cl-defun thread.get (&key name persist)

  "Create a new thread and the thread is returned.
NAME specified the name of the thread.
It does nothing but to let you identitfy the thread when calling
`list-process' of `process-list'.
PERSIST stated whether the thread should be persisted.
If it is nil, after a single instruction,
the thread quits automatically.
If it is t, the thread persists and you are responsible
for quiting the thread either by `thread.quit'(better)
or `thread.forceQuit'."
  
  ;; Start the local listener
  (unless (and thread--local-listener (process-live-p (get-process thread--local-listener)))
    (unless (setq thread--local-listener
                  (make-network-process :name thread--local-listener-proc-name
                                        :host 'local
                                        :server t
                                        :service t
                                        :family 'ipv4
                                        :filter 'thread--listener-receive-data))
      (error "Fail to create a data listener thread."))
    
    ;; Wait until thread--local-listener ready
    (while (null (eq  (process-status thread--local-listener) 'listen)) nil)
    (setq thread--local-port (process-contact thread--local-listener :service)))

  (catch 'thread-exceed-limit
    (let* ((thread-num (car (rassoc nil thread--record)))
           thread-name)

      (if thread-num
          (setq thread-name (concat thread--proc
                                    (format "%04d" thread-num)
                                    (and name (concat " - " name))))
        (throw 'thread-exceed-limit nil))

      (unless (process-live-p (get-process thread-name))
        
        ;; Creating the thread
        (start-process thread-name
                       nil
                       (file-truename
                        (expand-file-name invocation-name
                                          invocation-directory))
                       "-Q" "-batch"
                       "-l" (locate-library "fifo")
                       "-l" (locate-library "sign")
                       "-l" (locate-library "thread-packet")
                       "-l" (locate-library "thread-socket")
                       "-l" (locate-library "thread-server")
                       "-f" "threadS-init")
        
        (when (process-live-p (get-process thread-name))        
          ;; Send the thread name and local port by stdout
          (process-send-string thread-name (concat (prin1-to-string (cons thread-num thread--local-port)) "\n"))
          
          (let ((thread (make-instance 'thread
                                       :id thread-num
                                       :process (get-process thread-name)
                                       :persist persist)))
            
            ;; Register the thread to the thread--record
            (setf (cdr (assoc thread-num thread--record)) thread)
            thread))))))





(defun thread--listener-receive-data (proc data)
  
  ;; Data will arrived as string.
  ;; Large data will be split into small data chunks at parent process.
  ;; A newline charater "\n" indicates the end of the chunks.
  ;; One chunk is sent at a time.
  ;; Depends on OS, the max. data size for a data chunk is fixed, say 4kb for my PC.
  ;; So data chunk is put in thread--listener-buffer first.
  ;; And combine to form a complete data when the newline character is met.
  "Private function. Using it may cause serious problem."
  (if (string-match "\n" data (- (length data) 1))
      (progn
        (nconc thread--listener-buffer (list data))
        (thread.socket.inbound.push thread--listener-buffer)
        (setq thread--listener-buffer (list 0))) ;; Need to use (list 0) instead of '(0)
    (nconc thread--listener-buffer (list data))))





(defun thread--process-inbound-data ()

  ;; Process a complete inbound data from thread.socket.
  ;; Distribute the job to responsible functions.
  "Private function. Using it may cause serious problem."
    
  (let* ((job (thread.socket.inbound.pop))
         (string (mapconcat 'identity (cdr job) ""))
         packet
         thread
         packet-type)
    
    ;; Cut the last newline char
    (setq string (substring string 0 (- (length string) 1)))
    (setq packet (read string))
    
    (when (thread.packet-p packet)
      (setq thread (cdr
                    (assq (thread.packet.getSource packet) thread--record)))

      ;; Will disappear after compile
      (thread--debug-print-packet packet :inbound t)
      
      (setq packet-type (thread.packet.getType packet))
      (when (and thread (process-live-p (thread.getProcess thread)))
        ;; Distribute jobs
        (cond
         ((eq packet-type 'port)
          (thread--port-packet-handler thread packet))
         ((eq packet-type 'err)
          (thread--err-packet-handler thread packet))
         ((eq packet-type 'msg)
          (thread--msg-packet-handler thread packet))
         ((eq packet-type 'quit)
          (thread--quit-packet-handler thread))
         ((eq packet-type 'rpy)
          (thread--rpy-packet-handler thread packet))
         ((eq packet-type 'tgi)
          (thread--tgi-packet-handler thread packet)))))))





(defun thread--port-packet-handler (thread packet)
  
  ;; Handling replied packet which is of port type.
  ;; Set port number of thread.
  "Private function. Using it may cause serious problem."
  (let ((port (thread.packet.getData packet)))
    (thread.setPort thread port)
    (thread--do-next-after-process-job thread t)))


(defun thread--err-packet-handler (thread packet)

  ;; Handling replied packet which is of err type.
  ;; Calling appropiate function to handle error.
  "Private function. Using it may cause serious problem."
  
  (let ((error-handler (thread.packet.getErrorHandler packet))
        (arg (thread.packet.getData packet)))
    
    (when error-handler
     (ignore-errors (apply error-handler (list arg)))))
  (thread--do-next-after-process-job thread))


(defun thread--msg-packet-handler (thread packet)
  
  ;; Handling replied packet which is of msg type.
  ;; Output message to *thread log* if it is in debug mode.
  ;; Output message directly if it is not in debug mode.
  "Private function. Using it may cause serious problem."
  
  (let ((data (thread.packet.getData packet)))
    (if thread-debug-p
        (thread-debug-print (format "thread%d~ %s\n" (thread.getid thread) data))
      (message data))))


(defun thread--quit-packet-handler (thread)

  ;; Handling replied packet which is of quit type.
  ;; Kill the process associated with THREAD.
  ;; It can also use to force quit a thread.
  "Private function. Using it may cause serious problem."
  (ignore-errors (delete-process (thread.getSender thread)))
  (ignore-errors (delete-process (thread.getProcess thread)))
  (setf (cdr (assoc (thread.getid thread) thread--record)) nil))


(defun thread--rpy-packet-handler (thread packet)

  ;; Handling replied packet which is of msg type.
  ;; Calling appropiate function to handle the reply.
  "Private function. Using it may cause serious problem."

  (let ((reply-func (thread.packet.getReply packet))
        (arg (thread.packet.getData packet)))
    (when reply-func
      (ignore-errors (apply reply-func arg))))
  (thread--do-next-after-process-job thread))



(defun thread--tgi-packet-handler (thread packet)

  ;; Handle instruction generated by child thread.
  ;; Calling appropiate function to handle the instruction.

  (let ((instruction (thread.packet.getReply packet))
        (arg (thread.packet.getData packet)))
    (ignore-errors (apply instruction arg)))
  ;; Instruction generated by child thread does not count as reply
  ;; It should not change either the quene state or ready state of the thread.
  )




(defun thread--do-next-after-process-job (thread &optional notQuit)
  
  ;; Perform the next action after getting a reply from the child thread
  ;; and the reply has been processed.
  "Private function. Using it may cause serious problem."

  (thread.clrCurrentJob thread)
  (if (thread.job.hasNext thread)
      (if (thread.socket.isInBuffer thread)
          (progn
            (thread.socket.buffer.remove thread)
            (thread.pushToOutbound thread))
        (thread.pushToOutbound thread))
    (unless (or (thread.isPersist thread) notQuit)
      (thread.quit thread))))
  
  






 
(defclass thread (fifo)
  ((id
    :initarg :id
    :type integer
    :accessor thread.id
    :protection :private)
   (process
    :initarg :process
    :type process
    :accessor thread.process
    :protection :private)
   (sender
    :type process
    :accessor thread.sender
    :protection :private)
   (port
    :initform nil
    :initarg :port
    ;; port type is checked by setter
    :accessor thread.port
    :protection :private)
   (persist
    :initform nil
    :initarg :persist
    :type boolean
    :accessor thread.persist
    :protection :private)
   (job
    :initform nil
    :accessor thread.job
    :protection :private)
   (current-job
    :initform nil
    :accessor thread.currentJob
    :protection :private)
   (quene
    :initform nil
    :type boolean
    :accessor thread.quene
    :protection :private)
   (load-path
    :initform nil
    :type boolean
    :accessor thread.loadPath
    :protection :private))

   "Thread class. `thread.get' is the only vaild
way to create a thread instance.")


(defmethod initialize-instance :before ((obj thread) &rest args)
  ;; Constructor. Make sure name and process get initialized.
  "Private function. Using it may cause serious problem."
  (unless (plist-get (car args) ':id)
    (error "Slot :name must be initialized."))
  (unless (plist-get (car args) ':process)
    (error "Slot :process must be initialized.")))


(defmethod thread.getid ((obj thread))
  "Private function.Private function. Using it may cause serious problem."
  ;; Get the thread id (assq (car data) thread--record)
  (thread.id obj))

(defmethod thread.getProcess ((obj thread))
  "Private function. Using it may cause serious problem."
  ;; Get the thread process
  (thread.process obj))

(defmethod thread.getSender ((obj thread))
  "Private function. Using it may cause serious problem."
  ;; Get the thread sender
  (thread.sender obj))

(defmethod thread.setSender ((obj thread) sender)
  "Private function. Using it may cause serious problem."
  ;; Set the thread sender
  (setf (thread.sender obj) sender))

(defmethod thread.setPort ((obj thread) port)
  "Private function. Using it may cause serious problem."
  ;; Set the thread port
  (unless (and (integerp port) (> port 0) (<= port 65535))
    (error "Invalid port"))
   (setf (thread.port obj) port))

(defmethod thread.getPort ((obj thread))
  "Private function. Using it may cause serious problem."
  ;; Get the thread port
  (thread.port obj))

(defmethod thread.isPersist ((obj thread))
  "Private function. Using it may cause serious problem."
  ;; Return whether the thread should be persist
  (thread.persist obj))

(defmethod thread.getJob ((obj thread))
  ;; Get job list from thread
  "Private function. Using it may cause serious problem."
  (thread.job obj))

(defmethod thread.job.hasNext ((obj thread))
  ;; Whether there is job
  "Private function. Using it may cause serious problem."
  (when (thread.getJob obj) t))

(defmethod thread.pushJob ((obj thread) packet)
  ;; Push job to thread's job quene
  "Private function. Using it may cause serious problem."
  (fifo-push obj 'job packet))

(defmethod thread.popJob ((obj thread))
  ;; Pop job from thread's job quene and return the job's thread.packet.
  "Private function. Using it may cause serious problem."
  (fifo-pop obj 'job))

(defmethod thread.getCurrentJob ((obj thread))
  ;; Get the current job from the thread
  "Private function. Using it may cause serious problem."
  (thread.currentJob obj))

(defmethod thread.setCurrentJob ((obj thread) packet)
  ;; Set the current job to thread
  "Private function. Using it may cause serious problem."
  (setf (thread.currentJob obj) packet))

(defmethod thread.clrCurrentJob ((obj thread))
  ;; Remove the current job
  "Private function. Using it may cause serious problem."
  (setf (thread.currentJob obj) nil))

(defmethod thread.isReady ((obj thread))
  "Private function. Using it may cause serious problem."
  ;; Whether the thread is ready to send next job.
  ;; No current job means ready
  (unless (thread.currentJob obj) t))

(defmethod thread.isQuened ((obj thread))
  ;; Whether thread is quened in thread.socket
  "Private function. Using it may cause serious problem."
  (thread.quene obj))

(defmethod thread.setQuene ((obj thread) quene)
  ;; quene is either t or nil
  "Private function. Using it may cause serious problem."
  (setf (thread.quene obj) quene))

(defmethod thread.isPathReady ((obj thread))
  ;; Return whether load path is set
  "Private function. Using it may cause serious problem."
  (thread.loadPath obj))

(defmethod thread.flagPathReady ((obj thread))
  ;; Return whether load path is set
  "Private function. Using it may cause serious problem."
  (setf (thread.loadPath obj) t))

(defmacro thread.pushToOutboundBuffer (thread)
  ;; Push the thread to outbound buffer.
  ;; These function invokes iff thread fails to send out job for one time.
  `(progn
     (thread.setQuene ,thread t)
     (thread.socket.buffer.add ,thread)))

(defmacro thread.pushToOutbound (thread)
  ;; Push thread to outbound of the thread.socket
  ;; Make the setQuene and thread.socket.outbound.push atomic
  "Private function. Using it may cause serious problem."
  `(progn
     (thread.setQuene ,thread t)
     (thread.socket.outbound.push ,thread)))

(defmacro thread.popFromOutbound ()
  ;; Pop from outbound of thread.socket and return the thread.
  ;; Make the setQuene and thread.socket.outbound.pop atomic
  "Private function. Using it may cause serious problem."
  `(let ((thread (thread.socket.outbound.pop)))
     (thread.setQuene thread nil)
     thread))





(defmethod thread.quit ((obj thread))
  
  "Send a quit signal to child Thread to perform safe quit action."

  (when (process-live-p (thread.getProcess obj))
    (thread.pushJob obj (make-instance 'thread.packet
                                       :source (thread.getid obj)
                                       :type 'quit
                                       :data t))
    (unless (thread.isQuened obj)
      (thread.pushToOutbound obj))))

(defmethod thread.forceQuit ((obj thread))

  "Forced quit a THREAD without letting the thread to stop its job kindly."

  (thread--quit-packet-handler obj))






(defmethod thread--send-exec ((obj thread) func unique reply-func error-handler quit-warn &rest arg)

  "Not supposed to be called directly. Use `thread.send.exec' instead."
  
  (when (process-live-p (thread.getProcess obj))
    (let ((jobs (thread.getJob obj))
          (packet (make-instance 'thread.packet
                                       :source (thread.getid obj)
                                       :type 'exe
                                       :data (cons func arg)
                                       :reply reply-func
                                       :error-handler error-handler
                                       :quit-warn quit-warn)))
      (unless (and unique (or (member packet jobs) (equal (thread.getCurrentJob obj) packet)))
        (thread.pushJob obj packet)
        
        (unless (thread.isQuened obj)
          (thread.pushToOutbound obj))))))




(cl-defmacro thread.send.exec (thread func &rest arg &key unique reply-func error-handler quit-warn &allow-other-keys)

  ;; Just a wrapper to thread--send-exec
  ;; so that arguments can be supplied in a more elegant way.
  "Send single instruction to child thread.

THREAD is a thread object which can be aquired by `thread.get'.
FUNC is the symbol of function which you want to execute in child thread.
ARGS are the arguments supplied to FUNC.

UNIQUE is to stated whether the job is unique.
Non-nil value states that the job should be unique.
For example, you send a update database job to child thread.
The update database job takes some time, says 3 minutes.
If you send subsequent

REPLY-FUNC is the function to be called when excution of FUNC returned a
result. The result is pass as the argument to the REPLY-FUNC.
You can ignore it if you don't need a reply. 

ERROR-HANDLER is the function to be called when error is encountered
in the child thread during excuting of instruction.
The ERROR-HANDER function will be called with the error message as argument.
You can ignore it if you don't handle the error.

The instruction will be executed by `apply' in the child thread."
  
  (let (rest)
    (dolist (elt arg)
      (if (memq elt '(:unique :reply-func :error-handler :quit-warn))
          (setq key elt)
        (if key
            (progn
              (cond
               ((eq key ':unique)
                (setq unique elt))
               ((eq key ':reply-func)
                (setq reply-func elt))
               ((eq key ':error-handler)
                (setq error-handler elt))
               ((eq key ':quit-warn)
                (setq quit-warn elt)))
              (setq key nil))
          (push elt rest))))
    (setq rest (nreverse rest))

    `(thread--send-exec ,thread ,func ,unique ,reply-func ,error-handler ,quit-warn ,@rest)))




(defmethod thread--send-code ((obj thread) code unique reply-func error-handler quit-warn)

  "Not supposed to be called directly. Use `thread.send.code' instead."

  (when (process-live-p (thread.getProcess obj))
    (let ((jobs (thread.getJob obj))
          (packet (make-instance 'thread.packet
                                       :source (thread.getid obj)
                                       :type 'code
                                       :data code
                                       :reply reply-func
                                       :error-handler error-handler)))
      (unless (and unique (or (member packet jobs) (equal (thread.getCurrentJob obj) packet)))
        (thread.pushJob obj packet)
        
        (unless (thread.isQuened obj)
          (thread.pushToOutbound obj))))))


(cl-defmacro thread.send.code (thread &key code unique reply-func error-handler quit-warn)

  ;; Just a wrapper to thread--send-code
  ;; so that arguments can be supplied in a more elegant way.
  "Evaluate CODE in child thread.

THREAD is a thread object which can be aquired by `thread.get'.
CODE is the code being evaluated at child thread.

REPLY-FUNC is the function to be called when excution of FUNC returned a
result. The result is pass as the argument to the REPLY-FUNC.
You can ignore it if you don't need a reply. 

ERROR-HANDLER is the function to be called when error is encountered
in the child thread during excuting of instruction.
The ERROR-HANDER function will be called with the error message as argument.
You can ignore it if you don't handle the error."

  `(thread--send-code ,thread ,code ,unique ,reply-func ,error-handler ,quit-warn))


(defmethod thread.requirePackage ((obj thread) &rest packets)

  (unless (thread.isPathReady obj)
    (thread.send.exec obj 'threadS-set-load-path 
                      :error-handler 'thread-debug-print
                      load-path)
    (thread.flagPathReady obj))

  (thread.send.exec obj 'threadS-require-packet packets
                    :error-handler 'thread-debug-print))



(defun thread-debug-print (object)

  "Print string to thread log."

  (unless (stringp object)
    (setq object (prin1-to-string object)))
  
  (with-current-buffer (get-buffer-create thread-debug-buffer-name)
          (setq buffer-read-only nil)
          (goto-char (point-max))
          (insert (format-time-string "%Y%m%d - %I:%M:%S%p $ ")
                  (format "%s\n" object))
          (when (eq (get-buffer-window (current-buffer)) (get-buffer-window thread-debug-buffer-name))
            (recenter -3))
          (setq buffer-read-only t)))



(defun thread--process-outbound-data ()

  ;; Process the first thread in the outbound of thread.socket

  "Private function. Using it may cause serious problem."

  (let ((thread (thread.popFromOutbound)))
    ;; If ready do job, if not ready requene in outbound of thread.socket
    
    (if (thread.isReady thread)
        ;; Check if process alive, if not, release if from thread record
        (if (process-live-p (thread.getProcess thread))
              (let ((sender (thread.getSender thread))
                    packet)
                  
                ;; Make a network stream
                (unless (and sender (process-live-p sender))
                  (setq sender 
                        (open-network-stream (concat "thread" (number-to-string (thread.getid thread)) " - sender")
                                             nil
                                             "localhost"
                                             (thread.getPort thread)
                                             'plain)))
                
                (when sender
                  ;; Send job
                  (setq packet (thread.popJob thread))
                  (process-send-string sender
                                       (concat (prin1-to-string packet) "\n"))
                  (thread.setCurrentJob thread packet)

                  ;; Will disappear after compile
                  (thread--debug-print-packet packet :inbound nil)
                  
                  (if (thread.job.hasNext thread)
                      (progn
                        ;; Still has job, keep the sender, quene the thread again
                        (thread.setSender thread sender)
                        (thread.pushToOutbound thread))
                    ;; No job, close the network stream
                    (ignore-errors (delete-process sender))
                    (thread.setSender thread nil))))
          
          ;; Process not alive, delete form thread record
          (thread.forceQuit thread))

      ;; Thread is not ready, put to buffer
      (thread.pushToOutboundBuffer thread))))


;; (defmethod thread.send.setq ((obj thread) var value)
;;   (thread.send.exec obj 'threadS-setq var value))




(defun thread--kill-emacs (orig &optional arg count)

  ;; This function is supposed to be adviced
  ;; :around kill-emacs and save-buffers-kill-emacs.
  ;; When ~kill-emacs~ is invoked, attempt to close all threads safely
  ;; with certain period of time.
  ;; By default, it is 5 seconds defined customly in
  ;; thread-kill-emacs-close-thread-delay.
  "Private function. Using it may cause serious problem."

  (let (threads)
    (dolist (record thread--record)
      (when (cdr record)
        (push (cdr record) threads)))

    (if (and threads
             (or (null count)
                 (< (* count 0.3) thread-kill-emacs-close-thread-delay)))
        (progn
          (message "%d thread%s is running. Try to quit %s safely.
Emacs will be quit within %d seconds."
               (length threads)
               (if (> (length threads) 1) "s" "")
               (if (> (length threads) 1) "them" "it")
               (or (and count
                        (ceiling
                         (- thread-kill-emacs-close-thread-delay
                           (* count 0.3))))
                    thread-kill-emacs-close-thread-delay))

          (unless count
            ;; Try to safe quit all threads
            (dolist (thread threads)
              (thread.quit thread))
            
            ;; Don't block the ui when closing threads
            ;; Update the close thread progress in
            ;; thread--kill-emacs-reporter
            (sign-connect :sign 'thread--kill-emacs-signal
                          :worker 'thread--kill-emacs))
          ;; Call itself recusively 
          (emit 'thread--kill-emacs-signal
                :delay 0.3
                :arg (list orig arg (or (and (null count) 1) (1+ count)))))

      ;; If no threads, close happily.
      ;; If still has threads, froce close.
      (sign-disconnect 'thread--kill-emacs-signal 'thread--kill-emacs)

      (let (stop-quit)
        ;; Force quit any threads without warning
        ;; Ask for user confirm if there is warning
        (dolist (thread threads)
          (message (prin1-to-string (thread.getCurrentJob thread)))
          (if (and (thread.getCurrentJob thread) (thread.packet.getQuitWarn (thread.getCurrentJob thread)))
                (if (yes-or-no-p (thread.packet.getQuitWarn (thread.getCurrentJob thread)))
                  (thread.forceQuit thread)
                (setq stop-quit t))
            (thread.forceQuit thread)))
        
        (if stop-quit
            (message "User cancelled kill-emacs action.")
          (ignore-errors (delete-process thread--local-listener))
          ;; Close the network stream created by child threads.
          (dolist (process (process-list))
            (when (string-prefix-p "thread" (process-name process))
              (ignore-errors (delete-process process))))
          (apply orig (list arg)))))))


(advice-add 'kill-emacs :around 'thread--kill-emacs)
(advice-add 'save-buffers-kill-emacs :around 'thread--kill-emacs)


(make-directory thread--child-thread-quene-folder t)
(dolist (file (directory-files thread--child-thread-quene-folder))
  (unless (or (equal file "..") (equal file "."))
    (delete-file (concat thread--child-thread-quene-folder file))))


(provide 'thread)
;;; thread.el ends here

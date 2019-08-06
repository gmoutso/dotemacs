;;; ena-kernel.el --- Communicate with IPython notebook server

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-kernel.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-kernel.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-kernel.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ansi-color)

(require 'ena-core)
(require 'ena-log)
;; FIXME: use websocket.el directly once v1.0 is released.
(require 'ena-websocket)
(require 'ena-events)
(require 'ena-query)


;; FIXME: Rewrite `ena:$kernel' using `defclass'.  It should ease
;;        testing since I can mock I/O using method overriding.
(defstruct ena:$kernel
  "Hold kernel variables.

`ena:$kernel-url-or-port'
  URL or port of IPython server.
"
  url-or-port
  events
  kernel-id
  shell-channel
  iopub-channel
  base-url                              ; /kernels/
  kernel-url                            ; /kernels/<KERNEL-ID>
  ws-url                                ; ws://<URL>[:<PORT>]
  running
  username
  session-id
  msg-callbacks
  ;; FIXME: Use event instead of hook.
  after-start-hook
  after-execute-hook)

;; "Public" getters.  Use them outside of this package.

;;;###autoload
(defalias 'ena:kernel-url-or-port 'ena:$kernel-url-or-port)

;;;###autoload
(defalias 'ena:kernel-id 'ena:$kernel-kernel-id)


;;; Initialization and connection.

(defun ena:kernel-new (url-or-port base-url events)
  (make-ena:$kernel
   :url-or-port url-or-port
   :events events
   :kernel-id nil
   :shell-channel nil
   :iopub-channel nil
   :base-url base-url
   :running nil
   :username "username"
   :session-id (ena:utils-uuid)
   :msg-callbacks (make-hash-table :test 'equal)))


(defun ena:kernel-del (kernel)
  "Destructor for `ena:$kernel'."
  (ena:kernel-stop-channels kernel))


(defun ena:kernel--get-msg (kernel msg-type content)
  (list
   :header (list
            :msg_id (ena:utils-uuid)
            :username (ena:$kernel-username kernel)
            :session (ena:$kernel-session-id kernel)
            :msg_type msg-type)
   :metadata (make-hash-table)
   :content content
   :parent_header (make-hash-table)))


(defun ena:kernel-start (kernel notebook-id)
  "Start kernel of the notebook whose id is NOTEBOOK-ID."
  (unless (ena:$kernel-running kernel)
    (ena:query-singleton-ajax
     (list 'kernel-start (ena:$kernel-kernel-id kernel))
     (concat (ena:url (ena:$kernel-url-or-port kernel)
                      (ena:$kernel-base-url kernel))
             "?" (format "notebook=%s" notebook-id))
     :type "POST"
     :parser #'ena:json-read
     :success (apply-partially #'ena:kernel--kernel-started kernel))))


(defun ena:kernel-restart (kernel)
  (ena:events-trigger (ena:$kernel-events kernel)
                      'status_restarting.Kernel)
  (ena:log 'info "Restarting kernel")
  (when (ena:$kernel-running kernel)
    (ena:kernel-stop-channels kernel)
    (ena:query-singleton-ajax
     (list 'kernel-restart (ena:$kernel-kernel-id kernel))
     (ena:url (ena:$kernel-url-or-port kernel)
              (ena:$kernel-kernel-url kernel)
              "restart")
     :type "POST"
     :parser #'ena:json-read
     :success (apply-partially #'ena:kernel--kernel-started kernel))))


(defun* ena:kernel--kernel-started (kernel &key data &allow-other-keys)
  (destructuring-bind (&key kernel_id ws_url &allow-other-keys) data
    (unless (and kernel_id ws_url)
      (error "Failed to start kernel.  No `kernel_id' or `ws_url'.  Got %S."
             data))
    (ena:log 'info "Kernel started: %s" kernel_id)
    (setf (ena:$kernel-running kernel) t)
    (setf (ena:$kernel-kernel-id kernel) kernel_id)
    (setf (ena:$kernel-ws-url kernel) (ena:kernel--ws-url kernel ws_url))
    (setf (ena:$kernel-kernel-url kernel)
          (concat (ena:$kernel-base-url kernel) "/" kernel_id)))
  (ena:kernel-start-channels kernel)
  (let ((shell-channel (ena:$kernel-shell-channel kernel))
        (iopub-channel (ena:$kernel-iopub-channel kernel)))
    ;; FIXME: get rid of lexical-let
    (lexical-let ((kernel kernel))
      (setf (ena:$websocket-onmessage shell-channel)
            (lambda (packet)
              (ena:kernel--handle-shell-reply kernel packet)))
      (setf (ena:$websocket-onmessage iopub-channel)
            (lambda (packet)
              (ena:kernel--handle-iopub-reply kernel packet))))))


(defun ena:kernel--ws-url (kernel ws_url)
  "Use `ena:$kernel-url-or-port' if WS_URL is an empty string.
See: https://github.com/ipython/ipython/pull/3307"
  (if (string-match-p "^wss?://" ws_url)
      ws_url
    (let ((ena:url-localhost-template "ws://127.0.0.1:%s"))
      (ena:url (ena:$kernel-url-or-port kernel)))))


(defun ena:kernel--websocket-closed (kernel ws-url early)
  (if early
      (ena:display-warning
       "Websocket connection to %s could not be established.
  You will NOT be able to run code.  Your websocket.el may not be
  compatible with the websocket version in the server, or if the
  url does not look right, there could be an error in the
  server's configuration." ws-url)
    (ena:display-warning "Websocket connection closed unexpectedly.
  The kernel will no longer be responsive.")))


(defun ena:kernel-send-cookie (channel host)
  ;; cookie can be an empty string for IPython server with no password,
  ;; but something must be sent to start channel.
  (let ((cookie (ena:query-get-cookie host "/")))
    (ena:websocket-send channel cookie)))


(defun ena:kernel--ws-closed-callback (websocket kernel arg)
  ;; NOTE: The argument ARG should not be "unpacked" using `&rest'.
  ;; It must be given as a list to hold state `already-called-onclose'
  ;; so it can be modified in this function.
  (destructuring-bind (&key already-called-onclose ws-url early)
      arg
    (unless already-called-onclose
      (plist-put arg :already-called-onclose t)
      (unless (ena:$websocket-closed-by-client websocket)
        ;; Use "event-was-clean" when it is implemented in websocket.el.
        (ena:kernel--websocket-closed kernel ws-url early)))))


(defun ena:kernel-start-channels (kernel)
    (ena:kernel-stop-channels kernel)
    (let* ((ws-url (concat (ena:$kernel-ws-url kernel)
                           (ena:$kernel-kernel-url kernel)))
           (onclose-arg (list :ws-url ws-url
                              :already-called-onclose nil
                              :early t)))
      (ena:log 'info "Starting WS: %S" ws-url)
      (setf (ena:$kernel-shell-channel kernel)
            (ena:websocket (concat ws-url "/shell")))
      (setf (ena:$kernel-iopub-channel kernel)
            (ena:websocket (concat ws-url "/iopub")))

      (loop for c in (list (ena:$kernel-shell-channel kernel)
                           (ena:$kernel-iopub-channel kernel))
            do (setf (ena:$websocket-onclose-args c) (list kernel onclose-arg))
            do (setf (ena:$websocket-onopen c)
                     (lexical-let ((channel c)
                                   (kernel kernel)
                                   (host (let (url-or-port
                                               (ena:$kernel-url-or-port kernel))
                                           (if (stringp url-or-port)
                                               url-or-port
                                             ena:url-localhost))))
                       (lambda ()
                         (ena:kernel-send-cookie channel host)
                         ;; run `ena:$kernel-after-start-hook' if both
                         ;; channels are ready.
                         (when (ena:kernel-live-p kernel)
                           (ena:kernel-run-after-start-hook kernel)))))
            do (setf (ena:$websocket-onclose c)
                     #'ena:kernel--ws-closed-callback))

      ;; switch from early-close to late-close message after 1s
      (run-at-time
       1 nil
       (lambda (onclose-arg)
         (plist-put onclose-arg :early nil)
         (ena:log 'debug "(via run-at-time) onclose-arg changed to: %S"
                  onclose-arg))
       onclose-arg)))

;; NOTE: `onclose-arg' can be accessed as:
;; (nth 1 (ena:$websocket-onclose-args (ena:$kernel-shell-channel (ena:$notebook-kernel ena:notebook))))


(defun ena:kernel-run-after-start-hook (kernel)
  (ena:log 'debug "EIN:KERNEL-RUN-AFTER-START-HOOK")
  (mapc #'ena:funcall-packed
        (ena:$kernel-after-start-hook kernel)))


(defun ena:kernel-stop-channels (kernel)
  (when (ena:$kernel-shell-channel kernel)
    (setf (ena:$websocket-onclose (ena:$kernel-shell-channel kernel)) nil)
    (ena:websocket-close (ena:$kernel-shell-channel kernel))
    (setf (ena:$kernel-shell-channel kernel) nil))
  (when (ena:$kernel-iopub-channel kernel)
    (setf (ena:$websocket-onclose (ena:$kernel-iopub-channel kernel)) nil)
    (ena:websocket-close (ena:$kernel-iopub-channel kernel))
    (setf (ena:$kernel-iopub-channel kernel) nil)))


(defun ena:kernel-live-p (kernel)
  (and
   (ena:$kernel-p kernel)
   (ena:aand (ena:$kernel-shell-channel kernel) (ena:websocket-open-p it))
   (ena:aand (ena:$kernel-iopub-channel kernel) (ena:websocket-open-p it))))


(defmacro ena:kernel-if-ready (kernel &rest body)
  "Execute BODY if KERNEL is ready.  Warn user otherwise."
  (declare (indent 1))
  `(if (ena:kernel-live-p ,kernel)
       (progn ,@body)
     (ena:log 'warn "Kernel is not ready yet! (or closed already.)")))


;;; Main public methods

;; NOTE: The argument CALLBACKS for the following functions is almost
;;       same as the JS implementation in IPython.  However, as Emacs
;;       lisp does not support closure, value is "packed" using
;;       `cons': `car' is the actual callback function and `cdr' is
;;       its first argument.  It's like using `cons' instead of
;;       `$.proxy'.

(defun ena:kernel-object-info-request (kernel objname callbacks)
  "Send object info request of OBJNAME to KERNEL.

When calling this method pass a CALLBACKS structure of the form:

    (:object_info_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `object_into_reply' message.

`object_into_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#object-information
"
  (assert (ena:kernel-live-p kernel) nil "Kernel is not active.")
  (when objname
    (let* ((content (list :oname (format "%s" objname)))
           (msg (ena:kernel--get-msg kernel "object_info_request" content))
           (msg-id (plist-get (plist-get msg :header) :msg_id)))
      (ena:websocket-send
       (ena:$kernel-shell-channel kernel)
       (json-encode msg))
      (ena:kernel-set-callbacks-for-msg kernel msg-id callbacks)
      msg-id)))


(defun* ena:kernel-execute (kernel code &optional callbacks
                                   &key
                                   (silent t)
                                   (user-variables [])
                                   (user-expressions (make-hash-table))
                                   (allow-stdin json-false))
  "Execute CODE on KERNEL.

When calling this method pass a CALLBACKS structure of the form:

  (:execute_reply  EXECUTE-REPLY-CALLBACK
   :output         OUTPUT-CALLBACK
   :clear_output   CLEAR-OUTPUT-CALLBACK
   :set_next_input SET-NEXT-INPUT)

Objects end with -CALLBACK above must pack a FUNCTION and its
first ARGUMENT in a `cons'::

  (FUNCTION . ARGUMENT)

Call signature
--------------
::

  (`funcall' EXECUTE-REPLY-CALLBACK ARGUMENT          CONTENT METADATA)
  (`funcall' OUTPUT-CALLBACK        ARGUMENT MSG-TYPE CONTENT METADATA)
  (`funcall' CLEAR-OUTPUT-CALLBACK  ARGUMENT          CONTENT METADATA)
  (`funcall' SET-NEXT-INPUT         ARGUMENT TEXT)

* Both CONTENT and METADATA objects are plist.
* The MSG-TYPE argument for OUTPUT-CALLBACK is a string
  (one of `stream', `display_data', `pyout' and `pyerr').
* The CONTENT object for CLEAR-OUTPUT-CALLBACK has
  `stdout', `stderr' and `other' fields that are booleans.
* The SET-NEXT-INPUT callback will be passed the `set_next_input' payload,
  which is a string.
  See `ena:kernel--handle-shell-reply' for how the callbacks are called.

Links
-----
* For general description of CONTENT and METADATA:
  http://ipython.org/ipython-doc/dev/development/messaging.html#general-message-format
* `execute_reply' message is documented here:
  http://ipython.org/ipython-doc/dev/development/messaging.html#execute
* Output type messages is documented here:
  http://ipython.org/ipython-doc/dev/development/messaging.html#messages-on-the-pub-sub-socket

Sample implementations
----------------------
* `ena:cell--handle-execute-reply'
* `ena:cell--handle-output'
* `ena:cell--handle-clear-output'
* `ena:cell--handle-set-next-input'
"
  ;; FIXME: Consider changing callback to use `&key'.
  ;;        Otherwise, adding new arguments to callback requires
  ;;        backward incompatible changes (hence a big diff), unlike
  ;;        Javascript.  Downside of this is that there is no short way
  ;;        to write anonymous callback because there is no `lambda*'.
  ;;        You can use `function*', but that's bit long...

  ;; FIXME: Consider allowing a list of fixed argument so that the
  ;;        call signature becomes something like:
  ;;           (funcall FUNCTION [ARG ...] CONTENT METADATA)

  (assert (ena:kernel-live-p kernel) nil "Kernel is not active.")
  (let* ((content (list
                   :code code
                   :silent (or silent json-false)
                   :user_variables user-variables
                   :user_expressions user-expressions
                   :allow_stdin allow-stdin))
         (msg (ena:kernel--get-msg kernel "execute_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ena:websocket-send
     (ena:$kernel-shell-channel kernel)
     (json-encode msg))
    (ena:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    (unless silent
      (mapc #'ena:funcall-packed
            (ena:$kernel-after-execute-hook kernel)))
    msg-id))


(defun ena:kernel-complete (kernel line cursor-pos callbacks)
  "Complete code at CURSOR-POS in a string LINE on KERNEL.

CURSOR-POS is the position in the string LINE, not in the buffer.

When calling this method pass a CALLBACKS structure of the form:

    (:complete_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `complete_reply' message.

`complete_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#complete
"
  (assert (ena:kernel-live-p kernel) nil "Kernel is not active.")
  (let* ((content (list
                   :text ""
                   :line line
                   :cursor_pos cursor-pos))
         (msg (ena:kernel--get-msg kernel "complete_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ena:websocket-send
     (ena:$kernel-shell-channel kernel)
     (json-encode msg))
    (ena:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))


(defun* ena:kernel-history-request (kernel callbacks
                                           &key
                                           (output nil)
                                           (raw t)
                                           (hist-access-type "tail")
                                           session
                                           start
                                           stop
                                           (n 10)
                                           pattern
                                           unique)
  "Request execution history to KERNEL.

When calling this method pass a CALLBACKS structure of the form:

    (:history_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `history_reply' message.

`history_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#history

Relevant Python code:

* :py:method:`IPython.zmq.ipkernel.Kernel.history_request`
* :py:class:`IPython.core.history.HistoryAccessor`
"
  (assert (ena:kernel-live-p kernel) nil "Kernel is not active.")
  (let* ((content (list
                   :output (ena:json-any-to-bool output)
                   :raw (ena:json-any-to-bool raw)
                   :hist_access_type hist-access-type
                   :session session
                   :start start
                   :stop stop
                   :n n
                   :pattern pattern
                   :unique unique))
         (msg (ena:kernel--get-msg kernel "history_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ena:websocket-send
     (ena:$kernel-shell-channel kernel)
     (json-encode msg))
    (ena:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))


(defun ena:kernel-kernel-info-request (kernel callbacks)
  "Request core information of KERNEL.

When calling this method pass a CALLBACKS structure of the form::

  (:kernel_info_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `kernel_info_reply' message.

`kernel_info_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#kernel-info

Example::

  (ena:kernel-kernel-info-request
   (ena:get-kernel)
   '(:kernel_info_reply (message . \"CONTENT: %S\\nMETADATA: %S\")))
"
  (assert (ena:kernel-live-p kernel) nil "Kernel is not active.")
  (let* ((msg (ena:kernel--get-msg kernel "kernel_info_request" nil))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ena:websocket-send
     (ena:$kernel-shell-channel kernel)
     (json-encode msg))
    (ena:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))


(defun ena:kernel-interrupt (kernel)
  (when (ena:$kernel-running kernel)
    (ena:log 'info "Interrupting kernel")
    (ena:query-singleton-ajax
     (list 'kernel-interrupt (ena:$kernel-kernel-id kernel))
     (ena:url (ena:$kernel-url-or-port kernel)
              (ena:$kernel-kernel-url kernel)
              "interrupt")
     :type "POST"
     :success (lambda (&rest ignore)
                (ena:log 'info "Sent interruption command.")))))


(defun ena:kernel-kill (kernel &optional callback cbargs)
  (when (ena:$kernel-running kernel)
    (ena:query-singleton-ajax
     (list 'kernel-kill (ena:$kernel-kernel-id kernel))
     (ena:url (ena:$kernel-url-or-port kernel)
              (ena:$kernel-kernel-url kernel))
     :type "DELETE"
     :success (apply-partially
               (lambda (kernel callback cbargs &rest ignore)
                 (ena:log 'info "Notebook kernel is killed")
                 (setf (ena:$kernel-running kernel) nil)
                 (when callback (apply callback cbargs)))
               kernel callback cbargs))))


;; Reply handlers.

(defun ena:kernel-get-callbacks-for-msg (kernel msg-id)
  (gethash msg-id (ena:$kernel-msg-callbacks kernel)))

(defun ena:kernel-set-callbacks-for-msg (kernel msg-id callbacks)
  (puthash msg-id callbacks (ena:$kernel-msg-callbacks kernel)))

(defun ena:kernel--handle-shell-reply (kernel packet)
  (ena:log 'debug "KERNEL--HANDLE-SHELL-REPLY")
  (destructuring-bind
      (&key header content metadata parent_header &allow-other-keys)
      (ena:json-read-from-string packet)
    (let* ((msg-type (plist-get header :msg_type))
           (msg-id (plist-get parent_header :msg_id))
           (callbacks (ena:kernel-get-callbacks-for-msg kernel msg-id))
           (cb (plist-get callbacks (intern (format ":%s" msg-type)))))
      (ena:log 'debug "KERNEL--HANDLE-SHELL-REPLY: msg_type = %s" msg-type)
      (if cb
          (ena:funcall-packed cb content metadata)
        (ena:log 'debug "no callback for: msg_type=%s msg_id=%s"
                 msg-type msg-id))
      (ena:aif (plist-get content :payload)
          (ena:kernel--handle-payload kernel callbacks it))
      (let ((events (ena:$kernel-events kernel)))
        (ena:case-equal msg-type
          (("execute_reply")
           (ena:aif (plist-get content :execution_count)
               ;; It can be `nil' for silent execution
               (ena:events-trigger events 'execution_count.Kernel it)))))))
  (ena:log 'debug "KERNEL--HANDLE-SHELL-REPLY: finished"))

(defun ena:kernel--handle-payload (kernel callbacks payload)
  (loop with events = (ena:$kernel-events kernel)
        for p in payload
        for text = (plist-get p :text)
        for source = (plist-get p :source)
        if (member source '("IPython.kernel.zmq.page.page"
                            "IPython.zmq.page.page"))
        do (when (not (equal (ena:trim text) ""))
             (ena:events-trigger
              events 'open_with_text.Pager (list :text text)))
        else if
        (member
         source
         '("IPython.kernel.zmq.zmqshell.ZMQInteractiveShell.set_next_input"
           "IPython.zmq.zmqshell.ZMQInteractiveShell.set_next_input"))
        do (let ((cb (plist-get callbacks :set_next_input)))
             (when cb (ena:funcall-packed cb text)))))

(defun ena:kernel--handle-iopub-reply (kernel packet)
  (ena:log 'debug "KERNEL--HANDLE-IOPUB-REPLY")
  (destructuring-bind
      (&key content metadata parent_header header &allow-other-keys)
      (ena:json-read-from-string packet)
    (let* ((msg-type (plist-get header :msg_type))
           (callbacks (ena:kernel-get-callbacks-for-msg
                       kernel (plist-get parent_header :msg_id)))
           (events (ena:$kernel-events kernel)))
      (ena:log 'debug "KERNEL--HANDLE-IOPUB-REPLY: msg_type = %s" msg-type)
      (if (and (not (equal msg-type "status")) (null callbacks))
          (ena:log 'verbose "Got message not from this notebook.")
        (ena:case-equal msg-type
          (("stream" "display_data" "pyout" "pyerr")
           (ena:aif (plist-get callbacks :output)
               (ena:funcall-packed it msg-type content metadata)))
          (("status")
           (ena:case-equal (plist-get content :execution_state)
             (("busy")
              (ena:events-trigger events 'status_busy.Kernel))
             (("idle")
              (ena:events-trigger events 'status_idle.Kernel))
             (("dead")
              (ena:kernel-stop-channels kernel)
              (ena:events-trigger events 'status_dead.Kernel))))
          (("clear_output")
           (ena:aif (plist-get callbacks :clear_output)
               (ena:funcall-packed it content metadata)))))))
  (ena:log 'debug "KERNEL--HANDLE-IOPUB-REPLY: finished"))


;;; Utility functions

(defun ena:kernel-filename-to-python (kernel filename)
  "See: `ena:filename-to-python'."
  (ena:filename-to-python (ena:$kernel-url-or-port kernel) filename))

(defun ena:kernel-filename-from-python (kernel filename)
  "See: `ena:filename-from-python'."
  (ena:filename-from-python (ena:$kernel-url-or-port kernel) filename))

(defun ena:kernel-construct-defstring (content)
  "Construct call signature from CONTENT of ``:object_info_reply``.
Used in `ena:pytools-finish-tooltip', etc."
  (or (plist-get content :call_def)
      (plist-get content :init_definition)
      (plist-get content :definition)))

(defun ena:kernel-construct-help-string (content)
  "Construct help string from CONTENT of ``:object_info_reply``.
Used in `ena:pytools-finish-tooltip', etc."
  (ena:log 'debug "KERNEL-CONSTRUCT-HELP-STRING")
  (let* ((defstring (ena:aand
                     (ena:kernel-construct-defstring content)
                     (ansi-color-apply it)
                     (ena:string-fill-paragraph it)))
         (docstring (ena:aand
                     (or (plist-get content :call_docstring)
                         (plist-get content :init_docstring)
                         (plist-get content :docstring)
                         ;; "<empty docstring>"
                         )
                     (ansi-color-apply it)))
         (help (ena:aand
                (ena:filter 'identity (list defstring docstring))
                (ena:join-str "\n" it))))
    (ena:log 'debug "KERNEL-CONSTRUCT-HELP-STRING: help=%s" help)
    help))

(defun ena:kernel-request-stream (kernel code func &optional args)
  "Run lisp callback FUNC with the output stream returned by Python CODE.

The first argument to the lisp function FUNC is the stream output
as a string and the rest of the argument is the optional ARGS."
  (ena:kernel-execute
   kernel
   code
   (list :output (cons (lambda (packed msg-type content -metadata-not-used-)
                         (let ((func (car packed))
                               (args (cdr packed)))
                           (when (equal msg-type "stream")
                             (ena:aif (plist-get content :data)
                                 (apply func it args)))))
                       (cons func args)))))

(defun* ena:kernel-history-request-synchronously
    (kernel &rest args &key (timeout 0.5) (tick-time 0.05) &allow-other-keys)
  "Send the history request and wait TIMEOUT seconds.
Return a list (CONTENT METADATA).
This function checks the request reply every TICK-TIME seconds.
See `ena:kernel-history-request' for other usable options."
  ;; As `result' and `finished' are set in callback, make sure they
  ;; won't be trapped in other let-bindings.
  (lexical-let (result finished)
    (apply
     #'ena:kernel-history-request
     kernel
     (list :history_reply
           (cons (lambda (-ignore- content metadata)
                   (setq result (list content metadata))
                   (setq finished t))
                 nil))
     args)
    (loop repeat (floor (/ timeout tick-time))
          do (sit-for tick-time)
          when finished
          return t
          finally (error "Timeout"))
    result))

(defun ena:kernel-history-search-synchronously (kernel pattern &rest args)
  "Search execution history in KERNEL using PATTERN.
Return matched history as a list of strings.
See `ena:kernel-history-request-synchronously' and
`ena:kernel-history-request' for usable options."
  (let ((reply
         (apply #'ena:kernel-history-request-synchronously
                kernel
                :hist-access-type "search"
                :pattern pattern
                args)))
    (mapcar #'caddr (plist-get (car reply) :history))))

(provide 'ena-kernel)

;;; ena-kernel.el ends here

;;; ena-notebook.el --- Notebook module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-notebook.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-notebook.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-notebook.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * Coding rule about current buffer.
;; A lot of notebook and cell functions touches to current buffer and
;; it is not ideal to wrap all these functions by `with-current-buffer'.
;; Therefore, when the function takes `notebook' to the first argument
;; ("method" function), it is always assumed that the current buffer
;; is the notebook buffer.  **However**, functions called as callback
;; (via `url-retrieve', for example) must protect themselves by
;; calling from unknown buffer.

;;; Code:


(eval-when-compile (require 'cl))
(require 'ewoc)
(eval-when-compile (require 'auto-complete nil t))

(require 'ena-core)
(require 'ena-log)
(require 'ena-node)
(require 'ena-kernel)
(require 'ena-kernelinfo)
(require 'ena-cell)
(require 'ena-worksheet)
(require 'ena-scratchsheet)
(require 'ena-notification)
(require 'ena-completer)
(require 'ena-pager)
(require 'ena-events)
(require 'ena-notification)
(require 'ena-kill-ring)
(require 'ena-query)
(require 'ena-pytools)


;;; Configuration

(make-obsolete-variable 'ena:notebook-discard-output-on-save nil "0.2.0")

(defcustom ena:notebook-discard-output-on-save 'no
  "Configure if the output part of the cell should be saved or not.

.. warning:: This configuration is obsolete now.
   Use nbconvert (https://github.com/ipython/nbconvert) to
   strip output.

`no' : symbol
    Save output. This is the default.
`yes' : symbol
    Always discard output.
a function
    This function takes two arguments, notebook and cell.  Return
    `t' to discard output and return `nil' to save.  For example,
    if you don't want to save image output but other kind of
    output, use `ena:notebook-cell-has-image-output-p'.
"
  :type '(choice (const :tag "No" 'no)
                 (const :tag "Yes" 'yes)
                 )
  :group 'ena)

(defun ena:notebook-cell-has-image-output-p (-ignore- cell)
  (ena:cell-has-image-ouput-p cell))

(defun ena:notebook-discard-output-p (notebook cell)
  "Return non-`nil' if the output must be discarded, otherwise save."
  (case ena:notebook-discard-output-on-save
    (no nil)
    (yes t)
    (t (funcall ena:notebook-discard-output-on-save notebook cell))))

;; As opening/saving notebook treats possibly huge data, define these
;; timeouts separately:

(defcustom ena:notebook-querty-timeout-open (* 60 1000) ; 1 min
  "Query timeout for opening notebook.
If you cannot open large notebook because of timeout error, try
to increase this value.  Setting this value to `nil' means to use
global setting.  For global setting and more information, see
`ena:query-timeout'."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "Use global setting" nil))
  :group 'ena)

(defcustom ena:notebook-querty-timeout-save (* 60 1000) ; 1 min
  "Query timeout for saving notebook.
Similar to `ena:notebook-querty-timeout-open', but for saving
notebook.  For global setting and more information, see
`ena:query-timeout'."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "Use global setting" nil))
  :group 'ena)

(defcustom ena:helm-kernel-history-search-key nil
  "Bind `helm-ena-kernel-history' to this key in notebook mode.

Example::

    (setq ena:helm-kernel-history-search-key \"\\M-r\")

This key will be installed in the `ena:notebook-mode-map'."
  :type 'boolean
  :group 'ena)

(defcustom ena:anything-kernel-history-search-key nil
  "Bind `anything-ena-kernel-history' to this key in notebook mode.

Example::

    (setq ena:anything-kernel-history-search-key \"\\M-r\")

This key will be installed in the `ena:notebook-mode-map'."
  :type 'boolean
  :group 'ena)

(defcustom ena:notebook-set-buffer-file-name nil
  "[EXPERIMENTAL] Set `buffer-file-name' of notebook buffer."
  :type 'boolean
  :group 'ena)

(defvar ena:notebook-after-rename-hook nil
  "Hooks to run after notebook is renamed successfully.
Current buffer for these functions is set to the notebook buffer.")


;;; Class and variable

(defvar ena:base-kernel-url "/")
;; Currently there is no way to know this setting.  Maybe I should ask
;; IPython developers for an API to get this from notebook server.

(defvar ena:notebook-pager-buffer-name-template "*ena:pager %s/%s*")
(defvar ena:notebook-buffer-name-template "*ena: %s/%s*")

(defvar ena:notebook-save-retry-max 1
  "Maximum retries for notebook saving.")

(defstruct ena:$notebook
  "Hold notebook variables.

`ena:$notebook-url-or-port'
  URL or port of IPython server.

`ena:$notebook-notebook-id' : string
  uuid string

`ena:$notebook-kernel' : `ena:$kernel'
  `ena:$kernel' instance.

`ena:$notebook-kernelinfo' : `ena:kernelinfo'
  `ena:kernelinfo' instance.

`ena:$notebook-pager'
  Variable for `ena:pager-*' functions. See ena-pager.el.

`ena:$notebook-dirty' : boolean
  Set to `t' if notebook has unsaved changes.  Otherwise `nil'.

`ena:$notebook-metadata' : plist
  Notebook meta data (e.g., notebook name).

`ena:$notebook-name' : string
  Notebook name.

`ena:$notebook-nbformat' : integer
  Notebook file format version.

`ena:$notebook-nbformat-minor' : integer
  Notebook file format version.

`ena:$notebook-events' : `ena:$events'
  Event handler instance.

`ena:$notebook-worksheets' : list of `ena:worksheet'
  List of worksheets.

`ena:$notebook-scratchsheets' : list of `ena:worksheet'
  List of scratch worksheets.
"
  url-or-port
  notebook-id
  kernel
  kernelinfo
  pager
  dirty
  metadata
  notebook-name
  nbformat
  nbformat-minor
  events
  worksheets
  scratchsheets
  )

(ena:deflocal ena:%notebook% nil
  "Buffer local variable to store an instance of `ena:$notebook'.")
(define-obsolete-variable-alias 'ena:notebook 'ena:%notebook% "0.1.2")


;;; Constructor

(defun ena:notebook-new (url-or-port notebook-id &rest args)
  (let ((notebook (apply #'make-ena:$notebook
                         :url-or-port url-or-port
                         :notebook-id notebook-id
                         args)))
    notebook))


;;; Destructor

(defun ena:notebook-del (notebook)
  "Destructor for `ena:$notebook'."
  (ena:log-ignore-errors
    (ena:kernel-del (ena:$notebook-kernel notebook))))

(defun ena:notebook-close-worksheet (notebook ws)
  "Close worksheet WS in NOTEBOOK.  If WS is the last worksheet,
call notebook destructor `ena:notebook-del'."
  (symbol-macrolet ((worksheets (ena:$notebook-worksheets notebook))
                    (scratchsheets (ena:$notebook-scratchsheets notebook)))
    (cond
     ((ena:worksheet-p ws) (ena:worksheet-save-cells ws t))
     (t (setq scratchsheets (delq ws scratchsheets))))
    (unless (or (ena:filter (lambda (x)
                              (and (not (eq x ws))
                                   (ena:worksheet-has-buffer-p x)))
                            worksheets)
                scratchsheets)
      (ena:notebook-del notebook))))


;;; Notebook utility functions

(defun ena:notebook-buffer (notebook)
  "Return the buffer that is associated with NOTEBOOK."
  ;; FIXME: Find a better way to define notebook buffer!
  ;;        For example, the last accessed buffer.
  (let ((first-buffer
         (lambda (ws-list)
           (loop for ws in ws-list if (ena:worksheet-buffer ws) return it))))
    (or (funcall first-buffer (ena:$notebook-worksheets    notebook))
        (funcall first-buffer (ena:$notebook-scratchsheets notebook)))))

(defun ena:notebook-buffer-list (notebook)
  "Return the buffers associated with NOTEBOOK's kernel.
The buffer local variable `default-directory' of these buffers
will be updated with kernel's cwd."
  (ena:filter #'identity
              (mapcar #'ena:worksheet-buffer
                      (append (ena:$notebook-worksheets notebook)
                              (ena:$notebook-scratchsheets notebook)))))

(defun ena:notebook--get-nb-or-error ()
  (or ena:%notebook% (error "Not in notebook buffer.")))

;;;###autoload
(defalias 'ena:notebook-name 'ena:$notebook-notebook-name)

(defun ena:notebook-name-getter (notebook)
  (cons #'ena:notebook-name notebook))


;;; Open notebook

(defun ena:notebook-url (notebook)
  (ena:notebook-url-from-url-and-id (ena:$notebook-url-or-port notebook)
                                    (ena:$notebook-notebook-id notebook)))

(defun ena:notebook-url-from-url-and-id (url-or-port notebook-id)
  (ena:url url-or-port "notebooks" notebook-id))

(defun ena:notebook-pop-to-current-buffer (&rest -ignore-)
  "Default callback for `ena:notebook-open'."
  (pop-to-buffer (current-buffer)))

(defun ena:notebook-open (url-or-port notebook-id &optional callback cbargs)
  "Open notebook of NOTEBOOK-ID in the server URL-OR-PORT.
Opened notebook instance is returned.  Note that notebook might not be
ready at the time when this function is executed.

After the notebook is opened, CALLBACK is called as::

  \(apply CALLBACK notebook CREATED CBARGS)

where the second argument CREATED indicates whether the notebook
is newly created or not.  When CALLBACK is specified, buffer is
**not** brought up by `pop-to-buffer'.  It is caller's
responsibility to do so.  The current buffer is set to the
notebook buffer when CALLBACK is called."
  (unless callback (setq callback #'ena:notebook-pop-to-current-buffer))
  (let ((buffer (ena:notebook-get-opened-buffer url-or-port notebook-id)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (ena:log 'info "Notebook %s is already opened."
                   (ena:$notebook-notebook-name ena:%notebook%))
          (when callback
            (apply callback ena:%notebook% nil cbargs))
          ena:%notebook%)
      (ena:log 'info "Opening notebook %s..." notebook-id)
      (ena:notebook-request-open url-or-port notebook-id callback cbargs))))

(defun ena:notebook-request-open (url-or-port notebook-id
                                              &optional callback cbargs)
  "Request notebook of NOTEBOOK-ID to the server at URL-OR-PORT.
Return `ena:$notebook' instance.  Notebook may not be ready at
the time of execution.

CALLBACK is called as \(apply CALLBACK notebook t CBARGS).  The second
argument `t' indicates that the notebook is newly opened.
See `ena:notebook-open' for more information."
  (let ((url (ena:notebook-url-from-url-and-id url-or-port notebook-id))
        (notebook (ena:notebook-new url-or-port notebook-id)))
    (ena:log 'debug "Opening notebook at %s" url)
    (ena:query-singleton-ajax
     (list 'notebook-open url-or-port notebook-id)
     url
     :timeout ena:notebook-querty-timeout-open
     :parser #'ena:json-read
     :success (apply-partially
               #'ena:notebook-request-open-callback-with-callback
               notebook callback cbargs))
    notebook))

(defun ena:notebook-request-open-callback-with-callback (notebook
                                                         callback
                                                         cbargs
                                                         &rest args)
  (apply #'ena:notebook-request-open-callback notebook args)
  (when callback
    (with-current-buffer (ena:notebook-buffer notebook)
      (apply callback notebook t cbargs))))

(defun* ena:notebook-request-open-callback (notebook &key data
                                                     &allow-other-keys)
  (let ((notebook-id (ena:$notebook-notebook-id notebook)))
    (ena:notebook-bind-events notebook (ena:events-new))
    (ena:notebook-start-kernel notebook)
    (ena:notebook-from-json notebook data) ; notebook buffer is created here
    (setf (ena:$notebook-kernelinfo notebook)
          (ena:kernelinfo-new (ena:$notebook-kernel notebook)
                              (cons #'ena:notebook-buffer-list notebook)))
    (ena:notebook-put-opened-notebook notebook)
    (ena:notebook--check-nbformat data)
    (ena:log 'info "Notebook %s is ready"
             (ena:$notebook-notebook-name notebook))))

(defun ena:notebook--different-number (n1 n2)
  (and (numberp n1) (numberp n2) (not (= n1 n2))))

(defun ena:notebook--check-nbformat (data)
  "Warn user when nbformat is changed on server side.
See https://github.com/ipython/ipython/pull/1934 for the purpose
of minor mode."
  ;; See `Notebook.prototype.load_notebook_success'
  ;; at IPython/frontend/html/notebook/static/js/notebook.js
  (destructuring-bind (&key nbformat orig_nbformat
                            nbformat_minor orig_nbformat_minor
                            &allow-other-keys)
      data
    (cond
     ((ena:notebook--different-number nbformat orig_nbformat)
      (ena:display-warning
       (format "Notebook major version updated (v%d -> v%d).
  To not update version, do not save this notebook."
               orig_nbformat nbformat)))
     ((ena:notebook--different-number nbformat_minor orig_nbformat_minor)
      (ena:display-warning
       (format "This notebook is version v%s.%s, but IPython
  server you are using only fully support up to v%s.%s.
  Some features may not be available."
               orig_nbformat orig_nbformat_minor
               nbformat nbformat_minor))))))


;;; Initialization.

(defun ena:notebook-bind-events (notebook events)
  "Bind events related to PAGER to the event handler EVENTS."
  (setf (ena:$notebook-events notebook) events)
  (ena:worksheet-class-bind-events events)
  ;; Bind events for sub components:
  (setf (ena:$notebook-pager notebook)
        (ena:pager-new
         (format ena:notebook-pager-buffer-name-template
                 (ena:$notebook-url-or-port notebook)
                 (ena:$notebook-notebook-name notebook))
         (ena:$notebook-events notebook))))

(define-obsolete-function-alias
  'ena:notebook-show-in-shared-output
  'ena:shared-output-show-code-cell-at-point "0.1.2")


;;; Kernel related things

(defun ena:notebook-start-kernel (notebook)
  (let* ((base-url (concat ena:base-kernel-url "kernels"))
         (kernel (ena:kernel-new (ena:$notebook-url-or-port notebook)
                                 base-url
                                 (ena:$notebook-events notebook))))
    (setf (ena:$notebook-kernel notebook) kernel)
    (ena:pytools-setup-hooks kernel)
    (ena:kernel-start kernel
                      (ena:$notebook-notebook-id notebook))))

(defun ena:notebook-restart-kernel (notebook)
  (ena:kernel-restart (ena:$notebook-kernel notebook)))

(defun ena:notebook-restart-kernel-command ()
  "Send request to the server to restart kernel."
  (interactive)
  (if ena:%notebook%
      (when (y-or-n-p "Really restart kernel? ")
        (ena:notebook-restart-kernel ena:%notebook%))
    (ena:log 'error "Not in notebook buffer!")))

(define-obsolete-function-alias
  'ena:notebook-request-tool-tip-or-help-command
  'ena:pytools-request-tooltip-or-help "0.1.2")

(defun ena:notebook-complete-dot ()
  "Insert dot and request completion."
  (interactive)
  (if (and ena:%notebook% (ena:codecell-p (ena:get-cell-at-point)))
      (ena:completer-dot-complete)
    (insert ".")))

(defun ena:notebook-kernel-interrupt-command ()
  "Interrupt the kernel.
This is equivalent to do ``C-c`` in the console program."
  (interactive)
  (ena:kernel-interrupt (ena:$notebook-kernel ena:%notebook%)))

(defun ena:notebook-kernel-kill-command ()
  (interactive)
  (when (y-or-n-p "Really kill kernel?")
    (ena:kernel-kill (ena:$notebook-kernel ena:%notebook%))))

;; autoexec

(defun ena:notebook-execute-autoexec-cells (notebook)
  "Execute cells of which auto-execution flag is on."
  (interactive (list (or ena:%notebook% (error "Not in notebook buffer!"))))
  (mapc #'ena:worksheet-execute-autoexec-cells
        (ena:$notebook-worksheets notebook)))

(define-obsolete-function-alias
  'ena:notebook-eval-string
  'ena:shared-output-eval-string "0.1.2")


;;; Persistence and loading

(defun ena:notebook-set-notebook-name (notebook name)
  "Check NAME and change the name of NOTEBOOK to it."
  (if (ena:notebook-test-notebook-name name)
      (setf (ena:$notebook-notebook-name notebook) name)
    (ena:log 'error "%S is not a good notebook name." name)
    (error "%S is not a good notebook name." name)))

(defun ena:notebook-test-notebook-name (name)
  (and (stringp name)
       (> (length name) 0)
       (not (string-match "[\\/\\\\:]" name))))

(defun* ena:notebook--worksheet-new (notebook
                                     &optional (func #'ena:worksheet-new))
  (funcall func
           (ena:$notebook-nbformat notebook)
           (ena:notebook-name-getter notebook)
           (cons (lambda (notebook cell)
                   (ena:notebook-discard-output-p notebook cell))
                 notebook)
           (ena:$notebook-kernel notebook)
           (ena:$notebook-events notebook)))

(defun ena:notebook--worksheet-render (notebook ws)
  (ena:worksheet-render ws)
  (with-current-buffer (ena:worksheet-buffer ws)
    (ena:notebook-mode)
    ;; Now that major-mode is set, set buffer local variables:
    (ena:notebook--notification-setup notebook)
    (ena:notebook-setup-kill-buffer-hook)
    (ena:notebook-set-buffer-file-name-maybe notebook)
    (setq ena:%notebook% notebook)))

(defun ena:notebook--notification-setup (notebook)
  (ena:notification-setup
   (current-buffer)
   (ena:$notebook-events notebook)
   :get-list
   (lambda () (ena:$notebook-worksheets ena:%notebook%))
   :get-current
   (lambda () ena:%worksheet%)
   :get-name
   #'ena:worksheet-name
   :get-buffer
   (lambda (ws)
     (ena:notebook-worksheet--render-maybe ena:%notebook% ws "clicked")
     (ena:worksheet-buffer ws))
   :delete
   (lambda (ws)
     (ena:notebook-worksheet-delete ena:%notebook% ws t))
   :insert-prev
   (lambda (ws) (ena:notebook-worksheet-insert-prev ena:%notebook% ws))
   :insert-next
   (lambda (ws) (ena:notebook-worksheet-insert-next ena:%notebook% ws))
   :move-prev
   (lambda (ws) (ena:notebook-worksheet-move-prev ena:%notebook% ws))
   :move-next
   (lambda (ws) (ena:notebook-worksheet-move-next ena:%notebook% ws))
   ))

(defun ena:notebook-set-buffer-file-name-maybe (notebook)
  "Set `buffer-file-name' of the current buffer to ipynb file
of NOTEBOOK."
  (when ena:notebook-set-buffer-file-name
    (ena:notebook-fetch-data
     notebook
     (lambda (data notebook buffer)
       (with-current-buffer buffer
         (destructuring-bind (&key project &allow-other-keys)
             data
           (setq buffer-file-name
                 (expand-file-name
                  (format "%s.ipynb"
                          (ena:$notebook-notebook-name notebook))
                  project)))))
     (list notebook (current-buffer)))))

(defun ena:notebook-from-json (notebook data)
  (destructuring-bind (&key metadata nbformat nbformat_minor
                            &allow-other-keys)
      data
    (setf (ena:$notebook-metadata notebook) metadata)
    (setf (ena:$notebook-nbformat notebook) nbformat)
    (setf (ena:$notebook-nbformat-minor notebook) nbformat_minor)
    (setf (ena:$notebook-notebook-name notebook) (plist-get metadata :name)))
  (setf (ena:$notebook-worksheets notebook)
        (mapcar (lambda (ws-data)
                  (ena:worksheet-from-json
                   (ena:notebook--worksheet-new notebook) ws-data))
                (or (plist-get data :worksheets)
                    (list nil))))
  (ena:notebook--worksheet-render notebook
                                  (nth 0 (ena:$notebook-worksheets notebook)))
  notebook)

(defun ena:notebook-to-json (notebook)
  "Return json-ready alist."
  (let ((worksheets (mapcar #'ena:worksheet-to-json
                            (ena:$notebook-worksheets notebook))))
    `((worksheets . ,(apply #'vector worksheets))
      (metadata . ,(ena:$notebook-metadata notebook)))))

(defun ena:notebook-save-notebook (notebook retry &optional callback cbarg)
  (let ((data (ena:notebook-to-json notebook)))
    (plist-put (cdr (assq 'metadata data))
               :name (ena:$notebook-notebook-name notebook))
    (push `(nbformat . ,(ena:$notebook-nbformat notebook)) data)
    (ena:aif (ena:$notebook-nbformat-minor notebook)
        ;; Do not set nbformat when it is not given from server.
        (push `(nbformat_minor . ,it) data))
    (ena:events-trigger (ena:$notebook-events notebook)
                        'notebook_saving.Notebook)
    (ena:query-singleton-ajax
     (list 'notebook-save
           (ena:$notebook-url-or-port notebook)
           (ena:$notebook-notebook-id notebook))
     (ena:notebook-url notebook)
     :timeout ena:notebook-querty-timeout-save
     :type "PUT"
     :headers '(("Content-Type" . "application/json"))
     :data (json-encode data)
     :error (apply-partially #'ena:notebook-save-notebook-error notebook)
     :success (apply-partially #'ena:notebook-save-notebook-workaround
                               notebook retry callback cbarg)
     :status-code
     `((204 . ,(apply-partially
                (lambda (notebook callback cbarg &rest rest)
                  (apply #'ena:notebook-save-notebook-success
                         notebook rest)
                  (when callback
                    (apply callback cbarg rest)))
                notebook callback cbarg))))))

(defun ena:notebook-save-notebook-command ()
  "Save the notebook."
  (interactive)
  (ena:notebook-save-notebook ena:%notebook% 0))

(defun* ena:notebook-save-notebook-workaround
    (notebook retry callback cbarg
              &key
              status
              response
              &allow-other-keys
              &aux
              (response-status (request-response-status-code response)))
  ;; IPython server returns 204 only when the notebook URL is
  ;; accessed via PUT or DELETE.  As it seems Emacs failed to
  ;; choose PUT method every two times, let's check the response
  ;; here and fail when 204 is not returned.
  (unless (eq response-status 204)
    (with-current-buffer (ena:notebook-buffer notebook)
      (if (< retry ena:notebook-save-retry-max)
          (progn
            (ena:log 'info "Retry saving... Next count: %s" (1+ retry))
            (ena:notebook-save-notebook notebook (1+ retry)
                                        callback cbarg))
        (ena:notebook-save-notebook-error notebook :status status)
        (ena:log 'info
          "Status code (=%s) is not 204 and retry exceeds limit (=%s)."
          response-status ena:notebook-save-retry-max)))))

(defun ena:notebook-save-notebook-success (notebook &rest ignore)
  (ena:log 'info "Notebook is saved.")
  (setf (ena:$notebook-dirty notebook) nil)
  (mapc (lambda (ws)
          (ena:worksheet-save-cells ws) ; [#]_
          (ena:worksheet-set-modified-p ws nil))
        (ena:$notebook-worksheets notebook))
  (ena:events-trigger (ena:$notebook-events notebook)
                      'notebook_saved.Notebook))
;; .. [#] Consider the following case.
;;    (1) Open worksheet WS0 and other worksheets.
;;    (2) Edit worksheet WS0 then save the notebook.
;;    (3) Edit worksheet WS0.
;;    (4) Kill WS0 buffer by discarding the edit.
;;    (5) Save the notebook.
;;    This should save the latest WS0.  To do so, WS0 at the point (2)
;;    must be cached in the worksheet slot `:saved-cells'.

(defun* ena:notebook-save-notebook-error (notebook &key symbol-status
                                                   &allow-other-keys)
  (if (eq symbol-status 'user-cancel)
      (ena:log 'info "Cancel saving notebook.")
    (ena:log 'info "Failed to save notebook!")
    (ena:events-trigger (ena:$notebook-events notebook)
                        'notebook_save_failed.Notebook)))

(defun ena:notebook-rename-command (name)
  "Rename current notebook and save it immediately.

NAME is any non-empty string that does not contain '/' or '\\'."
  (interactive
   (list (read-string "Rename notebook: "
                      (let ((name (ena:$notebook-notebook-name ena:%notebook%)))
                        (unless (string-match "Untitled[0-9]+" name)
                          name)))))
  (ena:notebook-set-notebook-name ena:%notebook% name)
  (mapc #'ena:worksheet-set-buffer-name
        (ena:$notebook-worksheets ena:%notebook%))
  (ena:notebook-save-notebook
   ena:%notebook% 0
   (lambda (notebook &rest ignore)
     (with-current-buffer (ena:notebook-buffer notebook)
       (run-hooks 'ena:notebook-after-rename-hook)))
   ena:%notebook%))

(defun ena:notebook-close (notebook)
  "Close NOTEBOOK and kill its buffer."
  (interactive (prog1 (list (ena:notebook--get-nb-or-error))
                 (or (ena:notebook-ask-before-kill-buffer)
                     (error "Quit"))))
  (let ((ena:notebook-kill-buffer-ask nil))
    ;; Let `ena:notebook-kill-buffer-callback' do its job.
    (mapc #'kill-buffer (ena:notebook-buffer-list notebook))))

(defun ena:notebook-kill-kernel-then-close-command ()
  "Kill kernel and then kill notebook buffer.
To close notebook without killing kernel, just close the buffer
as usual."
  (interactive)
  (when (ena:notebook-ask-before-kill-buffer)
    (let ((kernel (ena:$notebook-kernel ena:%notebook%)))
      ;; If kernel is live, kill it before closing.
      (if (ena:kernel-live-p kernel)
          (ena:kernel-kill kernel #'ena:notebook-close (list ena:%notebook%))
        (ena:notebook-close ena:%notebook%)))))


;;; Worksheet

(defmacro ena:notebook--worksheet-render-new (notebook type)
  "Create new worksheet of TYPE in NOTEBOOK."
  (let ((func (intern (format "ena:%s-new" type)))
        (slot (list (intern (format "ena:$notebook-%ss" type)) notebook)))
    `(let ((ws (ena:notebook--worksheet-new ,notebook #',func)))
       (setf ,slot (append ,slot (list ws)))
       (ena:notebook--worksheet-render ,notebook ws)
       ws)))

(defun ena:notebook-worksheet-render-new (notebook)
  "Create new worksheet in NOTEBOOK."
  (ena:notebook--worksheet-render-new notebook worksheet))

(defun ena:notebook-worksheet-open-next-or-new (notebook ws &optional show)
  "Open next worksheet.  Create new if none.

Try to open the worksheet to the worksheet WS using the function
`ena:notebook-worksheet-open-next', open a new worksheet if not
found.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ena:notebook--get-nb-or-error)
                     (ena:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (ena:notebook-worksheet-open-next notebook ws)))
    (unless next
      (ena:log 'info "Creating new worksheet...")
      (setq next (ena:notebook-worksheet-render-new notebook))
      (ena:log 'info "Creating new worksheet... Done."))
    (when show
      (funcall show (ena:worksheet-buffer next)))))

(defun ena:notebook-worksheet-open-next-or-first (notebook ws &optional show)
  "Open next or first worksheet.

Try to open the worksheet to the worksheet WS using the function
`ena:notebook-worksheet-open-next', open the first worksheet if
not found.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ena:notebook--get-nb-or-error)
                     (ena:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (ena:notebook-worksheet-open-next notebook ws)))
    (unless next
      (setq next (car (ena:$notebook-worksheets notebook))))
    (when show
      (funcall show (ena:worksheet-buffer next)))))

(defun ena:notebook-worksheet-open-prev-or-last (notebook ws &optional show)
  "Open previous or last worksheet.
See also `ena:notebook-worksheet-open-next-or-first' and
`ena:notebook-worksheet-open-prev'."
  (interactive (list (ena:notebook--get-nb-or-error)
                     (ena:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((prev (ena:notebook-worksheet-open-prev notebook ws)))
    (unless prev
      (setq prev (car (last (ena:$notebook-worksheets notebook)))))
    (when show
      (funcall show (ena:worksheet-buffer prev)))))

(defun* ena:notebook-worksheet--render-maybe
    (notebook ws &optional (adj "next"))
  "Render worksheet WS of NOTEBOOK if it does not have buffer.
ADJ is a adjective to describe worksheet to be rendered."
  (if (ena:worksheet-has-buffer-p ws)
      (ena:log 'verbose "The worksheet already has a buffer.")
    (ena:log 'info "Rendering %s worksheet..." adj)
    (ena:notebook--worksheet-render notebook ws)
    (ena:log 'info "Rendering %s worksheet... Done." adj)))

(defun* ena:notebook-worksheet--open-new
    (notebook new &optional (adj "next") show)
  "Open (possibly new) worksheet NEW of NOTEBOOK with SHOW function.
ADJ is a adjective to describe worksheet to be opened.
SHOW is a function to be called with worksheet buffer if given."
  (when new
    (ena:notebook-worksheet--render-maybe notebook new adj))
  (when show
    (assert (ena:worksheet-p new) nil "No %s worksheet." adj)
    (funcall show (ena:worksheet-buffer new))))

(defun ena:notebook-worksheet-open-next (notebook ws &optional show)
  "Open next worksheet.

Search the worksheet after the worksheet WS, render it if it is
not yet, then return the worksheet.  If there is no such
worksheet, return nil.  Open the first worksheet if the worksheet
WS is an instance of `ena:scratchsheet'.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ena:notebook--get-nb-or-error)
                     (ena:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (if (ena:scratchsheet-p ws)
                  (car (ena:$notebook-worksheets notebook))
                (loop with worksheets = (ena:$notebook-worksheets notebook)
                      for current in worksheets
                      for next in (cdr worksheets)
                      when (eq current ws) return next))))
    (ena:notebook-worksheet--open-new notebook next "next" show)
    next))

(defun ena:notebook-worksheet-open-prev (notebook ws &optional show)
  "Open previous worksheet.
See also `ena:notebook-worksheet-open-next'."
  (interactive (list (ena:notebook--get-nb-or-error)
                     (ena:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((prev (if (ena:scratchsheet-p ws)
                  (car (last (ena:$notebook-worksheets notebook)))
                (loop for (prev current) on (ena:$notebook-worksheets notebook)
                      when (eq current ws) return prev))))
    (ena:notebook-worksheet--open-new notebook prev "previous" show)
    prev))

(defun ena:notebook-worksheet-open-ith (notebook i &optional show)
  "Open I-th (zero-origin) worksheet."
  (let ((ws (nth i (ena:$notebook-worksheets notebook))))
    (unless ws (error "No %s-th worksheet" (1+ i)))
    (ena:notebook-worksheet--open-new notebook ws (format "%s-th" i) show)))

(defmacro ena:notebook-worksheet--defun-open-nth (n)
  "Define a command to open N-th (one-origin) worksheet."
  (assert (and (integerp n) (> n 0)) t)
  (let ((func (intern (format "ena:notebook-worksheet-open-%sth" n))))
    `(defun ,func (notebook &optional show)
       ,(format "Open %d-th worksheet." n)
       (interactive (list (ena:notebook--get-nb-or-error)
                          #'switch-to-buffer))
       (ena:notebook-worksheet-open-ith notebook ,(1- n) show))))

(defmacro ena:notebook-worksheet--defun-all-open-nth (min max)
  `(progn
     ,@(loop for n from min to max
             collect `(ena:notebook-worksheet--defun-open-nth ,n))))

(ena:notebook-worksheet--defun-all-open-nth 1 8)

(defun ena:notebook-worksheet-open-last (notebook &optional show)
  "Open the last worksheet."
  (interactive (list (ena:notebook--get-nb-or-error)
                     #'switch-to-buffer))
  (let ((last (car (last (ena:$notebook-worksheets notebook)))))
    (ena:notebook-worksheet--open-new notebook last "last" show)
    last))

(defun ena:notebook-worksheet-insert-new (notebook ws &optional render show
                                                   inserter)
  (let ((new (ena:notebook--worksheet-new notebook)))
    (setf (ena:$notebook-worksheets notebook)
          (funcall inserter (ena:$notebook-worksheets notebook) ws new))
    (when (or render show)
      (ena:notebook--worksheet-render notebook new))
    (when show
      (funcall show (ena:worksheet-buffer new)))
    new))

(defun* ena:notebook-worksheet-insert-next
    (notebook ws &optional (render t) (show #'switch-to-buffer))
  "Insert a new worksheet after this worksheet and open it.
See also `ena:notebook-worksheet-insert-prev'.

.. The worksheet WS is searched in the worksheets slot of
   NOTEBOOK and a newly created worksheet is inserted after WS.
   Worksheet buffer is created when RENDER or SHOW is non-`nil'.
   SHOW is a function which take a buffer."
  (interactive (list (ena:notebook--get-nb-or-error)
                     (ena:worksheet--get-ws-or-error)))
  (ena:notebook-worksheet-insert-new notebook ws render show
                                     #'ena:list-insert-after))

(defun* ena:notebook-worksheet-insert-prev
    (notebook ws &optional (render t) (show #'switch-to-buffer))
  "Insert a new worksheet before this worksheet and open it.
See also `ena:notebook-worksheet-insert-next'."
  (interactive (list (ena:notebook--get-nb-or-error)
                     (ena:worksheet--get-ws-or-error)))
  (ena:notebook-worksheet-insert-new notebook ws render show
                                     #'ena:list-insert-before))

(defun ena:notebook-worksheet-delete (notebook ws &optional confirm)
  "Delete the current worksheet.
When used as a lisp function, delete worksheet WS from NOTEBOOk."
  (interactive (list (ena:notebook--get-nb-or-error)
                     (ena:worksheet--get-ws-or-error)
                     t))
  (when confirm
    (unless (y-or-n-p
             "Really remove this worksheet? There is no undo.")
      (error "Quit deleting the current worksheet.")))
  (setf (ena:$notebook-worksheets notebook)
        (delq ws (ena:$notebook-worksheets notebook)))
  (setf (ena:$notebook-dirty notebook) t)
  (let ((ena:notebook-kill-buffer-ask nil))
    (kill-buffer (ena:worksheet-buffer ws))))

(defun ena:notebook-worksheet-move-prev (notebook ws)
  "Switch the current worksheet with the previous one."
  (interactive (list (ena:notebook--get-nb-or-error)
                     (ena:worksheet--get-ws-or-error)))
  (assert (ena:worksheet-p ws) nil "Not worksheet.")
  (setf (ena:$notebook-worksheets notebook)
        (ena:list-move-left (ena:$notebook-worksheets notebook) ws)))

(defun ena:notebook-worksheet-move-next (notebook ws)
  "Switch the current worksheet with the previous one."
  (interactive (list (ena:notebook--get-nb-or-error)
                     (ena:worksheet--get-ws-or-error)))
  (assert (ena:worksheet-p ws) nil "Not worksheet.")
  (setf (ena:$notebook-worksheets notebook)
        (ena:list-move-right (ena:$notebook-worksheets notebook) ws)))

(defun* ena:notebook-worksheet-index
    (&optional (notebook ena:%notebook%)
               (ws ena:%worksheet%))
  "Return an index of the worksheet WS in NOTEBOOK."
  (loop for i from 0
        for ith-ws in (ena:$notebook-worksheets notebook)
        when (eq ith-ws ws)
        return i))


;;; Scratch sheet

(defun ena:notebook-scratchsheet-render-new (notebook)
  "Create new scratchsheet in NOTEBOOK."
  (ena:notebook--worksheet-render-new notebook scratchsheet))

(defun ena:notebook-scratchsheet-open (notebook &optional new popup)
  "Open \"scratch sheet\".
Open a new one when prefix argument is given.
Scratch sheet is almost identical to worksheet.  However, EIN
will not save the buffer.  Use this buffer like of normal IPython
console.  Note that you can always copy cells into the normal
worksheet to save result."
  (interactive (list (ena:get-notebook-or-error)
                     current-prefix-arg
                     t))
  (let ((ss (or (unless new
                  (car (ena:$notebook-scratchsheets notebook)))
                (ena:notebook-scratchsheet-render-new notebook))))
    (when popup
      (pop-to-buffer (ena:worksheet-buffer ss)))
    ss))


;;; Opened notebooks

(defvar ena:notebook--opened-map (make-hash-table :test 'equal)
  "A map: (URL-OR-PORT NOTEBOOK-ID) => notebook instance.")

(defun ena:notebook-get-opened-notebook (url-or-port notebook-id)
  (gethash (list url-or-port notebook-id) ena:notebook--opened-map))

(defun ena:notebook-get-opened-buffer (url-or-port notebook-id)
  (ena:aand (ena:notebook-get-opened-notebook url-or-port notebook-id)
            (ena:notebook-buffer it)))

(defun ena:notebook-put-opened-notebook (notebook)
  (puthash (list (ena:$notebook-url-or-port notebook)
                 (ena:$notebook-notebook-id notebook))
           notebook
           ena:notebook--opened-map))

(defun ena:notebook-opened-notebooks (&optional predicate)
  "Return list of opened notebook instances.
If PREDICATE is given, notebooks are filtered by PREDICATE.
PREDICATE is called with each notebook and notebook is included
in the returned list only when PREDICATE returns non-nil value."
  (let (notebooks)
    (maphash (lambda (k n) (if (ena:notebook-live-p n)
                               (push n notebooks)
                             (remhash k ena:notebook--opened-map)))
             ena:notebook--opened-map)
    (if predicate
        (ena:filter predicate notebooks)
      notebooks)))

(defun ena:notebook-opened-buffers (&optional predicate)
  "Return list of opened notebook buffers."
  (mapcar #'ena:notebook-buffer (ena:notebook-opened-notebooks predicate)))

(defun ena:notebook-opened-buffer-names (&optional predicate)
  "Return list of opened notebook buffer names."
  (mapcar #'buffer-name (ena:notebook-opened-buffers predicate)))


;;; Generic getter

(defun ena:get-url-or-port--notebook ()
  (when ena:%notebook% (ena:$notebook-url-or-port ena:%notebook%)))

(defun ena:get-notebook--notebook ()
  ena:%notebook%)

(defun ena:get-kernel--notebook ()
  (when (ena:$notebook-p ena:%notebook%)
    (ena:$notebook-kernel ena:%notebook%)))


;;; Predicate

(defun ena:notebook-buffer-p ()
  "Return non-`nil' if current buffer is notebook buffer."
  ena:%notebook%)

(defun ena:notebook-live-p (notebook)
  "Return non-`nil' if NOTEBOOK has live buffer."
  (buffer-live-p (ena:notebook-buffer notebook)))

(defun ena:notebook-modified-p (&optional notebook)
  "Return non-nil if NOTEBOOK is modified.
If NOTEBOOK is not given or nil then consider the notebook
associated with current buffer (if any)."
  (unless notebook (setq notebook ena:%notebook%))
  (and (ena:$notebook-p notebook)
       (ena:notebook-live-p notebook)
       (or (ena:$notebook-dirty notebook)
           (loop for ws in (ena:$notebook-worksheets notebook)
                 when (ena:worksheet-modified-p ws)
                 return t))))


;;; Notebook mode

(defcustom ena:notebook-modes
  '(ena:notebook-multilang-mode)
  "Notebook modes to use \(in order of preference).

When the notebook is opened, mode in this value is checked one by one
and the first usable mode is used.

Available modes:

* `ena:notebook-multilang-mode'
* `ena:notebook-mumamo-mode'
* `ena:notebook-python-mode'
* `ena:notebook-plain-mode'

Examples:

Use MuMaMo if it is installed.  Otherwise, use plain mode.
This is the old default setting::

  (setq ena:notebook-modes '(ena:notebook-mumamo-mode ena:notebook-plain-mode))

Avoid using MuMaMo even when it is installed::

  (setq ena:notebook-modes '(ena:notebook-plain-mode))

Use simple `python-mode' based notebook mode when MuMaMo is not installed::

  (setq ena:notebook-modes '(ena:notebook-mumamo-mode ena:notebook-python-mode))
"
  :type '(repeat (choice (const :tag "Multi-lang" ena:notebook-multilang-mode)
                         (const :tag "MuMaMo" ena:notebook-mumamo-mode)
                         (const :tag "Only Python" ena:notebook-python-mode)
                         (const :tag "Plain" ena:notebook-plain-mode)))
  :group 'ena)

(defcustom ena:notebook-mode-hook nil
  "Hook for `ena:notebook-mode'.
This hook is run regardless the actual major mode used."
  :type 'hook
  :group 'ena)

(defun ena:notebook-choose-mode ()
  "Return usable (defined) notebook mode."
  ;; So try to load extra modules here.
  (when (require 'mumamo nil t)
    (require 'ena-mumamo))
  ;; Return first matched mode
  (loop for mode in ena:notebook-modes
        if (functionp mode)
        return mode))

(defvar ena:notebook-mode-map (make-sparse-keymap))

(let ((map ena:notebook-mode-map))
  (define-key map "\C-c\C-c" 'ena:worksheet-execute-cell)
  (define-key map (kbd "M-RET") 'ena:worksheet-execute-cell-and-goto-next)
  (define-key map (kbd "<M-S-return>")
    'ena:worksheet-execute-cell-and-insert-below)
  (define-key map (kbd "C-c C-'") 'ena:worksheet-turn-on-autoexec)
  (define-key map "\C-c\C-e" 'ena:worksheet-toggle-output)
  (define-key map "\C-c\C-v" 'ena:worksheet-set-output-visibility-all)
  (define-key map "\C-c\C-l" 'ena:worksheet-clear-output)
  (define-key map (kbd "C-c C-S-l") 'ena:worksheet-clear-all-output)
  (define-key map (kbd "C-c C-;") 'ena:shared-output-show-code-cell-at-point)
  (define-key map "\C-c\C-k" 'ena:worksheet-kill-cell)
  (define-key map "\C-c\M-w" 'ena:worksheet-copy-cell)
  (define-key map "\C-c\C-w" 'ena:worksheet-copy-cell)
  (define-key map "\C-c\C-y" 'ena:worksheet-yank-cell)
  (define-key map "\C-c\C-a" 'ena:worksheet-insert-cell-above)
  (define-key map "\C-c\C-b" 'ena:worksheet-insert-cell-below)
  (define-key map "\C-c\C-t" 'ena:worksheet-toggle-cell-type)
  (define-key map "\C-c\C-u" 'ena:worksheet-change-cell-type)
  (define-key map "\C-c\C-s" 'ena:worksheet-split-cell-at-point)
  (define-key map "\C-c\C-m" 'ena:worksheet-merge-cell)
  (define-key map "\C-c\C-n" 'ena:worksheet-goto-next-input)
  (define-key map "\C-c\C-p" 'ena:worksheet-goto-prev-input)
  (define-key map (kbd "C-<up>") 'ena:worksheet-goto-prev-input)
  (define-key map (kbd "C-<down>") 'ena:worksheet-goto-next-input)
  (define-key map (kbd "C-c <up>") 'ena:worksheet-move-cell-up)
  (define-key map (kbd "C-c <down>") 'ena:worksheet-move-cell-down)
  (define-key map (kbd "M-<up>") 'ena:worksheet-move-cell-up)
  (define-key map (kbd "M-<down>") 'ena:worksheet-move-cell-down)
  (define-key map "\C-c\C-f" 'ena:pytools-request-tooltip-or-help)
  (define-key map "\C-c\C-i" 'ena:completer-complete)
  (define-key map "\C-c\C-x" 'ena:tb-show)
  (define-key map "\C-c\C-r" 'ena:notebook-restart-kernel-command)
  (define-key map "\C-c\C-z" 'ena:notebook-kernel-interrupt-command)
  (define-key map "\C-c\C-q" 'ena:notebook-kill-kernel-then-close-command)
  (define-key map (kbd "C-c C-#") 'ena:notebook-close)
  (define-key map (kbd "C-:") 'ena:shared-output-eval-string)
  (define-key map "\C-c\C-o" 'ena:console-open)
  (define-key map "\C-x\C-s" 'ena:notebook-save-notebook-command)
  (define-key map "\C-x\C-w" 'ena:notebook-rename-command)
  (define-key map "\M-."          'ena:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ena:pytools-jump-to-source-command)
  (define-key map "\M-,"          'ena:pytools-jump-back-command)
  (define-key map (kbd "C-c C-,") 'ena:pytools-jump-back-command)
  (define-key map "\M-p"          'ena:worksheet-previous-input-history)
  (define-key map "\M-n"          'ena:worksheet-next-input-history)
  (define-key map (kbd "C-c C-/") 'ena:notebook-scratchsheet-open)
  ;; Worksheets
  (define-key map (kbd "C-c !")     'ena:worksheet-rename-sheet)
  (define-key map (kbd "C-c {")     'ena:notebook-worksheet-open-prev-or-last)
  (define-key map (kbd "C-c }")     'ena:notebook-worksheet-open-next-or-first)
  (define-key map (kbd "C-c M-{")   'ena:notebook-worksheet-move-prev)
  (define-key map (kbd "C-c M-}")   'ena:notebook-worksheet-move-next)
  (define-key map (kbd "C-c +")     'ena:notebook-worksheet-insert-next)
  (define-key map (kbd "C-c M-+")   'ena:notebook-worksheet-insert-prev)
  (define-key map (kbd "C-c -")     'ena:notebook-worksheet-delete)
  (loop for n from 1 to 8
        do (define-key map (format "\C-c%d" n)
             (intern (format "ena:notebook-worksheet-open-%sth" n))))
  (define-key map "\C-c9" 'ena:notebook-worksheet-open-last)
  ;; Menu
  (easy-menu-define ena:notebook-menu map "EIN Notebook Mode Menu"
    `("EIN Notebook"
      ("File"
       ,@(ena:generate-menu
          '(("Save notebook" ena:notebook-save-notebook-command)
            ("Rename notebook" ena:notebook-rename-command)
            ("Close notebook without saving"
             ena:notebook-close)
            ("Kill kernel then close notebook"
             ena:notebook-kill-kernel-then-close-command))))
      ("Edit"
       ,@(ena:generate-menu
          '(("Kill cell" ena:worksheet-kill-cell)
            ("Copy cell" ena:worksheet-copy-cell)
            ("Yank cell" ena:worksheet-yank-cell)
            ("Insert cell above" ena:worksheet-insert-cell-above)
            ("Insert cell below" ena:worksheet-insert-cell-below)
            ("Toggle cell type" ena:worksheet-toggle-cell-type)
            ("Change cell type" ena:worksheet-change-cell-type)
            ("Split cell at point" ena:worksheet-split-cell-at-point)
            ("Merge cell" ena:worksheet-merge-cell)
            ("Go to next cell" ena:worksheet-goto-next-input)
            ("Go to previous cell" ena:worksheet-goto-prev-input)
            ("Move cell up" ena:worksheet-move-cell-up)
            ("Move cell down" ena:worksheet-move-cell-down)
            ("Dedent text in CELL" ena:worksheet-dedent-cell-text)
            )))
      ("Cell/Code"
       ,@(ena:generate-menu
          '(("Execute cell" ena:worksheet-execute-cell
             :active (ena:worksheet-at-codecell-p))
            ("Execute cell and go to next"
             ena:worksheet-execute-cell-and-goto-next
             :active (ena:worksheet-at-codecell-p))
            ("Execute cell and insert below"
             ena:worksheet-execute-cell-and-insert-below
             :active (ena:worksheet-at-codecell-p))
            ("Execute all"
             ena:worksheet-execute-all-cell)
            ("Turn on auto execution flag" ena:worksheet-turn-on-autoexec
             :active (ena:worksheet-at-codecell-p))
            ("Evaluate code in minibuffer" ena:shared-output-eval-string)
            ("Toggle instant cell execution mode" ena:iexec-mode)
            ))
       "---"
       ,@(ena:generate-menu
          '(("Toggle output visibility" ena:worksheet-toggle-output
             :active (ena:worksheet-at-codecell-p))
            ("Show all output"
             ena:worksheet-set-output-visibility-all)
            ("Discard output" ena:worksheet-clear-output
             :active (ena:worksheet-at-codecell-p))
            ("Discard all output" ena:worksheet-clear-all-output)
            ("Show full output" ena:shared-output-show-code-cell-at-point
             :active (ena:worksheet-at-codecell-p))
            ("Traceback viewer" ena:tb-show)
            ))
       "---"
       ,@(ena:generate-menu
          '(("Show object help"
             ena:pytools-request-tooltip-or-help)
            ("Complete code" ena:completer-complete
             :active (ena:worksheet-at-codecell-p))
            ("Jump to definition" ena:pytools-jump-to-source-command)
            ("Go back to the previous jump point"
             ena:pytools-jump-back-command)
            ("Previous input history"
             ena:worksheet-previous-input-history)
            ("Next input history"
             ena:worksheet-next-input-history))))
      ("Kernel"
       ,@(ena:generate-menu
          '(("Restart kernel" ena:notebook-restart-kernel-command)
            ("Interrupt kernel" ena:notebook-kernel-interrupt-command))))
      ("Worksheets [Experimental]"
       ,@(ena:generate-menu
          '(("Rename worksheet" ena:worksheet-rename-sheet)
            ("Insert next worksheet"
             ena:notebook-worksheet-insert-next)
            ("Insert previous worksheet"
             ena:notebook-worksheet-insert-prev)
            ("Delete worksheet" ena:notebook-worksheet-delete)
            ("Move worksheet left"  ena:notebook-worksheet-move-prev)
            ("Move worksheet right" ena:notebook-worksheet-move-next)
            ))
       "---"
       ,@(ena:generate-menu
          '(("Open previous worksheet"
             ena:notebook-worksheet-open-prev)
            ("Open previous or last worksheet"
             ena:notebook-worksheet-open-prev-or-last)
            ("Open next worksheet"
             ena:notebook-worksheet-open-next)
            ("Open next or first worksheet"
             ena:notebook-worksheet-open-next-or-first)
            ("Open next or new worksheet"
             ena:notebook-worksheet-open-next-or-new)
            ))
       "---"
       ,@(ena:generate-menu
          (append
           (loop for n from 1 to 8
                 collect
                 (list
                  (format "Open %d-th worksheet" n)
                  (intern (format "ena:notebook-worksheet-open-%sth" n))))
           '(("Open last worksheet" ena:notebook-worksheet-open-last)))))
      ("Junk notebook"
       ,@(ena:generate-menu
          '(("Junk this notebook" ena:junk-rename)
            ("Open new junk" ena:junk-new))))
      ;; Misc:
      ,@(ena:generate-menu
         '(("Open regular IPython console" ena:console-open)
           ("Open scratch sheet" ena:notebook-scratchsheet-open)
           ("Toggle pseudo console mode" ena:pseudo-console-mode)
           ))
      ))
  map)

(defun ena:notebook-mode ()
  (funcall (ena:notebook-choose-mode))
  (ena:complete-on-dot-install
   ena:notebook-mode-map 'ena:notebook-complete-dot)
  (ena:aif ena:helm-kernel-history-search-key
      (define-key ena:notebook-mode-map it 'helm-ena-kernel-history))
  (ena:aif ena:anything-kernel-history-search-key
      (define-key ena:notebook-mode-map it 'anything-ena-kernel-history))
  (ena:notebook-minor-mode +1)
  (run-hooks 'ena:notebook-mode-hook))

(add-hook 'ena:notebook-mode-hook 'ena:worksheet-imenu-setup)

(define-minor-mode ena:notebook-minor-mode
  "Minor mode to install `ena:notebook-mode-map' for `ena:notebook-mode'."
  :keymap ena:notebook-mode-map
  :group 'ena)

;; To avoid MuMaMo to discard `ena:notebook-minor-mode', make it
;; permanent local.
(put 'ena:notebook-minor-mode 'permanent-local t)

(define-derived-mode ena:notebook-plain-mode fundamental-mode "ena:notebook"
  "IPython notebook mode without fancy coloring."
  (font-lock-mode))

(define-derived-mode ena:notebook-python-mode python-mode "ena:python"
  "Use `python-mode' for whole notebook buffer.")

(defun ena:notebook-open-in-browser (&optional print)
  "Open current notebook in web browser.
When the prefix argument (``C-u``) is given, print page is opened.
Note that print page is not supported in IPython 0.12.1."
  (interactive "P")
  (let ((url (apply #'ena:url
                    (ena:$notebook-url-or-port ena:%notebook%)
                    (ena:$notebook-notebook-id ena:%notebook%)
                    (if print (list "print")))))
    (message "Opening %s in browser" url)
    (browse-url url)))

(defun ena:notebook-fetch-data (notebook callback &optional cbargs)
  "Fetch data in body tag of NOTEBOOK html page.
CALLBACK is called with a plist with data in the body tag as
the first argument and CBARGS as the rest of arguments."
  (let ((url-or-port (ena:$notebook-url-or-port notebook))
        (notebook-id (ena:$notebook-notebook-id notebook)))
    (ena:query-singleton-ajax
     (list 'notebook-fetch-data url-or-port notebook-id)
     (ena:url url-or-port notebook-id)
     :parser
     (lambda ()
       (list
        :project
        (ena:html-get-data-in-body-tag "data-project")
        :base-project-url
        (ena:html-get-data-in-body-tag "data-base-project-url")
        :base-kernel-url
        (ena:html-get-data-in-body-tag "data-base-kernel-url")
        :read-only
        (ena:html-get-data-in-body-tag "data-read-only")
        :notebook-id
        (ena:html-get-data-in-body-tag "data-notebook-id")))
     :success
     (apply-partially (function*
                       (lambda (callback cbargs &key data &allow-other-keys)
                         (apply callback data cbargs)))
                      callback cbargs))))


;;; Buffer and kill hooks

(defcustom ena:notebook-kill-buffer-ask t
  "Whether EIN should ask before killing unsaved notebook buffer."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'ena)

;; -- `kill-buffer-query-functions'
(defun ena:notebook-ask-before-kill-buffer ()
  "Return `nil' to prevent killing the notebook buffer.
Called via `kill-buffer-query-functions'."
  (not (or (and ena:notebook-kill-buffer-ask
                (ena:worksheet-p ena:%worksheet%) ; it's not `ena:scratchsheet'
                (ena:notebook-modified-p)
                (not (y-or-n-p
                      "You have unsaved changes. Discard changes?")))
           (when (ena:worksheet-p ena:%worksheet%)
             ;; To make `ena:worksheet-save-cells' no-op.
             (ena:worksheet-dont-save-cells ena:%worksheet%)
             nil))))

(add-hook 'kill-buffer-query-functions 'ena:notebook-ask-before-kill-buffer)

;; -- `kill-emacs-query-functions'
(defun ena:notebook-ask-before-kill-emacs ()
  "Return `nil' to prevent killing Emacs when unsaved notebook exists.
Called via `kill-emacs-query-functions'."
  (condition-case err
      (let ((unsaved (ena:filter #'ena:notebook-modified-p
                                 (ena:notebook-opened-notebooks))))
        (if (null unsaved)
            t
          (let ((answer
                 (y-or-n-p
                  (format "You have %s unsaved notebook(s). Discard changes?"
                          (length unsaved)))))
            ;; kill all unsaved buffers forcefully
            (when answer
              (mapc #'ena:notebook-close unsaved))
            answer)))
    ((debug error)
     (ena:log 'error "Got error: %S" err)
     (y-or-n-p "Error while examine notebooks.  Kill Emacs anyway? "))))

(add-hook 'kill-emacs-query-functions 'ena:notebook-ask-before-kill-emacs)

;; -- `kill-buffer-hook'
(defun ena:notebook-kill-buffer-callback ()
  "Call notebook destructor.  This function is called via `kill-buffer-hook'."
  (when (ena:$notebook-p ena:%notebook%)
    (ena:notebook-close-worksheet ena:%notebook% ena:%worksheet%)))

(defun ena:notebook-setup-kill-buffer-hook ()
  "Add \"notebook destructor\" to `kill-buffer-hook'."
  (add-hook 'kill-buffer-hook 'ena:notebook-kill-buffer-callback))

;; Useful command to close notebooks.
(defun ena:notebook-kill-all-buffers ()
  "Close all opened notebooks."
  (interactive)
  (let* ((notebooks (ena:notebook-opened-notebooks))
         (unsaved (ena:filter #'ena:notebook-modified-p notebooks)))
    (if notebooks
        (if (y-or-n-p
             (format (concat "You have %s opened notebook(s). "
                             (when unsaved
                               (format "%s are UNSAVED. " (length unsaved)))
                             "Really kill all of them?")
                     (length notebooks)))
            (progn (ena:log 'info "Killing all notebook buffers...")
                   (mapc #'ena:notebook-close notebooks)
                   (ena:log 'info "Killing all notebook buffers... Done!"))
          (ena:log 'info "Canceled to kill all notebooks."))
      (ena:log 'info "No opened notebooks."))))

(provide 'ena-notebook)

;;; ena-notebook.el ends here

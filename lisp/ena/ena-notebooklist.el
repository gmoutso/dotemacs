;;; ena-notebooklist.el --- Notebook list buffer

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-notebooklist.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-notebooklist.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-notebooklist.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'widget)

(require 'ena-core)
(require 'ena-notebook)
(require 'ena-subpackages)

(defcustom ena:notebooklist-first-open-hook nil
  "Hooks to run when the notebook list is opened at first time.

Example to open a notebook named _scratch_ when the notebook list
is opened at first time.::

  (add-hook
   'ena:notebooklist-first-open-hook
   (lambda () (ena:notebooklist-open-notebook-by-name \"_scratch_\")))

"
  :type 'hook
  :group 'ena)

(defstruct ena:$notebooklist
  "Hold notebooklist variables.

`ena:$notebooklist-url-or-port'
  URL or port of IPython server.

`ena:$notebooklist-data'
  JSON data sent from the server."
  url-or-port
  data)

(ena:deflocal ena:%notebooklist% nil
  "Buffer local variable to store an instance of `ena:$notebooklist'.")
(define-obsolete-variable-alias 'ena:notebooklist 'ena:%notebooklist% "0.1.2")

(defvar ena:notebooklist-buffer-name-template "*ena:notebooklist %s*")

(defvar ena:notebooklist-map (make-hash-table :test 'equal)
  "Data store for `ena:notebooklist-list'.
Mapping from URL-OR-PORT to an instance of `ena:$notebooklist'.")

(defun ena:notebooklist-list ()
  "Get a list of opened `ena:$notebooklist'."
  (ena:hash-vals ena:notebooklist-map))

(defun ena:notebooklist-list-add (nblist)
  "Register notebook list instance NBLIST for global lookup.
This function adds NBLIST to `ena:notebooklist-map'."
  (puthash (ena:$notebooklist-url-or-port nblist)
           nblist
           ena:notebooklist-map))

(defun ena:notebooklist-list-get (url-or-port)
  "Get an instance of `ena:$notebooklist' by URL-OR-PORT as a key."
  (gethash url-or-port ena:notebooklist-map))

(defun ena:notebooklist-open-notebook-by-name (name &optional url-or-port
                                                    callback cbargs)
  "Open notebook named NAME in the server URL-OR-PORT.
If URL-OR-PORT is not given or `nil', and the current buffer is
the notebook list buffer, the notebook is searched in the
notebook list of the current buffer.

When used in lisp, CALLBACK and CBARGS are passed to `ena:notebook-open'.
To suppress popup, you can pass a function `ena:do-nothing' as CALLBACK."
  (loop with nblist = (if url-or-port
                          (ena:notebooklist-list-get url-or-port)
                        ena:%notebooklist%)
        for note in (ena:$notebooklist-data nblist)
        for notebook-name = (plist-get note :name)
        for notebook-id = (plist-get note :notebook_id)
        when (equal notebook-name name)
        return (ena:notebook-open (ena:$notebooklist-url-or-port nblist)
                                  notebook-id callback cbargs)))

(defun ena:notebooklist-url (url-or-port)
  (ena:url url-or-port "notebooks"))

(defun ena:notebooklist-new-url (url-or-port)
  (ena:url url-or-port "new"))

(defun ena:notebooklist-get-buffer (url-or-port)
  (get-buffer-create
   (format ena:notebooklist-buffer-name-template url-or-port)))

(defun ena:notebooklist-ask-url-or-port ()
  (let* ((url-or-port-list (mapcar (lambda (x) (format "%s" x))
                                   ena:url-or-port))
         (default (format "%s" (ena:aif (ena:get-notebook)
                                   (ena:$notebook-url-or-port it)
                                 (ena:aif ena:%notebooklist%
                                     (ena:$notebooklist-url-or-port it)
                                   (ena:default-url-or-port)))))
         (url-or-port
          (completing-read (format "URL or port number (default %s): " default)
                           url-or-port-list
                           nil nil nil nil
                           default)))
    (if (string-match "^[0-9]+$" url-or-port)
        (string-to-number url-or-port)
      url-or-port)))

;;;###autoload
(defun ena:notebooklist-open (&optional url-or-port no-popup)
  "Open notebook list buffer."
  (interactive (list (ena:notebooklist-ask-url-or-port)))
  (unless url-or-port (setq url-or-port (ena:default-url-or-port)))
  (ena:subpackages-load)
  (let ((success
         (if no-popup
             #'ena:notebooklist-url-retrieve-callback
           (lambda (&rest args)
             (pop-to-buffer
              (apply #'ena:notebooklist-url-retrieve-callback args))))))
    (ena:query-singleton-ajax
     (list 'notebooklist-open url-or-port)
     (ena:notebooklist-url url-or-port)
     :parser #'ena:json-read
     :error (apply-partially #'ena:notebooklist-open-error url-or-port)
     :success (apply-partially success url-or-port)))
  (ena:notebooklist-get-buffer url-or-port))

(defun* ena:notebooklist-url-retrieve-callback (url-or-port
                                                &key
                                                data
                                                &allow-other-keys)
  "Called via `ena:notebooklist-open'."
  (with-current-buffer (ena:notebooklist-get-buffer url-or-port)
    (let ((already-opened-p (ena:notebooklist-list-get url-or-port))
          (orig-point (point)))
      (setq ena:%notebooklist%
            (make-ena:$notebooklist :url-or-port url-or-port
                                    :data data))
      (ena:notebooklist-list-add ena:%notebooklist%)
      (ena:notebooklist-render)
      (goto-char orig-point)
      (ena:log 'info "Opened notebook list at %s" url-or-port)
      (unless already-opened-p
        (run-hooks 'ena:notebooklist-first-open-hook))
      (current-buffer))))

(defun* ena:notebooklist-open-error (url-or-port
                                     &key symbol-status response
                                     &allow-other-keys)
  (ena:log 'verbose
    "Error thrown: %S" (request-response-error-thrown response))
  (ena:log 'error
    "Error (%s) while opening notebook list at the server %s."
    symbol-status url-or-port))

;;;###autoload
(defun ena:notebooklist-reload ()
  "Reload current Notebook list."
  (interactive)
  (ena:notebooklist-open (ena:$notebooklist-url-or-port ena:%notebooklist%) t))

(defun ena:notebooklist-refresh-related ()
  "Reload notebook list in which current notebook locates.
This function is called via `ena:notebook-after-rename-hook'."
  (ena:notebooklist-open (ena:$notebook-url-or-port ena:%notebook%) t))

(add-hook 'ena:notebook-after-rename-hook 'ena:notebooklist-refresh-related)

(defun ena:notebooklist-open-notebook (nblist notebook-id &optional name
                                              callback cbargs)
  (ena:notebook-open (ena:$notebooklist-url-or-port nblist) notebook-id
                     callback cbargs))

;;;###autoload
(defun ena:notebooklist-new-notebook (&optional url-or-port callback cbargs)
  "Ask server to create a new notebook and open it in a new buffer."
  (interactive (list (ena:notebooklist-ask-url-or-port)))
  (ena:log 'info "Creating a new notebook...")
  (unless url-or-port
    (setq url-or-port (ena:$notebooklist-url-or-port ena:%notebooklist%)))
  (assert url-or-port nil
          (concat "URL-OR-PORT is not given and the current buffer "
                  "is not the notebook list buffer."))
  (ena:query-singleton-ajax
   (list 'notebooklist-new-notebook url-or-port)
   (ena:notebooklist-new-url url-or-port)
   :parser (lambda ()
             (ena:html-get-data-in-body-tag "data-notebook-id"))
   :error (apply-partially #'ena:notebooklist-new-notebook-error
                           url-or-port callback cbargs)
   :success (apply-partially #'ena:notebooklist-new-notebook-callback
                             url-or-port callback cbargs)))

(defun* ena:notebooklist-new-notebook-callback (url-or-port
                                                callback
                                                cbargs
                                                &key
                                                data
                                                &allow-other-keys
                                                &aux
                                                (notebook-id data)
                                                (no-popup t))
  (ena:log 'info "Creating a new notebook... Done.")
  (if notebook-id
      (ena:notebook-open url-or-port notebook-id callback cbargs)
    (ena:log 'info (concat "Oops. EIN failed to open new notebook. "
                           "Please find it in the notebook list."))
    (setq no-popup nil))
  ;; reload or open notebook list
  (ena:notebooklist-open url-or-port no-popup))

(defun* ena:notebooklist-new-notebook-error
    (url-or-port callback cbargs
                 &key response &allow-other-keys
                 &aux
                 (no-popup t)
                 (error (request-response-error-thrown response))
                 (dest (request-response-url response)))
  (ena:log 'verbose
    "NOTEBOOKLIST-NEW-NOTEBOOK-ERROR url-or-port: %S; error: %S; dest: %S"
    url-or-port error dest)
  (ena:log 'error
    "Failed to open new notebook (error: %S). \
You may find the new one in the notebook list." error)
  (setq no-popup nil)
  (ena:notebooklist-open url-or-port no-popup))

;;;###autoload
(defun ena:notebooklist-new-notebook-with-name (name &optional url-or-port)
  "Open new notebook and rename the notebook."
  (interactive (let* ((url-or-port (or (ena:get-url-or-port)
                                       (ena:default-url-or-port)))
                      (name (read-from-minibuffer
                             (format "Notebook name (at %s): " url-or-port))))
                 (list name url-or-port)))
  (ena:notebooklist-new-notebook
   url-or-port
   (lambda (notebook created name)
     (assert created)
     (with-current-buffer (ena:notebook-buffer notebook)
       (ena:notebook-rename-command name)
       ;; As `ena:notebook-open' does not call `pop-to-buffer' when
       ;; callback is specified, `pop-to-buffer' must be called here:
       (pop-to-buffer (current-buffer))))
   (list name)))

(defun ena:notebooklist-delete-notebook-ask (notebook-id name)
  (when (y-or-n-p (format "Delete notebook %s?" name))
    (ena:notebooklist-delete-notebook notebook-id name)))

(defun ena:notebooklist-delete-notebook (notebook-id name)
  (ena:log 'info "Deleting notebook %s..." name)
  (ena:query-singleton-ajax
   (list 'notebooklist-delete-notebook
         (ena:$notebooklist-url-or-port ena:%notebooklist%) notebook-id)
   (ena:notebook-url-from-url-and-id
    (ena:$notebooklist-url-or-port ena:%notebooklist%)
    notebook-id)
   :type "DELETE"
   :success (apply-partially (lambda (buffer name &rest ignore)
                               (ena:log 'info
                                 "Deleting notebook %s... Done." name)
                               (with-current-buffer buffer
                                 (ena:notebooklist-reload)))
                             (current-buffer) name)))

(defun ena:notebooklist-render ()
  "Render notebook list widget.
Notebook list data is passed via the buffer local variable
`ena:notebooklist-data'."
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Create notebook list
  (widget-insert "IPython Notebook list\n\n")
  (widget-create
   'link
   :notify (lambda (&rest ignore) (ena:notebooklist-new-notebook))
   "New Notebook")
  (widget-insert " ")
  (widget-create
   'link
   :notify (lambda (&rest ignore) (ena:notebooklist-reload))
   "Reload List")
  (widget-insert " ")
  (widget-create
   'link
   :notify (lambda (&rest ignore)
             (browse-url
              (ena:url (ena:$notebooklist-url-or-port ena:%notebooklist%))))
   "Open In Browser")
  (widget-insert "\n")
  (loop for note in (ena:$notebooklist-data ena:%notebooklist%)
        for name = (plist-get note :name)
        for notebook-id = (plist-get note :notebook_id)
        do (progn (widget-create
                   'link
                   :notify (lexical-let ((name name)
                                         (notebook-id notebook-id))
                             (lambda (&rest ignore)
                               (ena:notebooklist-open-notebook
                                ena:%notebooklist% notebook-id name)))
                   "Open")
                  (widget-insert " ")
                  (widget-create
                   'link
                   :notify (lexical-let ((name name)
                                         (notebook-id notebook-id))
                             (lambda (&rest ignore)
                               (ena:notebooklist-delete-notebook-ask
                                notebook-id
                                name)))
                   "Delete")
                  (widget-insert " : " name)
                  (widget-insert "\n")))
  (ena:notebooklist-mode)
  (widget-setup))

;;;###autoload
(defun ena:notebooklist-list-notebooks ()
  "Return a list of notebook path (NBPATH).  Each element NBPATH
is a string of the format \"URL-OR-PORT/NOTEBOOK-NAME\"."
  (apply #'append
         (loop for nblist in (ena:notebooklist-list)
               for url-or-port = (ena:$notebooklist-url-or-port nblist)
               collect
               (loop for note in (ena:$notebooklist-data nblist)
                     collect (format "%s/%s"
                                     url-or-port
                                     (plist-get note :name))))))

;;;###autoload
(defun ena:notebooklist-open-notebook-global (nbpath &optional callback cbargs)
  "Choose notebook from all opened notebook list and open it.
Notebook is specified by a string NBPATH whose format is
\"URL-OR-PORT/NOTEBOOK-NAME\".

When used in lisp, CALLBACK and CBARGS are passed to `ena:notebook-open'."
  (interactive
   (list (completing-read
          "Open notebook [URL-OR-PORT/NAME]: "
          (ena:notebooklist-list-notebooks))))
  (let* ((path (split-string nbpath "/"))
         (url-or-port (car path))
         (name (cadr path)))
    (when (and (stringp url-or-port)
               (string-match "^[0-9]+$" url-or-port))
      (setq url-or-port (string-to-number url-or-port)))
    (let ((notebook-id
           (loop for nblist in (ena:notebooklist-list)
                 when (equal (ena:$notebooklist-url-or-port nblist) url-or-port)
                 if (loop for note in (ena:$notebooklist-data nblist)
                          when (equal (plist-get note :name) name)
                          return (plist-get note :notebook_id))
                 return it)))
      (if notebook-id
          (ena:notebook-open url-or-port notebook-id callback cbargs)
        (ena:log 'info "Notebook '%s' not found" nbpath)))))

;;;###autoload
(defun ena:notebooklist-load (&optional url-or-port)
  "Load notebook list but do not pop-up the notebook list buffer.

For example, if you want to load notebook list when Emacs starts,
add this in the Emacs initialization file::

  (add-to-hook 'after-init-hook 'ena:notebooklist-load)

or even this (if you want fast Emacs start-up)::

  ;; load notebook list if Emacs is idle for 3 sec after start-up
  (run-with-idle-timer 3 nil #'ena:notebooklist-load)

You should setup `ena:url-or-port' or `ena:default-url-or-port'
in order to make this code work.

See also:
`ena:connect-to-default-notebook', `ena:connect-default-notebook'."
  (ena:notebooklist-open url-or-port t))


(defun ena:notebooklist-find-server-by-notebook-name (name)
  "Find a notebook named NAME and return a list (URL-OR-PORT NOTEBOOK-ID)."
  (loop named outer
        for nblist in (ena:notebooklist-list)
        for url-or-port = (ena:$notebooklist-url-or-port nblist)
        do (loop for note in (ena:$notebooklist-data nblist)
                 when (equal (plist-get note :name) name)
                 do (return-from outer
                      (list url-or-port (plist-get note :notebook_id))))))

(defun ena:notebooklist-open-notebook-by-file-name
  (&optional filename noerror buffer-callback)
  "Find the notebook named as same as the current file in the servers.
Open the notebook if found.  Note that this command will *not*
upload the current file to the server.

.. When FILENAME is unspecified the variable `buffer-file-name'
   is used instead.  Set NOERROR to non-`nil' to suppress errors.
   BUFFER-CALLBACK is called after opening notebook with the
   current buffer as the only one argument."
  (interactive (progn (assert buffer-file-name nil "Not visiting a file.")
                      nil))
  (unless filename (setq filename buffer-file-name))
  (assert filename nil "No file found.")
  (let* ((name (file-name-sans-extension
                (file-name-nondirectory (or filename))))
         (found (ena:notebooklist-find-server-by-notebook-name name))
         (callback (lambda (-ignore-1- -ignore-2- buffer buffer-callback)
                     (ena:notebook-pop-to-current-buffer) ; default
                     (when (buffer-live-p buffer)
                       (funcall buffer-callback buffer))))
         (cbargs (list (current-buffer) (or buffer-callback #'ignore))))
    (unless noerror
      (assert found nil "No server has notebook named: %s" name))
    (destructuring-bind (url-or-port notebook-id) found
      (ena:notebook-open url-or-port notebook-id callback cbargs))))

(defvar ena:notebooklist-find-file-buffer-callback #'ignore)

(defun ena:notebooklist-find-file-callback ()
  "A callback function for `find-file-hook' to open notebook.

FIMXE: document how to use `ena:notebooklist-find-file-callback'
       when I am convinced with the API."
  (ena:and-let* ((filename buffer-file-name)
                 ((string-match-p "\\.ipynb$" filename)))
    (ena:notebooklist-open-notebook-by-file-name
     filename t ena:notebooklist-find-file-buffer-callback)))


;;; Login

(defun ena:notebooklist-login (url-or-port password)
  "Login to IPython notebook server."
  (interactive (list (ena:notebooklist-ask-url-or-port)
                     (read-passwd "Password: ")))
  (ena:log 'debug "NOTEBOOKLIST-LOGIN: %s" url-or-port)
  (ena:query-singleton-ajax
   (list 'notebooklist-login url-or-port)
   (ena:url url-or-port "login")
   :type "POST"
   :data (concat "password=" (url-hexify-string password))
   :parser #'ena:notebooklist-login--parser
   :error (apply-partially #'ena:notebooklist-login--error url-or-port)
   :success (apply-partially #'ena:notebooklist-login--success url-or-port)))

(defun ena:notebooklist-login--parser ()
  (goto-char (point-min))
  (list :bad-page (re-search-forward "<input type=.?password" nil t)))

(defun ena:notebooklist-login--success-1 (url-or-port)
  (ena:log 'info "Login to %s complete. \
Now you can open notebook list by `ena:notebooklist-open'." url-or-port))

(defun ena:notebooklist-login--error-1 (url-or-port)
  (ena:log 'info "Failed to login to %s" url-or-port))

(defun* ena:notebooklist-login--success (url-or-port &key
                                                     data
                                                     &allow-other-keys)
  (if (plist-get data :bad-page)
      (ena:notebooklist-login--error-1 url-or-port)
    (ena:notebooklist-login--success-1 url-or-port)))

(defun* ena:notebooklist-login--error
    (url-or-port &key
                 data
                 symbol-status
                 response
                 &allow-other-keys
                 &aux
                 (response-status (request-response-status-code response)))
  (if (or
       ;; workaround for url-retrieve backend
       (and (eq symbol-status 'timeout)
            (equal response-status 302)
            (request-response-header response "set-cookie"))
       ;; workaround for curl backend
       (and (equal response-status 405)
            (ena:aand (car (request-response-history response))
                      (request-response-header it "set-cookie"))))
      (ena:notebooklist-login--success-1 url-or-port)
    (ena:notebooklist-login--error-1 url-or-port)))


;;; Generic getter

(defun ena:get-url-or-port--notebooklist ()
  (when (ena:$notebooklist-p ena:%notebooklist%)
    (ena:$notebooklist-url-or-port ena:%notebooklist%)))


;;; Notebook list mode

(define-derived-mode ena:notebooklist-mode fundamental-mode "ena:notebooklist"
  "IPython notebook list mode.")

(defun ena:notebooklist-prev-item () (interactive) (move-beginning-of-line 0))
(defun ena:notebooklist-next-item () (interactive) (move-beginning-of-line 2))

(setq ena:notebooklist-mode-map (copy-keymap widget-keymap))

(let ((map ena:notebooklist-mode-map))
  (define-key map "\C-c\C-r" 'ena:notebooklist-reload)
  (define-key map "g" 'ena:notebooklist-reload)
  (define-key map "p" 'ena:notebooklist-prev-item)
  (define-key map "n" 'ena:notebooklist-next-item)
  (define-key map "q" 'bury-buffer)
  (easy-menu-define ena:notebooklist-menu map "EIN Notebook List Mode Menu"
    `("EIN Notebook List"
      ,@(ena:generate-menu
         '(("Reload" ena:notebooklist-reload)
           ("New Notebook" ena:notebooklist-new-notebook)
           ("New Notebook (with name)"
            ena:notebooklist-new-notebook-with-name)
           ("New Junk Notebook" ena:junk-new))))))

(provide 'ena-notebooklist)

;;; ena-notebooklist.el ends here

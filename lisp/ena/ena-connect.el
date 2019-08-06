;;; ena-connect.el --- Connect external buffers to IPython

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-connect.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-connect.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-connect.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: There is a problem when connected notebook is closed.
;;        This can be fixed in some ways:
;; * Turn off ena:connect when the command that uses kernel is invoked
;;   but corresponding notebook was closed already.
;; * Connect directly to ena:kernel and make its destructor to care
;;   about connecting buffers.

;;; Code:

(require 'eieio)
(eval-when-compile (require 'auto-complete nil t))

(require 'ena-notebook)

(declare-function ena:notebooklist-list-notebooks "ena-notebooklist")
(declare-function ena:notebooklist-open-notebook-global "ena-notebooklist")


;;; Utils

(defun ena:maybe-save-buffer (option)
  "Conditionally save current buffer.
Return `t' if the buffer is unmodified or `nil' otherwise.
If the buffer is modified, buffer is saved depending on the value
of OPTION:
  ask  : Ask whether the buffer should be saved.
  yes  : Save buffer always.
  no   : Do not save buffer."
  (if (not (buffer-modified-p))
      t
    (case option
      (ask (when (y-or-n-p "Save buffer? ")
             (save-buffer)
             t))
      (yes (save-buffer)
           t)
      (t nil))))


;;; Configuration

(defcustom ena:connect-run-command "%run"
  "``%run`` magic command used for `ena:connect-run-buffer'.
Types same as `ena:console-security-dir' are valid."
  :type '(choice
          (string :tag "command" "%run")
          (alist :tag "command mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "command" "%run"))
          (function :tag "command getter"
                    (lambda (url-or-port) (format "%%run -n -i -t -d"))))
  :group 'ena)

(defcustom ena:connect-reload-command "%run -n"
  "Setting for `ena:connect-reload-buffer'.
Same as `ena:connect-run-command'."
  :type '(choice
          (string :tag "command" "%run")
          (alist :tag "command mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "command" "%run"))
          (function :tag "command getter"
                    (lambda (url-or-port) (format "%%run -n -i -t -d"))))
  :group 'ena)

(defun ena:connect-run-command-get ()
  (ena:choose-setting 'ena:connect-run-command
                      (ena:$notebook-url-or-port (ena:connect-get-notebook))))

(defcustom ena:connect-save-before-run 'yes
  "Whether the buffer should be saved before `ena:connect-run-buffer'."
  :type '(choice (const :tag "Always save buffer" yes)
                 (const :tag "Always do not save buffer" no)
                 (const :tag "Ask" ask))
  :group 'ena)

(defcustom ena:connect-aotoexec-lighter nil
  "String appended to the lighter of `ena:connect-mode' (`ena:c')
when auto-execution mode is on.  When `nil', use the same string
as `ena:cell-autoexec-prompt'."
  :type '(choice (string :tag "String appended to ena:c" "@")
                 (const :tag "Use `ena:cell-autoexec-prompt'." nil))
  :group 'ena)

(defcustom ena:connect-default-notebook nil
  "Notebook to be connect when `ena:connect-to-default-notebook' is called.

Example setting to connect to \"My_Notebook\" in the server at
port 8888 when opening any buffer in `python-mode'::

  (setq ena:connect-default-notebook \"8888/My_Notebook\")
  (add-hook 'python-mode-hook 'ena:connect-to-default-notebook)

`ena:connect-default-notebook' can also be a function without any
argument.  This function must return a string (notebook path of
the form \"URL-OR-PORT/NOTEBOOK-NAME\").

As `ena:connect-to-default-notebook' requires notebook list to be
loaded, consider using `ena:notebooklist-load' to load notebook
list if you want to connect to notebook without manually opening
notebook list."
  :type '(choice (string :tag "URL-OR-PORT/NOTEBOOK-NAME")
                 (function :tag "Notebook path getter"))
  :group 'ena)


;;; Class

(ena:deflocal ena:%connect% nil
  "Buffer local variable to store an instance of `ena:connect'")
(define-obsolete-variable-alias 'ena:@connect 'ena:%connect% "0.1.2")

(defclass ena:connect ()
  ((notebook :initarg :notebook :type ena:$notebook)
   (buffer :initarg :buffer :type buffer)
   (autoexec :initarg :autoexec :initform nil :type boolean
             :document "Auto-execution mode flag.

See also the document of the `autoexec' slot of `ena:codecell'
class.")))

(defun ena:connect-setup (notebook buffer)
  (with-current-buffer buffer
    (setq ena:%connect%
          (ena:connect "Connect" :notebook notebook :buffer buffer))
    ena:%connect%))


;;; Methods

;; FIXME: Clarify names of these `connect-to-*' functions:

;;;###autoload
(defun ena:connect-to-notebook-command (&optional not-yet-opened)
  "Connect to notebook.  When the prefix argument is given,
you can choose any notebook on your server including the ones
not yet opened.  Otherwise, already chose from already opened
notebooks."
  (interactive "P")
  (call-interactively (if not-yet-opened
                          #'ena:connect-to-notebook
                        #'ena:connect-to-notebook-buffer)))

;;;###autoload
(defun ena:connect-to-notebook (nbpath &optional buffer no-reconnection)
  "Connect any buffer to notebook and its kernel."
  (interactive
   (list
    (completing-read
     "Notebook to connect [URL-OR-PORT/NAME]: "
     (ena:notebooklist-list-notebooks))))
  (ena:notebooklist-open-notebook-global
   nbpath
   (lambda (notebook -ignore- buffer no-reconnection)
     (ena:connect-buffer-to-notebook notebook buffer no-reconnection))
   (list (or buffer (current-buffer)) no-reconnection)))

;;;###autoload
(defun ena:connect-to-notebook-buffer (buffer-or-name)
  "Connect any buffer to opened notebook and its kernel."
  (interactive (list (completing-read "Notebook buffer to connect: "
                                      (ena:notebook-opened-buffer-names))))
  (let ((notebook
         (buffer-local-value 'ena:%notebook% (get-buffer buffer-or-name))))
    (ena:connect-buffer-to-notebook notebook)))

;;;###autoload
(defun ena:connect-buffer-to-notebook (notebook &optional buffer
                                                no-reconnection)
  "Connect BUFFER to NOTEBOOK."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (if (or (not no-reconnection)
            (not ena:%connect%))
        (let ((connection (ena:connect-setup notebook buffer)))
          (when (ena:eval-if-bound 'ac-sources)
            (push 'ac-source-ena-async ac-sources))
          (ena:connect-mode)
          (ena:log 'info "Connected to %s"
                   (ena:$notebook-notebook-name notebook))
          connection)
      (ena:log 'info "Buffer is already connected to notebook."))))

(defun ena:connect-get-notebook ()
  (oref ena:%connect% :notebook))

(defun ena:connect-get-kernel ()
  (ena:$notebook-kernel (ena:connect-get-notebook)))

(defun ena:connect-eval-buffer ()
  "Evaluate the whole buffer.  Note that this will run the code
inside the ``if __name__ == \"__main__\":`` block."
  (interactive)
  (ena:shared-output-eval-string (buffer-string) nil nil nil :silent t)
  (ena:connect-execute-autoexec-cells)
  (ena:log 'info "Whole buffer is sent to the kernel."))

(defun ena:connect-run-buffer (&optional ask-command)
  "Run buffer using ``%run``.  Ask for command if the prefix ``C-u`` is given.
Variable `ena:connect-run-command' sets the default command."
  (interactive "P")
  (ena:aif (ena:aand (ena:get-url-or-port)
                     (ena:filename-to-python it (buffer-file-name)))
      (let* ((default-command (ena:connect-run-command-get))
             (command (if ask-command
                          (read-from-minibuffer "Command: " default-command)
                        default-command))
             (cmd (format "%s %s" command it)))
        (if (ena:maybe-save-buffer ena:connect-save-before-run)
            (progn
              (ena:shared-output-eval-string cmd nil nil nil :silent t)
              (ena:connect-execute-autoexec-cells)
              (ena:log 'info "Command sent to the kernel: %s" cmd))
          (ena:log 'info "Buffer must be saved before %%run.")))
    (error (concat "This buffer has no associated file.  "
                   "Use `ena:connect-eval-buffer' instead."))))

(defun ena:connect-run-or-eval-buffer (&optional eval)
  "Run buffer using the ``%run`` magic command or eval whole
buffer if the prefix ``C-u`` is given.
Variable `ena:connect-run-command' sets the command to run.
You can change the command and/or set the options.
See also: `ena:connect-run-buffer', `ena:connect-eval-buffer'."
  (interactive "P")
  (if eval
      (ena:connect-eval-buffer)
    (ena:connect-run-buffer)))

(defun ena:connect-reload-buffer ()
  "Reload buffer using the command set by `ena:connect-reload-command'."
  (interactive)
  (let ((ena:connect-run-command ena:connect-reload-command))
    (call-interactively #'ena:connect-run-buffer)))

(defun ena:connect-eval-region (start end)
  (interactive "r")
  (ena:shared-output-eval-string (buffer-substring start end))
  (ena:log 'info "Selected region is sent to the kernel."))

(define-obsolete-function-alias
  'ena:connect-eval-string-internal
  'ena:shared-output-eval-string "0.1.2")

(define-obsolete-function-alias
  'ena:connect-request-tool-tip-or-help-command
  'ena:pytools-request-tooltip-or-help "0.1.2")

(defun ena:connect-pop-to-notebook ()
  (interactive)
  (ena:connect-assert-connected)
  (pop-to-buffer (ena:notebook-buffer (ena:connect-get-notebook))))


;;; Generic getter

(defun ena:get-url-or-port--connect ()
  (ena:aand (ena:get-notebook--connect) (ena:$notebook-url-or-port it)))

(defun ena:get-notebook--connect ()
  (when (ena:connect-p ena:%connect%)
    (oref ena:%connect% :notebook)))

(defun ena:get-kernel--connect ()
  (ena:aand (ena:get-notebook--connect) (ena:$notebook-kernel it)))

(defun ena:get-traceback-data--connect ()
  ;; FIXME: Check if the TB in shared-output buffer is originated from
  ;;        the current buffer.
  (ena:aand (ena:shared-output-get-cell) (ena:cell-get-tb-data it)))
(autoload 'ena:shared-output-get-cell "ena-shared-output") ; FIXME: Remove!


;;; Auto-execution

(defun ena:connect-assert-connected ()
  (assert (ena:connect-p ena:%connect%) nil
          "Current buffer (%s) is not connected to IPython notebook."
          (buffer-name))
  (assert (ena:notebook-live-p (oref ena:%connect% :notebook)) nil
          "Connected notebook is not live (probably already closed)."))

(defun ena:connect-execute-autoexec-cells ()
  "Call `ena:notebook-execute-autoexec-cells' via `after-save-hook'."
  (ena:connect-assert-connected)
  (when (oref ena:%connect% :autoexec)
    (ena:notebook-execute-autoexec-cells (ena:connect-get-notebook))))

(defun ena:connect-toggle-autoexec ()
  "Toggle auto-execution mode of the current connected buffer.

When auto-execution mode is on, cells in connected notebook will
be automatically executed whenever run, eval or reload command [#]_
is called in this buffer.

.. [#] Namely, one of

   * `ena:connect-run-buffer'
   * `ena:connect-eval-buffer'
   * `ena:connect-run-or-eval-buffer'
   * `ena:connect-reload-buffer'

Note that you need to set cells to run in the connecting buffer
or no cell will be executed.
Use the `ena:worksheet-turn-on-autoexec' command in notebook to
change the cells to run."
  (interactive)
  (ena:connect-assert-connected)
  (let ((autoexec-p (not (oref ena:%connect% :autoexec))))
    (oset ena:%connect% :autoexec autoexec-p)
    (ena:log 'info "Auto-execution mode is %s."
             (if autoexec-p "enabled" "disabled"))))


;;; Auto-connect

;;;###autoload
(defun ena:connect-to-default-notebook ()
  "Connect to the default notebook specified by
`ena:connect-default-notebook'.  Set this to `python-mode-hook'
to automatically connect any python-mode buffer to the
notebook."
  (ena:log 'verbose "CONNECT-TO-DEFAULT-NOTEBOOK")
  (ena:and-let* ((nbpath ena:connect-default-notebook)
                 ((not (ena:worksheet-buffer-p))))
    (when (functionp nbpath)
      (setq nbpath (funcall nbpath)))
    (ena:connect-to-notebook nbpath nil t)))



;;; ena:connect-mode

(defvar ena:connect-mode-map (make-sparse-keymap))

(let ((map ena:connect-mode-map))
  (define-key map "\C-c\C-c" 'ena:connect-run-or-eval-buffer)
  (define-key map "\C-c\C-l" 'ena:connect-reload-buffer)
  (define-key map "\C-c\C-r" 'ena:connect-eval-region)
  (define-key map (kbd "C-:") 'ena:shared-output-eval-string)
  (define-key map "\C-c\C-f" 'ena:pytools-request-tooltip-or-help)
  (define-key map "\C-c\C-i" 'ena:completer-complete)
  (define-key map "\C-c\C-z" 'ena:connect-pop-to-notebook)
  (define-key map "\C-c\C-a" 'ena:connect-toggle-autoexec)
  (define-key map "\C-c\C-o" 'ena:console-open)
  (define-key map "\C-c\C-x" 'ena:tb-show)
  (define-key map "\M-."          'ena:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ena:pytools-jump-to-source-command)
  (define-key map "\M-,"          'ena:pytools-jump-back-command)
  (define-key map (kbd "C-c C-,") 'ena:pytools-jump-back-command)
  (define-key map (kbd "C-c C-/") 'ena:notebook-scratchsheet-open)
  map)

(defun ena:connect-mode-get-lighter ()
  (if (oref ena:%connect% :autoexec)
      (format " ena:c%s" (or ena:connect-aotoexec-lighter
                             ena:cell-autoexec-prompt))
    " ena:c"))

(define-minor-mode ena:connect-mode
  "Minor mode for communicating with IPython notebook.

\\{ena:connect-mode-map}"
  :lighter (:eval (ena:connect-mode-get-lighter))
  :keymap ena:connect-mode-map
  :group 'ena
  (ena:complete-on-dot-install ena:connect-mode-map))

(put 'ena:connect-mode 'permanent-local t)


(provide 'ena-connect)

;;; ena-connect.el ends here

;;; ena-dev.el --- Development tools

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-dev.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-dev.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-dev.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(declare-function rst-shift-region "rst")

(require 'ena-notebook)
(require 'ena-subpackages)

;;;###autoload
(defun ena:dev-insert-mode-map (map-string)
  "Insert mode-map into rst document.  For README.rst."
  (save-excursion
    (insert "\n\n::\n\n")
    (let ((beg (point)))
      (search-forward ".. // KEYS END //")
      (move-beginning-of-line nil)
      (delete-region beg (point))
      (insert "\n")
      (goto-char beg)
      (insert (substitute-command-keys map-string))
      (rst-shift-region beg (point) 1))))

(defun ena:load-files (&optional regex dir ignore-compiled)
  (let* ((dir (or dir ena:source-dir))
         (regex (or regex ".+"))
         (files (and
                 (file-accessible-directory-p dir)
                 (directory-files dir 'full regex))))
    (unless ignore-compiled
      (setq files (mapcar #'file-name-sans-extension files)))
    (mapc #'load files)))

(defun ena:dev-reload ()
  "Reload ena-*.el modules."
  (interactive)
  (ena:notebook-kill-all-buffers)
  (makunbound 'ena:notebook-mode-map)   ; so defvar works.
  (load "ena-notebook")  ; ... but make sure it will be defined first.
  (ena:load-files "^ena-.*\\.el$")
  (ena:subpackages-reload))

(defun* ena:dev-require-all (&key (ignore-p #'ignore))
  (loop for f in (directory-files ena:source-dir nil "^ena-.*\\.el$")
        unless (or (equal f "ena-pkg.el")
                   (funcall ignore-p f))
        do (require (intern (file-name-sans-extension f)) nil t))
  ;; For `widget-button-press':
  (require 'wid-edit nil t))

(defadvice backtrace (around ena:dev-short-backtrace)
  "A hack to shorten backtrace.

As code cells hold base64-encoded image data, backtrace tends to
be VERY long.  So I am setting `print-level' to *1*.  Note that
setting it globally via `setq' does not work because the value
for debugger is hard-coded.  See `debugger-setup-buffer'."
  (let ((print-level 1))
    ad-do-it))

(defun ena:dev-patch-backtrace ()
  "Monkey patch `backtrace' function to make it shorter."
  (interactive)
  (ad-enable-advice 'backtrace 'around 'ena:dev-short-backtrace)
  (ad-activate 'backtrace))

(defun ena:dev-depatch-backtrace ()
  "Undo `ena:dev-patch-backtrace'."
  (interactive)
  (ad-deactivate 'backtrace)
  (ad-disable-advice 'backtrace 'around 'ena:dev-short-backtrace)
  ;; In case it has other advices.
  (ad-activate 'backtrace))

(defun ena:dev-show-debug-setting ()
  "Show variables related to EIN debugging."
  (interactive)
  (message (concat "debug-on-error=%s websocket-debug=%s "
                   "websocket-callback-debug-on-error=%s "
                   "ena:debug=%s ena:log-level=%s ena:log-message-level=%s")
           debug-on-error websocket-debug websocket-callback-debug-on-error
           ena:debug
           (ena:log-level-int-to-name ena:log-level)
           (ena:log-level-int-to-name ena:log-message-level)))

;;;###autoload
(defun ena:dev-start-debug (&optional ws-callback)
  "Enable EIN debugging support.
When the prefix argument is given, debugging support for websocket
callback (`websocket-callback-debug-on-error') is enabled."
  (interactive "P")
  (setq debug-on-error t)
  (setq websocket-debug t)
  (when ws-callback
    (setq websocket-callback-debug-on-error t))
  (setq ena:debug t)
  (ena:log-set-level 'debug)
  (ena:log-set-message-level 'verbose)
  (ena:dev-patch-backtrace)
  (ena:dev-show-debug-setting))

;;;###autoload
(defun ena:dev-stop-debug ()
  "Disable debugging support enabled by `ena:dev-start-debug'."
  (interactive)
  (setq debug-on-error nil)
  (setq websocket-debug nil)
  (setq websocket-callback-debug-on-error nil)
  (setq ena:debug nil)
  (ena:log-set-level 'verbose)
  (ena:log-set-message-level 'info)
  (ena:dev-depatch-backtrace)
  (ena:dev-show-debug-setting))

(defun ena:dev-pop-to-debug-shell ()
  "Open shell channel websocket log buffer."
  (interactive)
  (pop-to-buffer
   (websocket-get-debug-buffer-create
    (ena:$websocket-ws (ena:$kernel-shell-channel
                        (ena:$notebook-kernel ena:%notebook%))))))

(defun ena:dev-pop-to-debug-iopub ()
  "Open iopub channel websocket log buffer."
  (interactive)
  (pop-to-buffer
   (websocket-get-debug-buffer-create
    (ena:$websocket-ws (ena:$kernel-iopub-channel
                        (ena:$notebook-kernel ena:%notebook%))))))

(defun ena:dev-notebook-plain-mode ()
  "Use `ena:notebook-plain-mode'."
  (interactive)
  (setq ena:notebook-modes '(ena:notebook-plain-mode)))

(defun ena:dev-notebook-python-mode ()
  "Use `ena:notebook-python-mode'."
  (interactive)
  (setq ena:notebook-modes '(ena:notebook-python-mode)))

(defun ena:dev-notebook-mumamo-mode ()
  "Use `ena:notebook-mumamo-mode'."
  (interactive)
  (setq ena:notebook-modes '(ena:notebook-mumamo-mode)))

(defun ena:dev-notebook-multilang-mode ()
  "Use `ena:notebook-multilang-mode'."
  (interactive)
  (setq ena:notebook-modes '(ena:notebook-multilang-mode)))

(defun ena:dev-sys-info--lib (name)
  (let* ((libsym (intern-soft name))
         (version-var (loop for fmt in '("%s-version" "%s:version")
                            if (intern-soft (format fmt name))
                            return it))
         (version (symbol-value version-var)))
    (list :name name
          :path (ena:aand (locate-library name) (abbreviate-file-name it))
          :featurep (featurep libsym)
          :version-var version-var
          :version version)))

(defun ena:dev-dump-vars (names)
  (loop for var in names
        collect (intern (format ":%s" var))
        collect (symbol-value (intern (format "ena:%s" var)))))

(defun ena:dev-stdout-program (command args)
  "Safely call COMMAND with ARGS and return its stdout."
  (ena:aand (executable-find command)
            (with-temp-buffer
              (erase-buffer)
              (apply #'call-process it nil t nil args)
              (buffer-string))))

(defun ena:dev-sys-info ()
  (list
   "EIN system info"
   :emacs-version (emacs-version)
   :emacs-bzr-version (ena:eval-if-bound 'emacs-bzr-version)
   :window-system window-system
   ;; Emacs variant detection
   ;; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
   :emacs-variant
   (cond ((featurep 'meadow) 'meadow)
         ((featurep 'carbon-emacs-package) 'carbon))
   :os (list
        :uname (ena:dev-stdout-program "uname" '("-a"))
        :lsb-release (ena:dev-stdout-program "lsb_release" '("-a")))
   :image-types (ena:eval-if-bound 'image-types)
   :image-types-available (ena:filter #'image-type-available-p
                                      (ena:eval-if-bound 'image-types))
   :request (list :backend request-backend)
   :ena (append (list :version (ena:version))
                (ena:dev-dump-vars '("source-dir")))
   :lib (ena:filter (lambda (info) (plist-get info :path))
                    (mapcar #'ena:dev-sys-info--lib
                            '("websocket" "request" "auto-complete" "mumamo"
                              "auto-complete" "popup" "fuzzy" "pos-tip"
                              "python" "python-mode" "markdown-mode"
                              "smartrep" "anything" "helm")))))

(defun ena:dev-show-sys-info (&optional show-in-buffer)
  "Show Emacs and library information."
  (interactive (list t))
  (let ((info (ena:dev-sys-info)))
    (if show-in-buffer
        (let ((buffer (get-buffer-create "*ena:sys-info*")))
          (with-current-buffer buffer
            (erase-buffer)
            (pp info buffer)
            (pop-to-buffer buffer)))
      (message "EIN INFO:\n%s" (pp-to-string info)))))

;;;###autoload
(defun ena:dev-bug-report-template ()
  "Open a buffer with bug report template."
  (interactive)
  (let ((buffer (generate-new-buffer "*ena:bug-report*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "<!--
This template is to help you write a good bug report.
You may skip some sections, but please make sure to include
the last section \"System info\", unless you find some
personal information there.

After finish writing it, please post it here:
https://github.com/tkf/emacs-ipython-notebook/issues/new
-->

## Check list

- [ ] I read \"Avoid standard Emacs traps\" section in
  https://github.com/tkf/emacs-ipython-notebook/blob/master/CONTRIBUTING.md
- [ ] I checked that IPython works with the browser interface.
- [ ] I checked that the problem was not due to badly compiled
  files.  Removing `*.elc` files from source directory of EIN and
  its dependencies did not solve the problem.
- [ ] I checked that the libraries are loaded from intended place.
  (You can check the location in the \"System info\" section below)

## Description of the problem you have


## Steps to reproduce the problem

1.
2.
3.

## Expected output


## Your EIN configuration (in .emacs.d/init.el or somewhere else)


## Your IPython configuration

1. What is your IPython version? (run `ipython --version`):

2. How do you start IPython? (e.g., `ipython notebook --port 9999`):

3. What is your IPython notebook port number or URL?:


## Additional information (if any)


")
      (insert "## System info:\n\n```cl\n")
      (condition-case err
          (ena:dev-print-sys-info buffer)
        (error (insert (format "`ena:dev-sys-info' produce: %S" err))))
      (insert "```\n")
      (goto-char (point-min))
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (pop-to-buffer buffer))))

(defun ena:dev-print-sys-info (&optional stream)
  (princ (ena:dev--pp-to-string (ena:dev-sys-info))
         (or stream standard-output)))

(defun ena:dev--pp-to-string (object)
  "`pp-to-string' with additional prettifier."
  (with-temp-buffer
    (erase-buffer)
    (let ((pp-escape-newlines nil))
      (pp object (current-buffer)))
    (goto-char (point-min))
    (let ((emacs-lisp-mode-hook nil))
      (emacs-lisp-mode))
    (ena:dev--prettify-sexp)
    (buffer-string)))

(defun ena:dev--prettify-sexp ()
  "Prettify s-exp at point recursively.
Use this function in addition to `pp' (see `ena:dev--pp-to-string')."
  (down-list)
  (condition-case nil
      (while t
        (forward-sexp)
        ;; Prettify nested s-exp.
        (when (looking-back ")")
          (save-excursion
            (backward-sexp)
            (ena:dev--prettify-sexp)))
        ;; Add newline before keyword symbol.
        (when (looking-at-p " :")
          (newline-and-indent))
        ;; Add newline before long string literal.
        (when (and (looking-at-p " \"")
                   (let ((end (save-excursion
                                (forward-sexp)
                                (point))))
                     (> (- end (point)) 80)))
          (newline-and-indent)))
    (scan-error)))

(provide 'ena-dev)

;;; ena-dev.el ends here

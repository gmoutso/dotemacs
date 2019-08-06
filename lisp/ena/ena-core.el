;;; ena-core.el --- EIN core

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-core.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-core.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-core.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))

;; Optional dependency on tramp:
(declare-function tramp-make-tramp-file-name "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-dissect-file-name "tramp")


(require 'ena)  ; get autoloaded functions into namespace
(require 'ena-utils)


(defgroup ena nil
  "IPython notebook client in Emacs"
  :group 'applications
  :prefix "ena:")

(defvar ena:version "0.2.1alpha2"
  "Version number for Emacs IPython Notebook (EIN).")


;;; Configuration

(defcustom ena:url-or-port '(8888)
  "List of default url-or-port values.
This will be used for completion. So put your IPython servers.
You can connect to servers not in this list \(but you will need
to type every time)."
  :type '(repeat (choice (integer :tag "Port number" 8888)
                         (string :tag "URL" "http://127.0.0.1:8888")))
  :group 'ena)

(defcustom ena:default-url-or-port nil
  "Default URL or port.  This should be your main IPython
Notebook server."
  :type '(choice (integer :tag "Port number" 8888)
                 (string :tag "URL" "http://127.0.0.1:8888")
                 (const :tag "First value of `ena:url-or-port'" nil))
  :group 'ena)

(defcustom ena:filename-translations nil
  "Convert file paths between Emacs and Python process.

This value can take these form:

alist
    Its key specifies URL-OR-PORT and value must be a list of two
    functions: (TO-PYTHON FROM-PYTHON).  Key (URL-OR-PORT) can be
    string (URL), integer (port), or `default' (symbol).  The
    value of `default' is used when other key does not much.
function
    Called with an argument URL-OR-PORT (integer or string).
    This function must return a list of two functions:
    (TO-PYTHON FROM-PYTHON).

Here, the functions TO-PYTHON and FROM-PYTHON are defined as:

TO-PYTHON
    A function which converts a file name (returned by
    `buffer-file-name') to the one Python understands.
FROM-PYTHON
    A function which converts a file path returned by
    Python process to the one Emacs understands.

Use `ena:tramp-create-filename-translator' to easily generate the
pair of TO-PYTHON and FROM-PYTHON."
  ;; I've got the idea from `slime-filename-translations'.
  :type '(choice
          (alist :tag "Translations mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (list (function :tag "TO-PYTHON")
                                   (function :tag "FROM-PYTHON")))
          (function :tag "Translations getter"))
  :group 'ena)



;;; Constants

(defvar ena:source-dir (file-name-directory load-file-name)
  "Directory in which ``ena*.el`` locate.")



;;; Configuration getter

(defun ena:default-url-or-port ()
  (or ena:default-url-or-port (car ena:url-or-port) 8888))

(defun ena:version ()
  "Return a string containing `ena:version' and git revision if
the source is in git repository."
  (ena:aif (when (ena:git-root-p
                  (concat (file-name-as-directory ena:source-dir) ".."))
             (let ((default-directory ena:source-dir))
               (ena:git-revision-dirty)))
      (concat ena:version "." it)
    ena:version))



;;; File name translation (tramp support)

;; Probably it's better to define `ena:filename-translations-get' as
;; an EIEIO method so that I don't have to re-define functions such as
;; `ena:kernel-filename-to-python' and `ena:kernel-filename-from-python'.

(defun ena:filename-translations-get (url-or-port)
  (ena:choose-setting 'ena:filename-translations url-or-port))

(defun ena:filename-to-python (url-or-port filename)
  (ena:aif (car (ena:filename-translations-get url-or-port))
      (funcall it filename)
    filename))

(defun ena:filename-from-python (url-or-port filename)
  (ena:aif (cadr (ena:filename-translations-get url-or-port))
      (funcall it filename)
    filename))

(defun ena:make-tramp-file-name (username remote-host python-filename)
  "Old (with multi-hops) tramp compatibility function.
Adapted from `slime-make-tramp-file-name'."
  (if (boundp 'tramp-multi-methods)
      (tramp-make-tramp-file-name nil nil
                                  username
                                  remote-host
                                  python-filename)
    (tramp-make-tramp-file-name nil
                                username
                                remote-host
                                python-filename)))

(defun ena:tramp-create-filename-translator (remote-host &optional username)
  "Generate a pair of TO-PYTHON and FROM-PYTHON for
`ena:filename-translations'.

Usage::

    (setq ena:filename-translations
          `((8888
             . ,(ena:tramp-create-filename-translator \"MY-HOSTNAME\"))))
    ;; Equivalently:
    (setq ena:filename-translations
          (lambda (url-or-port)
            (when (equal url-or-port 8888)
              (ena:tramp-create-filename-translator \"MY-HOSTNAME\"))))

This setting assumes that the IPython server which can be
connected using the port 8888 in localhost is actually running in
the host named MY-HOSTNAME.

Adapted from `slime-create-filename-translator'."
  (require 'tramp)
  (lexical-let ((remote-host remote-host)
                (username (or username (user-login-name))))
    (list (lambda (emacs-filename)
            (tramp-file-name-localname
             (tramp-dissect-file-name emacs-filename)))
          (lambda (python-filename)
             (ena:make-tramp-file-name username remote-host python-filename)))))



;;; Generic getter

(defun ena:generic-getter (func-list)
  "Internal function for generic getter functions (`ena:get-*').

FUNC-LIST is a list of function which takes no argument and
return what is desired or nil.  Each function in FUNC-LIST is
called one by one and the first non-nil result will be used.  The
function is not called when it is not bound.  So, it is safe to
give functions defined in lazy-loaded sub-modules.

This is something similar to dispatching in generic function such
as `defgeneric' in EIEIO, but it takes no argument.  Actual
implementation is chosen based on context (buffer, point, etc.).
This helps writing generic commands which requires same object
but can operate in different contexts."
  (loop for func in func-list
        if (and (functionp func) (funcall func))
        return it))

(defun ena:get-url-or-port ()
  (ena:generic-getter '(ena:get-url-or-port--notebooklist
                        ena:get-url-or-port--notebook
                        ena:get-url-or-port--worksheet
                        ena:get-url-or-port--shared-output
                        ena:get-url-or-port--connect)))

(defun ena:get-notebook ()
  (ena:generic-getter '(ena:get-notebook--notebook
                        ;; ena:get-notebook--shared-output
                        ena:get-notebook--connect)))

(defun ena:get-notebook-or-error ()
  (or (ena:get-notebook)
      (error "No notebook related to the current buffer.")))

(defun ena:get-kernel ()
  (ena:generic-getter '(ena:get-kernel--notebook
                        ena:get-kernel--worksheet
                        ena:get-kernel--shared-output
                        ena:get-kernel--connect)))

(defun ena:get-kernel-or-error ()
  (or (ena:get-kernel)
      (error "No kernel related to the current buffer.")))

(defun ena:get-cell-at-point ()
  (ena:generic-getter '(ena:get-cell-at-point--worksheet
                        ena:get-cell-at-point--shared-output)))

(defun ena:get-traceback-data ()
  (ena:generic-getter '(ena:get-traceback-data--worksheet
                        ena:get-traceback-data--shared-output
                        ena:get-traceback-data--connect)))



;;; Emacs utilities

(defun ena:byte-compile-ena ()
  "Byte compile EIN files."
  (interactive)
  (let* ((files (directory-files ena:source-dir 'full "^ena-.*\\.el$"))
         (errors (ena:filter
                  'identity
                  (mapcar (lambda (f) (unless (byte-compile-file f) f))
                          files))))
    (ena:aif errors
        (error "Got %s errors while compiling these files: %s"
               (length errors)
               (ena:join-str " " (mapcar #'file-name-nondirectory it))))
    (message "Compiled %s files" (length files))))


(provide 'ena-core)

;;; ena-core.el ends here

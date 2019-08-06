;;; ena-helm.el --- Helm/anything commands

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-helm.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-helm.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-helm.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))

(declare-function anything-other-buffer "anything")
(declare-function helm-other-buffer "helm")

(require 'ena-kernel)


;;; Macros

(defmacro ena:helm-export-source (name)
  (let* ((orig-source (intern (format "ena:helm-source-%s"        name)))
         (any-source  (intern (format "anything-c-source-ena-%s" name)))
         (helm-source (intern (format "helm-c-source-ena-%s"     name)))
         (docstring (format "Alias to `%s'" orig-source)))
    `(progn
       (defvaralias ',helm-source ',orig-source ,docstring)
       (defvaralias ',any-source  ',orig-source ,docstring))))


;;; Dynamic Variables

(defvar ena:helm-pattern 'helm-pattern
  "Dynamically bound to one of `helm-pattern' or `anything-pattern'.")

(defvar ena:helm-kernel nil
  "Dynamically bound to a kernel object.")



;;; History search

(defcustom ena:helm-kernel-history-search-auto-pattern t
  "Automatically construct search pattern when non-`nil'.

1. Single space is converted to \"*\".
2. A backslash followed by a space is converted to a single space.
3. A \"*\" is added at the beginning and end of the pattern.

This variable applies to both `helm-ena-kernel-history' and
`anything-ena-kernel-history'."
  :group 'ena)

(defun ena:helm-kernel-history-search-construct-pattern (pattern)
  (when ena:helm-kernel-history-search-auto-pattern
    (setq pattern
          (replace-regexp-in-string "[^\\\\ ]\\( \\)[^\\\\ ]"
                                    "*" pattern nil nil 1))
    (setq pattern
          (replace-regexp-in-string "\\\\ " " " pattern))
    (setq pattern (concat "*" pattern "*")))
  pattern)

(defun ena:helm-kernel-history-search-get-candidates ()
  "Retrieve search result from kernel.
It requires the following dynamical variables:
* `ena:helm-pattern'
* `ena:helm-kernel'"
  (let* ((pattern (ena:helm-kernel-history-search-construct-pattern
                   (eval ena:helm-pattern)))
         (candidates (ena:kernel-history-search-synchronously
                      ena:helm-kernel pattern :unique t)))
    ;; Most recent history first:
    (nreverse candidates)))

(defvar ena:helm-source-kernel-history
  '((name . "IPython history")
    (candidates . ena:helm-kernel-history-search-get-candidates)
    (requires-pattern . 3)
    ;; There is no need to filter out candidates:
    (match . (identity))
    (volatile)
    (action . insert)
    (delayed)
    (multiline))
  "Helm/anything source for searching kernel history.")

;;;###autoload
(defun anything-ena-kernel-history ()
  "Search kernel execution history then insert the selected one."
  (interactive)
  (let ((ena:helm-pattern 'anything-pattern)
        (ena:helm-kernel (ena:get-kernel-or-error)))
    (anything-other-buffer ena:helm-source-kernel-history "*anything ena*")))

;;;###autoload
(defun helm-ena-kernel-history ()
  "Search kernel execution history then insert the selected one."
  (interactive)
  (let ((ena:helm-pattern 'helm-pattern)
        (ena:helm-kernel (ena:get-kernel-or-error)))
    (helm-other-buffer ena:helm-source-kernel-history "*helm ena*")))



;;; Notebook buffers

(defvar ena:helm-source-notebook-buffers
  '((name . "All IPython notebook buffers")
    (candidates . ena:notebook-opened-buffer-names)
    (type . buffer))
  "Helm/anything source for all opened notebook buffers.")

(defvar ena:helm-source-modified-notebook-buffers
  '((name . "Modified IPython notebook buffers")
    (candidates
     . (lambda ()
         (ena:notebook-opened-buffer-names #'ena:notebook-modified-p)))
    (type . buffer))
  "Helm/anything source for modified notebook buffers.")

(defvar ena:helm-source-saved-notebook-buffers
  '((name . "Saved IPython notebook buffers")
    (candidates
     . (lambda ()
         (ena:notebook-opened-buffer-names
          (lambda (nb) (not (ena:notebook-modified-p nb))))))
    (type . buffer))
  "Helm/anything source for saved notebook buffers.")


;;; "Export" sources to `helm/anything-c-source-*'
(ena:helm-export-source notebook-buffers)
(ena:helm-export-source modified-notebook-buffers)
(ena:helm-export-source saved-notebook-buffers)


;;; Helm/anything commands

(defvar ena:helm-notebook-buffer-sources
  '(ena:helm-source-modified-notebook-buffers
    ena:helm-source-saved-notebook-buffers))

;;;###autoload
(defun anything-ena-notebook-buffers ()
  "Choose opened notebook using anything.el interface."
  (interactive)
  (anything-other-buffer ena:helm-notebook-buffer-sources "*anything ena*"))

;;;###autoload
(defun helm-ena-notebook-buffers ()
  "Choose opened notebook using helm interface."
  (interactive)
  (helm-other-buffer ena:helm-notebook-buffer-sources "*helm ena*"))

(provide 'ena-helm)
;;; ena-helm.el ends here

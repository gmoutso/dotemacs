;;; ena-multilang.el --- Notebook mode with multiple language fontification

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-multilang.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-multilang.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-multilang.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (defvar markdown-mode-map))

(require 'ena-worksheet)
(require 'ena-multilang-fontify)

(defun ena:ml-fontify (limit)
  "Fontify next input area comes after the current point then
return `t' or `nil' if not found.
See info node `(elisp) Search-based Fontification'."
  (ena:log-ignore-errors
    (ena:ml-fontify-1 limit)))

(defun ena:ml-current-or-next-input-cell (ewoc-node)
  "Almost identical to `ena:worksheet-next-input-cell' but return
the current cell if EWOC-NODE is the input area node."
  (let* ((ewoc-data (ewoc-data ewoc-node))
         (cell (ena:$node-data ewoc-data))
         (path (ena:$node-path ewoc-data))
         (element (nth 1 path)))
    (if (memql element '(prompt input))
        cell
      (ena:cell-next cell))))

(defun ena:ml-fontify-1 (limit)
  "Actual implementation of `ena:ml-fontify'.
This function may raise an error."
  (ena:and-let* ((pos (point))
                 (node (ena:worksheet-get-nearest-cell-ewoc-node pos limit))
                 (cell (ena:ml-current-or-next-input-cell node))
                 (start (ena:cell-input-pos-min cell))
                 (end   (ena:cell-input-pos-max cell))
                 ((<= end limit))
                 ((< start end))
                 (lang (ena:cell-language cell)))
    (let ((inhibit-read-only t))
      (ena:mlf-font-lock-fontify-block lang start end)
      ;; Emacs fontification mechanism requires the function to move
      ;; the point.  Do *not* use `(goto-char end)'.  As END is in the
      ;; input area, fontification falls into an infinite loop.
      (ewoc-goto-node (oref cell :ewoc) (ena:cell-element-get cell :footer)))
    t))

(defun ena:ml-back-to-prev-node ()
  (ena:aand (ena:worksheet-get-ewoc) (ewoc-goto-prev it 1)))

(defvar ena:ml-font-lock-keywords
  '((ena:ml-fontify))
  "Default `font-lock-keywords' for `ena:notebook-multilang-mode'.")

(defun ena:ml-set-font-lock-defaults ()
  (set (make-local-variable 'font-lock-defaults)
       '(ena:ml-font-lock-keywords
         ;; The following are adapted from org-mode but I am not sure
         ;; if I need them:
         t nil nil
         ena:ml-back-to-prev-node)))

;;;###autoload
(define-derived-mode ena:notebook-multilang-mode fundamental-mode "ena:ml"
  "Notebook mode with multiple language fontification."
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'parse-sexp-lookup-properties)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'beginning-of-defun-function)
  (make-local-variable 'end-of-defun-function)
  (setq beginning-of-defun-function 'ena:worksheet-beginning-of-cell-input)
  (setq end-of-defun-function 'ena:worksheet-end-of-cell-input)
  (ena:ml-lang-setup-python)
  (ena:ml-set-font-lock-defaults))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'ena:notebook-multilang-mode))


;;; Language setup functions

(defun ena:ml-lang-setup-python ()
  (setq comment-start "# ")
  (setq comment-start-skip "#+\\s-*")
  (setq parse-sexp-lookup-properties t)
  (setq parse-sexp-ignore-comments t)

  (when (boundp 'python-mode-map)
    (set-keymap-parent ena:notebook-multilang-mode-map python-mode-map))
  (cond
   ((featurep 'python)
    (setq indent-line-function #'python-indent-line-function)
    (setq indent-region-function #'python-indent-region))
   ((featurep 'python-mode)
    ;; FIXME: write keymap setup for python-mode.el
    )))

(defun ena:ml-lang-setup-markdown ()
  "Use `markdown-mode-map'.  NOTE: This function is not used now."
  (when (featurep 'markdown-mode)
    (set-keymap-parent ena:notebook-multilang-mode-map markdown-mode-map)))

;; FIXME: dynamically call ena:ml-lang-setup-LANG using
;;        `post-command-hook'.
;; FIMXE: add more ena:ml-lang-setup-LANG to switch kaymap.


;;; yasnippet

(defvar ena:ml-yasnippet-parents '(python-mode markdown-mode)
  "Parent modes for `ena:notebook-multilang-mode' to register in yasnippet.")

(defun ena:ml-setup-yasnippet ()
  (loop for define-parents in '(yas/define-parents
                                yas--define-parents)
        when (fboundp define-parents)
        do (ignore-errors
             ;; `let' is for workaround the bug in yasnippet
             (let ((mode-sym 'ena:notebook-multilang-mode))
               (funcall define-parents
                        mode-sym
                        ena:ml-yasnippet-parents)))))

(eval-after-load "yasnippet" '(ena:ml-setup-yasnippet))

(provide 'ena-multilang)

;;; ena-multilang.el ends here

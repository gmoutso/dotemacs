;;; ena-output-area.el --- Output area module

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-output-area.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; ena-output-area.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-output-area.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)

(require 'ena-core)



;;; XML/HTML utils

(defun ena:xml-parse-html-string (html-string)
  "Parse HTML-STRING and return a dom object which
can be handled by the xml module."
  (with-temp-buffer
    (erase-buffer)
    (insert html-string)
    (libxml-parse-html-region (point-min) (point-max))))

(defalias 'ena:xml-node-p 'listp)

(defun ena:xml-tree-apply (dom operation)
  "Apply OPERATION on nodes in DOM.  Apply the same OPERATION on
the next level children when it returns `nil'."
  (loop for child in (xml-node-children dom)
        if (and (not (funcall operation child))
                (ena:xml-node-p child))
        do (ena:xml-tree-apply child operation)))

(defun ena:xml-replace-attributes (dom tag attr replace-p replacer)
  "Replace value of ATTR of TAG in DOM using REPLACER
when REPLACE-P returns non-`nil'."
  (ena:xml-tree-apply
   dom
   (lambda (node)
     (ena:and-let* (((ena:xml-node-p node))
                    ((eq (xml-node-name node) tag))
                    (attr-cell (assoc attr (xml-node-attributes node)))
                    (val (cdr attr-cell))
                    ((funcall replace-p val)))
       (setcdr attr-cell (funcall replacer val))
       t))))


;;; HTML renderer

(defun ena:output-area-get-html-renderer ()
  ;; FIXME: make this configurable
  (cond
   ((and (fboundp 'shr-insert-document)
         (fboundp 'libxml-parse-xml-region))
    #'ena:insert-html-shr)
   (t #'ena:insert-read-only)))

(defcustom ena:shr-env
  '((shr-table-horizontal-line ?-)
    (shr-table-vertical-line ?|)
    (shr-table-corner ?+))
  "Variables let-bound while calling `shr-insert-document'.

To use default shr setting::

    (setq ena:shr-env nil)

Draw boundaries for table (default)::

    (setq ena:shr-env
          '((shr-table-horizontal-line ?-)
            (shr-table-vertical-line ?|)
            (shr-table-corner ?+)))
"
  :group 'ena)

(defun ena:shr-insert-document (dom)
  "`shr-insert-document' with EIN setting."
  (eval `(let ,ena:shr-env (shr-insert-document dom))))

(defun ena:insert-html-shr (html-string)
  "Render HTML-STRING using `shr-insert-document'.

Usage::

    (ena:insert-html-shr \"<b>HTML</b> string\")

"
  (let ((dom (ena:xml-parse-html-string html-string))
        (start (point))
        end)
    (ena:insert-html--fix-urls dom)
    (ena:shr-insert-document dom)
    (setq end (point))
    (put-text-property start end 'read-only t)
    (put-text-property start end 'front-sticky t)))

(defun ena:insert-html--fix-urls (dom &optional url-or-port)
  "Destructively prepend notebook server URL to local URLs in DOM."
  (ena:and-let* ((url-or-port (or url-or-port (ena:get-url-or-port)))
                 (replace-p (lambda (val) (string-match-p "^/?files/" val)))
                 (replacer (lambda (val) (ena:url url-or-port val))))
    (ena:xml-replace-attributes dom 'a 'href replace-p replacer)
    (ena:xml-replace-attributes dom 'img 'src replace-p replacer)))


(provide 'ena-output-area)

;;; ena-output-area.el ends here

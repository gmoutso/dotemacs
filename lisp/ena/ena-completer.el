;;; ena-completer.el --- Completion module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-completer.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-completer.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-completer.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(declare-function ac-cursor-on-diable-face-p "auto-complete")

(eval-when-compile (require 'cl))

(require 'ena-core)
(require 'ena-log)
(require 'ena-subpackages)
(require 'ena-kernel)

(defun ena:completer-choose ()
  (when (require 'auto-complete nil t)
    (require 'ena-ac))
  (cond
   ((and (or ena:use-auto-complete
             ena:use-auto-complete-superpack)
         (ena:eval-if-bound 'auto-complete-mode)
         (fboundp 'ena:completer-finish-completing-ac))
    #'ena:completer-finish-completing-ac)
   (t
    #'ena:completer-finish-completing-default)))

(defun ena:completer-beginning (matched-text)
  (save-excursion
    (re-search-backward (concat matched-text "\\="))))

(defun ena:completer-finish-completing (args content -metadata-not-used-)
  (ena:log 'debug "COMPLETER-FINISH-COMPLETING: content=%S" content)
  (let ((matched-text (plist-get content :matched_text))
        (matches (plist-get content :matches))
        (completer (ena:completer-choose)))
    (ena:log 'debug "COMPLETER-FINISH-COMPLETING: completer=%s" completer)
    (apply completer matched-text matches args)))

(defun ena:completer-finish-completing-default (matched-text matches
                                                             &rest -ignore-)
  (let* ((end (point))
         (beg (ena:completer-beginning matched-text))
         (word (if (and beg matches)
                   (completing-read "Complete: " matches
                                    nil nil matched-text))))
    (when word
      (delete-region beg end)
      (insert word))))

(defun* ena:completer-complete
    (kernel &rest args &key callbacks &allow-other-keys)
  "Start completion for the code at point.

.. It sends `:complete_request' to KERNEL.
   CALLBACKS is passed to `ena:kernel-complete'.

   If you specify CALLBACKS explicitly (i.e., you are not using
   `ena:completer-finish-completing'), keyword argument will be
   ignored.  Otherwise, ARGS are passed as additional arguments
   to completer callback functions.  ARGS **must** be keyword
   arguments.

   EXPAND keyword argument is supported by
   `ena:completer-finish-completing-ac'.  When it is specified,
   it overrides `ac-expand-on-auto-complete' when calling
   `auto-complete'."
  (interactive (list (ena:get-kernel)))
  (unless callbacks
    (setq callbacks
          (list :complete_reply
                (cons #'ena:completer-finish-completing
                      (ena:plist-exclude args '(:callbacks))))))
  (ena:kernel-complete kernel
                       (thing-at-point 'line)
                       (current-column)
                       callbacks))

(defun ena:completer-dot-complete ()
  "Insert dot and request completion."
  (interactive)
  (insert ".")
  (ena:and-let* ((kernel (ena:get-kernel))
                 ((not (ac-cursor-on-diable-face-p)))
                 ((ena:kernel-live-p kernel)))
    (ena:completer-complete kernel :expand nil)))

(defcustom ena:complete-on-dot t
  "Start completion when inserting a dot.  Note that
`ena:use-auto-complete' (or `ena:use-auto-complete-superpack')
must be `t' to enable this option.  This variable has effect on
notebook buffers and connected buffers."
  :type 'boolean
  :group 'ena)

(defun ena:complete-on-dot-install (map &optional func)
  (if (and ena:complete-on-dot
           (featurep 'auto-complete)
           (or ena:use-auto-complete
               ena:use-auto-complete-superpack))
      (define-key map "." (or func #'ena:completer-dot-complete))
    (define-key map "." nil)))

(provide 'ena-completer)

;;; ena-completer.el ends here

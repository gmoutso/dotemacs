;;; ena-log.el --- Logging module for ena.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-log.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-log.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-log.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ena-core)


(defvar ena:log-all-buffer-name " *ena:log-all*")

(defvar ena:log-level-def
  '(;; debugging
    (blather . 60) (trace . 50) (debug . 40)
    ;; information
    (verbose . 30) (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")
;; Some names are stolen from supervisord (http://supervisord.org/logging.html)

(defvar ena:log-level 30)
(defvar ena:log-message-level 20)

(defvar ena:log-print-level 1 "`print-level' for `ena:log'")
(defvar ena:log-print-length 10 "`print-length' for `ena:log'")
(defvar ena:log-max-string 1000)


(defun ena:log-set-level (level)
  (setq ena:log-level (ena:log-level-name-to-int level)))

(defun ena:log-set-message-level (level)
  (setq ena:log-message-level (ena:log-level-name-to-int level)))

(defun ena:log-level-int-to-name (int)
  (loop for (n . i) in ena:log-level-def
        when (>= int i)
        return n
        finally 'error))

(defun ena:log-level-name-to-int (name)
  (cdr (assq name ena:log-level-def)))

(defun ena:log-wrapper (level func)
  (setq level (ena:log-level-name-to-int level))
  (when (<= level ena:log-level)
    (let* ((levname (ena:log-level-int-to-name level))
           (print-level ena:log-print-level)
           (print-length ena:log-print-length)
           (msg (format "[%s] %s"  levname (funcall func)))
           (orig-buffer (current-buffer)))
      (if (and ena:log-max-string
               (> (length msg) ena:log-max-string))
          (setq msg (substring msg 0 ena:log-max-string)))
      (ena:with-read-only-buffer (get-buffer-create ena:log-all-buffer-name)
        (goto-char (point-max))
        (insert msg (format " @%S" orig-buffer) "\n"))
      (when (<= level ena:log-message-level)
        (message "ena: %s" msg)))))

(defmacro ena:log (level string &rest args)
  (declare (indent 1))
  `(ena:log-wrapper ,level (lambda () (format ,string ,@args))))

;; FIXME: this variable must go to somewhere more central
(defvar ena:debug nil
  "Set to non-`nil' to raise errors instead of suppressing it.
Change the behavior of `ena:log-ignore-errors'.")

(defmacro ena:log-ignore-errors (&rest body)
  "Execute BODY; if an error occurs, log the error and return nil.
Otherwise, return result of last form in BODY."
  (declare (debug t) (indent 0))
  `(if ena:debug
       (progn ,@body)
     (condition-case err
         (progn ,@body)
       (error
        (ena:log 'debug "Error: %S" err)
        (ena:log 'error (error-message-string err))
        nil))))

(defun ena:log-pop-to-all-buffer ()
  (interactive)
  (pop-to-buffer (get-buffer-create ena:log-all-buffer-name)))

(provide 'ena-log)

;;; ena-log.el ends here

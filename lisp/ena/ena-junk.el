;;; ena-junk.el --- Open a notebook to do random things

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-junk.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-junk.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-junk.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ena-notebooklist)


(define-obsolete-variable-alias 'ena:scratch-notebook-name-template
  'ena:junk-notebook-name-template "0.2.0")

(defcustom ena:junk-notebook-name-template "junk-%Y-%m-%d-%H%M%S"
  "Junk notebook name template.
This value is used from `ena:notebooklist-new-scratch-notebook'
and `ena:notebook-rename-to-scratch-command'.  This must be a
format string which can be passed to `format-time-string'."
  :type '(string :tag "Format string")
  :group 'ena)

(defun ena:junk-notebook-name ()
  "Generate new scratch notebook name based on `current-time' and
`ena:junk-notebook-name-template'."
  (format-time-string ena:junk-notebook-name-template (current-time)))


(define-obsolete-function-alias 'ena:notebooklist-new-scratch-notebook
  'ena:junk-new)

;;;###autoload
(defun ena:junk-new (name url-or-port)
  "Open a notebook to try random thing.
Notebook name is determined based on
`ena:junk-notebook-name-template'.

When prefix argument is given, it asks URL or port to use."
  (interactive (let ((name (ena:junk-notebook-name))
                     (url-or-port (or (ena:get-url-or-port)
                                      (ena:default-url-or-port))))
                 (setq name (read-string "Open notebook as: " name))
                 (when current-prefix-arg
                   (setq url-or-port (ena:notebooklist-ask-url-or-port)))
                 (list name url-or-port)))
  (ena:notebooklist-new-notebook-with-name name url-or-port))


(define-obsolete-function-alias ' ena:notebook-rename-to-scratch-command
  'ena:junk-rename)

;;;###autoload
(defun ena:junk-rename (name)
  "Rename the current notebook based on `ena:junk-notebook-name-template'
and save it immediately."
  (interactive
   (list (read-string "Rename notebook: "
                      (ena:junk-notebook-name))))
  (ena:notebook-rename-command name))

(provide 'ena-junk)

;;; ena-junk.el ends here

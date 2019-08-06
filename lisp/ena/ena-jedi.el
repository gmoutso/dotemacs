;;; ena-jedi.el --- ena Jedi

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-jedi.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-jedi.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-jedi.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'jedi nil t)

(require 'ena-ac)
(require 'ena-completer)
(eval-when-compile (require 'ena-connect))

(defvar ena:jedi-dot-complete-sources
  '(ac-source-jedi-direct ac-source-ena-direct))

(defun ena:jedi--completer-complete ()
  (let ((d (deferred:new #'identity))
        (kernel (ena:get-kernel)))
    (if (ena:kernel-live-p kernel)
        (ena:completer-complete
         kernel
         :callbacks
         (list :complete_reply
               (cons (lambda (d &rest args) (deferred:callback-post d args))
                     d)))
      ;; Pass "no match" result when kernel the request was not sent:
      (deferred:callback-post d (list nil nil)))
    d))

;;;###autoload
(defun* ena:jedi-complete (&key (expand ac-expand-on-auto-complete))
  "Run completion using candidates calculated by EIN and Jedi."
  (interactive)
  (lexical-let ((expand expand))
    (deferred:$
      (deferred:parallel              ; or `deferred:earlier' is better?
        (jedi:complete-request)
        (ena:jedi--completer-complete))
      (deferred:nextc it
        (lambda (replies)
          (destructuring-bind
              (_  ; ignore `jedi:complete-request' what returns.
               ((&key matched_text matches &allow-other-keys) ; :complete_reply
                _))  ; ignore metadata
              replies
            (ena:ac-prepare-completion matches)
            (let ((ac-expand-on-auto-complete expand))
              (ac-start))))))))
;; Why `ac-start'?  See: `jedi:complete'.

;;;###autoload
(defun ena:jedi-dot-complete ()
  "Insert \".\" and run `ena:jedi-complete'."
  (interactive)
  (insert ".")
  (unless (ac-cursor-on-diable-face-p)
    (ena:jedi-complete :expand nil)))

(defun ena:jedi-complete-on-dot-install (map)
  (ena:complete-on-dot-install map #'ena:jedi-dot-complete))

;;;###autoload
(defun ena:jedi-setup ()
  "Setup auto-completion using EIN and Jedi.el_ together.

Jedi.el_ is a Python auto-completion library for Emacs.
To use EIN and Jedi together, add the following in your Emacs setup.::

  (add-hook 'ena:connect-mode-hook 'ena:jedi-setup)

.. _Jedi.el: https://github.com/tkf/emacs-jedi"
  (let ((map ena:connect-mode-map))
    (define-key map "\C-c\C-i" 'ena:jedi-complete)
    (ena:jedi-complete-on-dot-install map)))

(provide 'ena-jedi)

;;; ena-jedi.el ends here

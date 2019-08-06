;;; ena-pager.el --- Pager module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-pager.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-pager.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-pager.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ansi-color)

(require 'ena-core)
(require 'ena-events)

;; FIXME: Make a class with `:get-notebook-name' slot like `ena:worksheet'

(defun ena:pager-new (name events)
  ;; currently pager = name.
  (ena:pager-bind-events name events)
  name)

(defun ena:pager-bind-events (pager events)
  "Bind events related to PAGER to the event handler EVENTS."
  (ena:events-on events
                 'open_with_text.Pager
                 #'ena:pager--open-with-text
                 pager))

(defun ena:pager--open-with-text (pager data)
  (let ((text (plist-get data :text)))
    (unless (equal (ena:trim text) "")
      (ena:pager-clear pager)
      (ena:pager-expand pager)
      (ena:pager-append-text pager text))))

(defun ena:pager-clear (pager)
  (ena:with-read-only-buffer (get-buffer-create pager)
    (erase-buffer)))

(defun ena:pager-expand (pager)
  (pop-to-buffer (get-buffer-create pager))
  (goto-char (point-min)))

(defun ena:pager-append-text (pager text)
  (ena:with-read-only-buffer (get-buffer-create pager)
    (insert (ansi-color-apply text))
    (ena:pager-mode)))

;; FIXME: this should be automatically called when opening pager.
(defun ena:pager-goto-docstring-bset-loc ()
  "Goto the best location of the documentation."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^Docstring:")
  (beginning-of-line 0)
  (recenter 0))

(define-derived-mode ena:pager-mode fundamental-mode "ena:pager"
  "IPython notebook pager mode."
  (view-mode)
  (font-lock-mode))

(setq ena:pager-mode-map (make-sparse-keymap))

(let ((map ena:pager-mode-map))
  (define-key map "\C-c\C-b" 'ena:pager-goto-docstring-bset-loc)
  (define-key map "q" 'bury-buffer)
  map)

(provide 'ena-pager)

;;; ena-pager.el ends here

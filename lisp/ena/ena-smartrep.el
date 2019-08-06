;;; ena-smartrep.el --- smartrep integration

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-smartrep.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-smartrep.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-smartrep.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'smartrep nil t)
(require 'ena-notebook)

(defcustom ena:smartrep-notebook-mode-alist
  '(("C-t" . ena:worksheet-toggle-cell-type)
    ("C-l" . ena:worksheet-clear-output)
    ("C-k" . ena:worksheet-kill-cell)
    ("C-y" . ena:worksheet-yank-cell)
    ("C-a" . ena:worksheet-insert-cell-above)
    ("C-b" . ena:worksheet-insert-cell-below)
    ("C-n" . ena:worksheet-goto-next-input)
    ("C-p" . ena:worksheet-goto-prev-input)
    ("C-m" . ena:worksheet-merge-cell)
    ("<up>" . ena:worksheet-move-cell-up)
    ("<down>" . ena:worksheet-move-cell-down)
    )
  "alist passed to `smartrep-define-key'."
  :group 'ena)

(defun ena:smartrep-config ()
  (interactive)
  (smartrep-define-key
      ena:notebook-mode-map
      "C-c"
    ena:smartrep-notebook-mode-alist))


(defvar ena:smartrep-config-once-called nil)

(defun ena:smartrep-config-once ()
  (unless ena:smartrep-config-once-called
    (setq ena:smartrep-config-once-called t)
    (ena:smartrep-config)))

(provide 'ena-smartrep)

;;; ena-smartrep.el ends here

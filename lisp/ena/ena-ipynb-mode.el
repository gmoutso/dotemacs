;;; ena-ipynb-mode.el --- A simple mode for ipynb file

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-ipynb-mode.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-ipynb-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-ipynb-mode.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ena-notebooklist)


(defvar ena:ipynb-parent-mode 'js-mode
  "A mode (a symbol) to use for parent mode for `ena:ipynb-mode'.
Note that this variable must be set *before* compiling EIN.")

(defalias 'ena:ipynb-parent-mode ena:ipynb-parent-mode)

;;;###autoload
(define-derived-mode ena:ipynb-mode ena:ipynb-parent-mode "ena:ipynb"
  "A simple mode for ipynb file.")

(let ((map ena:ipynb-mode-map))
  (define-key map "\C-c\C-z" 'ena:notebooklist-open-notebook-by-file-name)
  (define-key map "\C-c\C-o" 'ena:notebooklist-open-notebook-by-file-name)
  (easy-menu-define ena:ipynb-menu map "EIN IPyNB Mode Menu"
    `("EIN IPyNB File"
      ,@(ena:generate-menu
         '(("Open notebook" ena:notebooklist-open-notebook-by-file-name))))))

;;;###autoload
(add-to-list 'auto-mode-alist '(".*\\.ipynb\\'" . ena:ipynb-mode))


(provide 'ena-ipynb-mode)

;;; ena-ipynb-mode.el ends here

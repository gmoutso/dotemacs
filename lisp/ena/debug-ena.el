;;; debug-ena.el --- Debug ena.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; debug-ena.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; debug-ena.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with debug-ena.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; emacs -Q -L path/to/nxhtml/util/ -l debug-ena.el

;;; Code:

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'ena)
(require 'ena-dev)

(ena:dev-start-debug)
(ena:notebooklist-open)


;;; Extra stuff

(require 'markdown-mode nil t)
(require 'rst nil t)

(declare-function ena:ac-config "ena-ac")
(declare-function global-auto-complete-mode "auto-complete")
(when (featurep 'auto-complete)
  (global-auto-complete-mode t)
  (setq ena:use-auto-complete-superpack t))

(declare-function ena:smartrep-config "ena-smartrep")
(when (featurep 'smartrep)
  (setq ena:use-smartrep t))

(custom-set-faces
   ;; Turn off background color for mumamo major chunk, to see
   ;; highlighting of prompt and stderr.
   '(mumamo-background-chunk-major
     ((((class color) (min-colors 88) (background dark)) nil)))
   ;; '(mumamo-background-chunk-submode1
   ;;   ((((class color) (min-colors 88) (background dark)) nil)))
   )


;; Suppress this warning when using mumamo:
;; Warning: `font-lock-syntactic-keywords' is an obsolete variable (as of 24.1);
;;     use `syntax-propertize-function' instead.
;; See: http://stackoverflow.com/a/5470584/727827
(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 1))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-syntactic-keywords)))

;;; debug-ena.el ends here

;;; ena-subpackages.el --- Subpacakge management

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-subpackages.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-subpackages.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-subpackages.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (defvar ena:ac-config-once-called)
                   (defvar ena:smartrep-config-once-called))

(declare-function ena:ac-config-once "ena-ac")
(declare-function ena:smartrep-config-once "ena-smartrep")


(defcustom ena:use-auto-complete nil
  "Set to `t' to use preset auto-complete configuration.
Use `ena:use-auto-complete-superpack' when you need more powerful
auto completion."
  :type 'boolean
  :group 'ena)

(defcustom ena:use-auto-complete-superpack nil
  "Set to `t' to use preset a little bit hacky auto-complete configuration.
When this option is enabled, cached omni completion is available."
  :type 'boolean
  :group 'ena)

(defcustom ena:use-smartrep nil
  "Set to `t' to use preset smartrep configuration.

.. warning:: When used with MuMaMo (see `ena:notebook-modes'),
   keyboard macro which manipulates cell (add, remove, move,
   etc.) may start infinite loop (you need to stop it with
   ``C-g``).  Please be careful using this option if you are a
   heavy keyboard macro user.  Using keyboard macro for other
   commands is fine.

.. (Comment) I guess this infinite loop happens because the three
   modules (kmacro.el, mumamo.el and smartrep.el) touches to
   `unread-command-events' in somehow inconsistent ways."
  :type 'boolean
  :group 'ena)

(defcustom ena:load-dev nil
  "Load development helper."
  :type 'boolean
  :group 'ena)

(defun ena:subpackages-load ()
  "Load sub-packages depending on configurations."
  (when (or ena:use-auto-complete
            ena:use-auto-complete-superpack)
    (require 'ena-ac)
    (ena:ac-config-once ena:use-auto-complete-superpack))
  (when ena:use-smartrep
    (require 'ena-smartrep)
    (ena:smartrep-config-once))
  (when ena:load-dev
    (require 'ena-dev)))

(defun ena:subpackages-reload ()
  "Reload sub-packages."
  (interactive)
  (setq ena:ac-config-once-called nil)
  (setq ena:smartrep-config-once-called nil)
  (ena:subpackages-load))

(provide 'ena-subpackages)

;;; ena-subpackages.el ends here

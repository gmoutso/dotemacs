;;; ena-iexec.el --- Instant execution mode for notebook

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-iexec.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-iexec.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-iexec.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ena-worksheet)

(defcustom ena:iexec-delay 0.3
  "Delay before executing cell after change in second."
  :type 'number
  :group 'ena)

(defvar ena:iexec-timer nil)

(defun ena:iexec-execute-cell (cell)
  "Call `ena:notebook-execute-cell' after `ena:iexec-delay' second.
If the previous execution timer is not fired yet, cancel the timer."
  (when ena:iexec-timer
    (cancel-timer ena:iexec-timer))
  (setq ena:iexec-timer
        (run-with-idle-timer ena:iexec-delay nil
                             #'ena:worksheet-execute-cell
                             ena:%worksheet% cell)))

(defun ena:iexec-should-execute-p (cell beg end)
  "Return non-`nil' if CELL should be executed by the change within
BEG and END."
  (and (ena:codecell-p cell)
       this-command
       (ena:aif (ena:cell-input-pos-min cell) (<= it beg))
       (ena:aif (ena:cell-input-pos-max cell) (>= it end))))

(defun ena:iexec-after-change (beg end -ignore-len-)
  "Called via `after-change-functions' hook."
  (let ((cell (ena:worksheet-get-current-cell :pos beg)))
    (when (ena:iexec-should-execute-p cell beg end)
      (ena:iexec-execute-cell cell))))

;;;###autoload
(define-minor-mode ena:iexec-mode
  "Instant cell execution minor mode.
Code cell at point will be automatically executed after any
change in its input area."
  :lighter " ena:i"
  :group 'ena
  (if ena:iexec-mode
      (add-hook 'after-change-functions 'ena:iexec-after-change nil t)
    (remove-hook 'after-change-functions 'ena:iexec-after-change t)))

;; To avoid MuMaMo to discard `ena:iexec-after-change', make it
;; permanent local.
(put 'ena:iexec-after-change 'permanent-local-hook t)
(put 'ena:iexec-mode 'permanent-local t)

(provide 'ena-iexec)

;;; ena-iexec.el ends here

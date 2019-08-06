;;; ena-pseudo-console.el --- Pseudo console mode

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-pseudo-console.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; ena-pseudo-console.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-pseudo-console.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar ena:pseudo-console-mode-map (make-sparse-keymap))

(let ((map ena:pseudo-console-mode-map))
  (define-key map "\C-m" 'ena:worksheet-execute-cell-and-insert-below))

;;;###autoload
(define-minor-mode ena:pseudo-console-mode
  "Pseudo console mode.  Hit RET to execute code."
  :lighter " ena:pseudo"
  :keymap ena:pseudo-console-mode-map
  :group 'ena)

;; To avoid MuMaMo to discard `ena:pseudo-console-mode', make it
;; permanent local.
(put 'ena:pseudo-console-mode 'permanent-local t)

(provide 'ena-pseudo-console)

;;; ena-pseudo-console.el ends here

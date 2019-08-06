;;; ena-kill-ring.el --- Kill-ring for cells

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-kill-ring.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-kill-ring.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-kill-ring.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Stolen from simple.el.

;;; Code:

(defvar ena:kill-ring nil)
(defvar ena:kill-ring-yank-pointer nil)
(defvar ena:kill-ring-max kill-ring-max)

(defun ena:kill-new (obj)
  "Make OBJ the latest kill in the kill ring `ena:kill-ring'.
Set `ena:kill-ring-yank-pointer' to point to it."
  (push obj ena:kill-ring)
  (if (> (length ena:kill-ring) ena:kill-ring-max)
      (setcdr (nthcdr (1- ena:kill-ring-max) ena:kill-ring) nil))
  (setq ena:kill-ring-yank-pointer ena:kill-ring))

(defun ena:current-kill (n &optional do-not-move)
  "Rotate the yanking point by N places, and then return that kill.
If optional arg DO-NOT-MOVE is non-nil, then don't actually
move the yanking point; just return the Nth kill forward."
  (unless ena:kill-ring (error "Kill ring is empty"))
  (let ((ARGth-kill-element
         (nthcdr (mod (- n (length ena:kill-ring-yank-pointer))
                      (length ena:kill-ring))
                 ena:kill-ring)))
    (unless do-not-move
      (setq ena:kill-ring-yank-pointer ARGth-kill-element))
    (car ARGth-kill-element)))

(provide 'ena-kill-ring)

;;; ena-kill-ring.el ends here

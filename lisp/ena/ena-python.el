;;; ena-python.el --- Workarounds for python.el

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-python.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-python.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-python.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'python)

(require 'ena-worksheet)

(defvar ena:python-block-start
  (rx line-start
      symbol-start
      (or "def" "class" "if" "elif" "else" "try"
          "except" "finally" "for" "while" "with")
      symbol-end))

(defun ena:python-indent-calculate-levels ()
  "Forcefully set indent level to 0 when there is no python block
yet in this cell."
  (ena:and-let* ((cell (ena:worksheet-get-current-cell :noerror t))
                 (beg (ena:cell-input-pos-min cell))
                 ((< beg (point))))
    (save-excursion
      (unless (search-backward-regexp ena:python-block-start beg t)
        (setq python-indent-levels (list 0))
        (setq python-indent-current-level 0)
        t))))

(defadvice python-indent-calculate-levels
  (around ena:python-indent-calculate-levels activate)
  "Hack `python-indent-calculate-levels' to reset indent per cell.

Let's say you have a notebook something like this::

  In [1]:
  def func():
      pass

  In [2]:
  something[]

Here, ``[]`` is the cursor position.  When you hit the tab here,
you don't expect it to indent.  However, python.el tries to follow
the indent of ``func()`` then you get indentation.  This advice
workaround this problem.

Note that this workaround does not work with the MuMaMo based
notebook mode."
  (unless (ena:python-indent-calculate-levels)
    ad-do-it))

(provide 'ena-python)

;;; ena-python.el ends here

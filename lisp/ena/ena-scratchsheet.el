;;; ena-scratchsheet.el --- Worksheet without needs for saving

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-scratchsheet.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-scratchsheet.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-scratchsheet.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ena-worksheet)

(defvar ena:scratchsheet-buffer-name-template "*ena:scratch %s/%s*")

(defclass ena:scratchsheet (ena:worksheet)
  ;; Note that `data' slot is accessed when rendering worksheet.
  ;; So, set valid empty data (`nil') here.
  ((data :initarg :data :initform nil))
  :documentation
  "Worksheet without needs for saving.")

(defun ena:scratchsheet-new (nbformat get-notebook-name discard-output-p
                                      kernel events &rest args)
  (apply #'make-instance 'ena:scratchsheet
         :nbformat nbformat :get-notebook-name get-notebook-name
         :discard-output-p discard-output-p :kernel kernel :events events
         args))

(defmethod ena:worksheet--buffer-name ((ws ena:scratchsheet))
  (format ena:scratchsheet-buffer-name-template
          (ena:worksheet-url-or-port ws)
          (ena:worksheet-full-name ws)))

(provide 'ena-scratchsheet)

;;; ena-scratchsheet.el ends here

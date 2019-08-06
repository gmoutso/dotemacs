;;; ena-node.el --- Structure to hold data in ewoc node

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-node.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-node.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-node.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ewoc)

(require 'ena-core)


(defstruct ena:$node
  path                                  ; list of path
  data                                  ; actual data
  class                                 ; list
  )

(defun ena:node-new (path data &optional class &rest args)
  (apply #'make-ena:$node :path path :data data :class class args))

(defun ena:node-add-class (node &rest classes)
  (mapc (lambda (c) (add-to-list (ena:$node-class node) c)) classes))

(defun ena:node-remove-class (node &rest classes)
  (let ((node-class (ena:$node-class node)))
    (mapc (lambda (c) (setq node-class (delq c node-class))) classes)
    (setf (ena:$node-class node) node-class)))

(defun ena:node-has-class (node class)
  (memq class (ena:$node-class node)))

(defun ena:node-filter (ewoc-node-list &rest args)
  (loop for (key . class) in (ena:plist-iter args)
        do (setq ewoc-node-list
                 (loop for ewoc-node in ewoc-node-list
                       for node = (ewoc-data ewoc-node)
                       when (case key
                              (:is (ena:node-has-class node class))
                              (:not (not (ena:node-has-class node class)))
                              (t (error "%s is not supported" key)))
                       collect ewoc-node)))
  ewoc-node-list)

(provide 'ena-node)

;;; ena-node.el ends here

;;; ena-events.el --- Event module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-events.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-events.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-events.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)

(require 'ena-core)
(require 'ena-log)


;;; Events handling class

(defclass ena:events ()
  ((callbacks :initarg :callbacks :type hash-table
              :initform (make-hash-table :test 'eq)))
  "Event handler class.")

(defun ena:events-new ()
  "Return a new event handler instance."
  (make-instance 'ena:events))

(defun ena:events-trigger (events event-type &optional data)
  "Trigger EVENT-TYPE and let event handler EVENTS handle that event."
  (ena:log 'debug "Event: %S" event-type)
  (ena:aif (gethash event-type (oref events :callbacks))
      (mapc (lambda (cb-arg) (ena:funcall-packed cb-arg data)) it)
    (ena:log 'info "Unknown event: %S" event-type)))


(defmethod ena:events-on ((events ena:events) event-type
                          callback &optional arg)
  "Set event trigger hook.

When EVENT-TYPE is triggered on the event handler EVENTS,
CALLBACK is called.  CALLBACK must take two arguments:
ARG as the first argument and DATA, which is passed via
`ena:events-trigger', as the second."
  (assert (symbolp event-type))
  (let* ((table (oref events :callbacks))
         (cbs (gethash event-type table)))
    (push (cons callback arg) cbs)
    (puthash event-type cbs table)))


(provide 'ena-events)

;;; ena-events.el ends here

;;; ena-query.el --- jQuery like interface on to of url-retrieve

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-query.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-query.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-query.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'request)

(require 'ena-core)
(require 'ena-log)


;;; Utils

(defun ena:safe-funcall-packed (packed &rest args)
  (when packed
    (ena:log-ignore-errors (apply #'ena:funcall-packed packed args))))


;;; Variables

(defcustom ena:query-timeout
  (if (eq request-backend 'url-retrieve) 1000 nil)
  "Default query timeout for HTTP access in millisecond.

Setting this to `nil' means no timeout.
If you have ``curl`` command line program, it is automatically set to
`nil' as ``curl`` is reliable than `url-retrieve' therefore no need for
a workaround (see below).

If you do the same operation before the timeout, old operation
will be canceled \(see also `ena:query-singleton-ajax').

.. note:: This value exists because it looks like `url-retrieve'
   occasionally fails to finish \(start?) querying.  Timeout is
   used to let user notice that their operation is not finished.
   It also prevent opening a lot of useless process buffers.
   You will see them when closing Emacs if there is no timeout.

   If you know how to fix the problem with `url-retrieve', please
   let me know or send pull request at github!
   \(Related bug report in Emacs bug tracker:
   http://debbugs.gnu.org/cgi/bugreport.cgi?bug=11469)"
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "No timeout" nil))
  :group 'ena)


;;; Functions

(defvar ena:query-running-process-table (make-hash-table :test 'equal))

(defun* ena:query-singleton-ajax (key url &rest settings
                                      &key
                                      (timeout ena:query-timeout)
                                      &allow-other-keys)
  "Cancel the old process if there is a process associated with
KEY, then call `request' with URL and SETTINGS.  KEY is compared by
`equal'."
  (ena:query-gc-running-process-table)
  (when timeout
    (setq settings (plist-put settings :timeout (/ timeout 1000.0))))
  (ena:aif (gethash key ena:query-running-process-table)
      (unless (request-response-done-p it)
        (request-abort it)))            ; This will run callbacks
  (let ((response (apply #'request url settings)))
    (puthash key response ena:query-running-process-table)
    response))

(defun ena:query-gc-running-process-table ()
  "Garbage collect dead processes in `ena:query-running-process-table'."
  (maphash
   (lambda (key buffer)
     (when (request-response-done-p buffer)
       (remhash key ena:query-running-process-table)))
   ena:query-running-process-table))


;;; Cookie

(defalias 'ena:query-get-cookie 'request-cookie-string)

(provide 'ena-query)

;;; ena-query.el ends here

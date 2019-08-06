;;; ena-shared-output.el --- Output buffer for ena-connect.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-shared-output.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-shared-output.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-shared-output.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; When executing code from outside of notebook, some place for output
;; is needed.  This module buffer containing one special cell for that
;; purpose.

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)

(require 'ena-cell)


;;; Classes and variables

(defclass ena:shared-output-cell (ena:codecell)
  ((cell-type :initarg :cell-type :initform "shared-output")
   ;; (element-names :initform (:prompt :output :footer))
   (popup :initarg :popup :initform nil :type boolean)
   )
  "A singleton cell to show output from non-notebook buffers.")

(defclass ena:shared-output ()
  ((cell :initarg :cell :type ena:shared-output-cell)
   (events :initarg :events :type ena:events)
   (ewoc :initarg :ewoc :type ewoc)))

(defvar ena:%shared-output% nil
  "Hold an instance of `ena:shared-output'.")

(defconst ena:shared-output-buffer-name "*ena:shared-output*")


;;; Cell related

(defmethod ena:cell-execute ((cell ena:shared-output-cell) kernel code
                             &optional popup &rest args)
  (unless (plist-get args :silent)
    (setq args (plist-put args :silent nil)))
  (oset cell :popup popup)
  (oset cell :kernel kernel)
  (apply #'ena:cell-execute-internal cell kernel code args))

(defmethod ena:cell--handle-output ((cell ena:shared-output-cell)
                                    msg-type content -metadata-not-used-)
  ;; Show short message
  (ena:case-equal msg-type
    (("pyout")
     (let ((num (plist-get content :execution_count))
           (text (plist-get (plist-get content :data) :text/plain)))
       (when text
         (ena:log 'info "Out[%s]: %s" num (car (split-string text "\n"))))))
    (("stream")
     (let ((stream (or (plist-get content :stream) "stdout"))
           (text (plist-get content :data)))
       (when text
         (ena:log 'info "%s: %s" stream (car (split-string text "\n"))))))
    (t
     (ena:log 'info "Got output '%s' in the shared buffer." msg-type)))
  ;; Open `ena:shared-output-buffer-name' if necessary
  (when (oref cell :popup)
    (pop-to-buffer (ena:shared-output-create-buffer)))
  ;; Finally do the normal drawing
  (call-next-method))


;;; Main

(defun ena:shared-output-create-buffer ()
  "Get or create the shared output buffer."
  (get-buffer-create ena:shared-output-buffer-name))

(defun ena:shared-output-buffer ()
  "Get the buffer associated with `ena:%shared-output%'."
  (ewoc-buffer (oref ena:%shared-output% :ewoc)))

(defun ena:shared-output-buffer-p (&optional buffer)
  "Return non-`nil' when BUFFER (or current buffer) is shared-output buffer."
  (eq (or buffer (current-buffer)) (ena:shared-output-buffer)))

(defun ena:shared-output-healthy-p ()
  (and (ena:shared-output-p ena:%shared-output%)
       (buffer-live-p (ena:shared-output-buffer))))

(defun ena:shared-output-get-or-create ()
  (if (ena:shared-output-healthy-p)
      ena:%shared-output%
    (with-current-buffer (ena:shared-output-create-buffer)
      ;; FIXME: This is a duplication of `ena:worksheet-render'.
      ;;        Must be merged.
      (let* ((inhibit-read-only t)
             ;; Enable nonsep for ewoc object (the last argument is non-nil).
             ;; This is for putting read-only text properties to the newlines.
             (ewoc (ena:ewoc-create 'ena:worksheet-pp
                                    (ena:propertize-read-only "\n")
                                    nil t))
             (events (ena:events-new))
             (cell (ena:shared-output-cell "SharedOutputCell"
                                           :ewoc ewoc
                                           :events events)))
        (erase-buffer)
        (ena:shared-output-bind-events events)
        (setq ena:%shared-output%
              (ena:shared-output "SharedOutput" :ewoc ewoc :cell cell
                                  :events events))
        (ena:cell-enter-last cell))
      (setq buffer-read-only t)
      (ena:shared-output-mode)
      ena:%shared-output%)))

(defun ena:shared-output-bind-events (events)
  "Add dummy event handlers."
  (ena:events-on events 'set_dirty.Worksheet #'ignore)
  (ena:events-on events 'maybe_reset_undo.Worksheet #'ignore))

(defun ena:shared-output-get-cell ()
  "Get the singleton shared output cell.
Create a cell if the buffer has none."
  (oref (ena:shared-output-get-or-create) :cell))

(defun ena:shared-output-get-kernel ()
  (let ((cell (ena:shared-output-get-cell)))
    (when (slot-boundp cell :kernel)
      (oref cell :kernel))))

;;;###autoload
(defun ena:shared-output-pop-to-buffer ()
  "Open shared output buffer."
  (interactive)
  (ena:shared-output-get-or-create)
  (pop-to-buffer (ena:shared-output-create-buffer)))

(defmethod ena:shared-output-show-code-cell ((cell ena:codecell))
  "Show code CELL in shared-output buffer.
Note that this function assumed to be called in the buffer
where CELL locates."
  (let ((new (ena:cell-convert cell "shared-output")))
    ;; Make sure `ena:%shared-output%' is initialized:
    (ena:shared-output-get-or-create)
    (with-current-buffer (ena:shared-output-create-buffer)
      (let ((inhibit-read-only t)
            (ena:cell-max-num-outputs nil))
        (oset new :ewoc (oref ena:%shared-output% :ewoc))
        (oset new :events (oref ena:%shared-output% :events))
        (erase-buffer)  ; because there are only one cell anyway
        (oset ena:%shared-output% :cell new)
        (ena:cell-enter-last new)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun ena:shared-output-show-code-cell-at-point ()
  "Show code cell at point in shared-output buffer.
It is useful when the output of the cell at point is truncated.
See also `ena:cell-max-num-outputs'."
  (interactive)
  (let ((cell (ena:get-cell-at-point)))
    (if (ena:codecell-p cell)
        (ena:shared-output-show-code-cell cell)
      (error "No code cell at point."))))

(defvar ena:shared-output-eval-string-history nil
  "History of the `ena:shared-output-eval-string' prompt.")

;;;###autoload
(defun ena:shared-output-eval-string (code &optional popup verbose kernel
                                           &rest args)
  "Evaluate a piece of code.  Prompt will appear asking the code to run.
This is handy when you want to execute something quickly without
making a cell.  If the code outputs something, it will go to the
shared output buffer.  You can open the buffer by the command
`ena:shared-output-pop-to-buffer'.

.. ARGS is passed to `ena:kernel-execute'.  Unlike `ena:kernel-execute',
   `:silent' is `nil' by default."
  (interactive
   (let ((kernel (ena:get-kernel-or-error))
         ;; ... so error will be raised before user typing code if it
         ;; is impossible to execute
         (code (read-string
                "IP[y]: "
                (when (region-active-p)
                  (buffer-substring (region-beginning) (region-end)))
                'ena:shared-output-eval-string-history)))
     (list code nil t kernel)))
  (unless kernel (setq kernel (ena:get-kernel-or-error)))
  (let ((cell (ena:shared-output-get-cell)))
    (apply #'ena:cell-execute cell kernel (ena:trim-indent code) popup args))
  (when verbose
    (ena:log 'info "Code \"%s\" is sent to the kernel." code)))


;;; Generic getter

(defun ena:get-url-or-port--shared-output ()
  (ena:aand (ena:get-kernel--shared-output) (ena:kernel-url-or-port it)))

;; (defun ena:get-notebook--shared-output ())

(defun ena:get-kernel--shared-output ()
  (let ((cell (ena:get-cell-at-point--shared-output)))
    (when (and (object-p cell) (slot-boundp cell :kernel))
      (oref cell :kernel))))

(defun ena:get-cell-at-point--shared-output ()
  (when (and (ena:shared-output-p ena:%shared-output%)
             (ena:shared-output-buffer-p))
    (oref ena:%shared-output% :cell)))

(defun ena:get-traceback-data--shared-output ()
  (ena:aand (ena:get-cell-at-point--shared-output) (ena:cell-get-tb-data it)))


;;; ena:shared-output-mode

(define-derived-mode ena:shared-output-mode fundamental-mode "ena:so"
  "Shared output mode."
  (font-lock-mode))

(let ((map ena:shared-output-mode-map))
  (define-key map "\C-c\C-x" 'ena:tb-show)
  (define-key map "\M-."          'ena:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ena:pytools-jump-to-source-command)
  (define-key map "q" 'bury-buffer))

(add-hook 'ena:shared-output-mode-hook 'ena:truncate-lines-on)


(provide 'ena-shared-output)

;;; ena-shared-output.el ends here

;;; ena-pytools.el --- Python tools build on top of kernel

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-pytools.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-pytools.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-pytools.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))

;; for `ena:pytools-pandas-to-ses'
(declare-function ses-yank-tsf "ses")
(declare-function ses-command-hook "ses")

(require 'ena-kernel)

(defun ena:goto-file (filename lineno &optional other-window)
  "Jump to file FILEAME at line LINENO.
If OTHER-WINDOW is non-`nil', open the file in the other window."
  (funcall (if other-window #'find-file-other-window #'find-file) filename)
  (goto-char (point-min))
  (forward-line (1- lineno)))

(defun ena:goto-marker (marker &optional other-window)
  (funcall (if other-window #'pop-to-buffer #'switch-to-buffer)
           (marker-buffer marker))
  (goto-char marker))

(defcustom ena:propagate-connect t
  "Set to `t' to connect to the notebook after jumping to a buffer."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'ena)

(defun ena:pytools-setup-hooks (kernel)
  (push (cons #'ena:pytools-add-sys-path kernel)
        (ena:$kernel-after-start-hook kernel)))

(defun ena:pytools-add-sys-path (kernel)
  (ena:kernel-execute
   kernel
   (format "__import__('sys').path.append('%s')" ena:source-dir)))


;;; Tooltip and help

(defun ena:pytools-request-tooltip (kernel func)
  (interactive (list (ena:get-kernel-or-error)
                     (ena:object-at-point-or-error)))
  (ena:kernel-object-info-request
   kernel func (list :object_info_reply
                     (cons #'ena:pytools-finish-tooltip nil))))

(declare-function pos-tip-show "pos-tip")
(declare-function popup-tip "popup")

(defun ena:pytools-finish-tooltip (-ignore- content -metadata-not-used-)
  ;; See: Tooltip.prototype._show (tooltip.js)
  (let ((tooltip (ena:kernel-construct-help-string content))
        (defstring (ena:kernel-construct-defstring content))
        (name (plist-get content :name)))
    (if tooltip
        (cond
         ((and window-system (featurep 'pos-tip))
          (pos-tip-show tooltip 'ena:pos-tip-face nil nil 0))
         ((featurep 'popup)
          (popup-tip tooltip))
         (t (when (stringp defstring)
              (message (ena:trim (ansi-color-apply defstring))))))
      (ena:log 'info "no info for %s" name))))

(defun ena:pytools-request-help (kernel func)
  (interactive (list (ena:get-kernel-or-error)
                     (ena:object-at-point-or-error)))
  (ena:kernel-execute kernel
                      (format "%s?" func) ; = code
                      nil                 ; = callbacks
                      ;; It looks like that magic command does
                      ;; not work in silent mode.
                      :silent nil))

(defun ena:pytools-request-tooltip-or-help (&optional pager)
  "Show the help for the object at point using tooltip.
When the prefix argument ``C-u`` is given, open the help in the
pager buffer.  You can explicitly specify the object by selecting it."
  (interactive "P")
  (call-interactively (if pager
                          #'ena:pytools-request-help
                        #'ena:pytools-request-tooltip)))


;;; Source jump

(defvar ena:pytools-jump-stack nil)

(defvar ena:pytools-jump-to-source-not-found-regexp
  (ena:join-str "\\|"
                (list "^WARNING: .*"
                      "^Traceback (most recent call last):\n"
                      "^.*<ipython-input-[^>\n]+>\n"
                      "^\n")))

(defun ena:pytools-jump-to-source (kernel object &optional
                                          other-window notebook)
  (ena:log 'info "Jumping to the source of %s..." object)
  (let ((last (car ena:pytools-jump-stack)))
    (if (ena:aand last (eql (current-buffer) (marker-buffer it)))
        (unless (equal (point) (marker-position last))
          (push (point-marker) ena:pytools-jump-stack))
      (setq ena:pytools-jump-stack (list (point-marker)))))
  (ena:kernel-execute
   kernel
   (format "__import__('ena').find_source('%s')" object)
   (list
    :output
    (cons
     (lambda (packed msg-type content -metadata-not-used-)
       (destructuring-bind (kernel object other-window notebook)
           packed
         (ena:case-equal msg-type
           (("stream")
            (ena:aif (plist-get content :data)
                (if (string-match ena:pytools-jump-to-source-not-found-regexp
                                  it)
                    (ena:log 'info
                      "Jumping to the source of %s...Not found" object)
                  (destructuring-bind (filename &optional lineno &rest ignore)
                      (split-string it "\n")
                    (setq lineno (string-to-number lineno))
                    (setq filename
                          (ena:kernel-filename-from-python kernel filename))
                    (let ((ena:connect-default-notebook nil))
                      ;; Avoid auto connection to connect to the
                      ;; NOTEBOOK instead of the default one.
                      (ena:goto-file filename lineno other-window))
                    ;; Connect current buffer to NOTEBOOK. No reconnection.
                    (ena:connect-buffer-to-notebook notebook nil t)
                    (push (point-marker) ena:pytools-jump-stack)
                    (ena:log 'info
                      "Jumping to the source of %s...Done" object)))))
           (("pyerr")
            (ena:log 'info
              "Jumping to the source of %s...Not found" object)))))
     (list kernel object other-window notebook)))))

(defun ena:pytools-jump-to-source-command (&optional other-window)
  "Jump to the source code of the object at point.
When the prefix argument ``C-u`` is given, open the source code
in the other window.  You can explicitly specify the object by
selecting it."
  (interactive "P")
  (let ((kernel (ena:get-kernel))
        (object (ena:object-at-point)))
    (assert (ena:kernel-live-p kernel) nil "Kernel is not ready.")
    (assert object nil "Object at point not found.")
    (ena:pytools-jump-to-source kernel object other-window
                                (when ena:propagate-connect
                                  (ena:get-notebook)))))

(defun ena:pytools-jump-back-command (&optional other-window)
  "Go back to the point where `ena:pytools-jump-to-source-command'
is executed last time.  When the prefix argument ``C-u`` is
given, open the last point in the other window."
  (interactive "P")
  (when (ena:aand (car ena:pytools-jump-stack)
                  (equal (point) (marker-position it)))
    (setq ena:pytools-jump-stack (cdr ena:pytools-jump-stack)))
  (ena:aif (car ena:pytools-jump-stack)
      (ena:goto-marker it other-window)
    (ena:log 'info "Nothing on stack.")))

(define-obsolete-function-alias
  'ena:pytools-eval-string-internal
  'ena:shared-output-eval-string "0.1.2")

(defun ena:pytools-doctest ()
  "Do the doctest of the object at point."
  (interactive)
  (let ((object (ena:object-at-point)))
    (ena:shared-output-eval-string
     (format "__import__('ena').run_docstring_examples(%s)" object)
     t)))

(defun ena:pytools-whos ()
  "Execute ``%whos`` magic command and popup the result."
  (interactive)
  (ena:shared-output-eval-string "%whos" t))

(defun ena:pytools-hierarchy (&optional ask)
  "Draw inheritance graph of the class at point.
hierarchymagic_ extension is needed to be installed.
You can explicitly specify the object by selecting it.

.. _hierarchymagic: https://github.com/tkf/ipython-hierarchymagic"
  (interactive "P")
  (let ((object (ena:object-at-point)))
    (when ask
      (setq object (read-from-minibuffer "class or object: " object)))
    (assert (and object (not (equal object "")))
            nil "Object at point not found.")
    (ena:shared-output-eval-string (format "%%hierarchy %s" object) t)))

(defun ena:pytools-pandas-to-ses (dataframe)
  "View pandas_ DataFrame in SES_ (Simple Emacs Spreadsheet).
Open a `ses-mode' buffer and import DataFrame object into it.

SES_ is distributed with Emacs since Emacs 22, so you don't need
to install it if you are using newer Emacs.

.. _pandas: http://pandas.pydata.org
.. _SES: http://www.gnu.org/software/emacs/manual/html_node/ses/index.html"
  (interactive (list (read-from-minibuffer "pandas DataFrame "
                                           (ena:object-at-point))))
  (let ((buffer (get-buffer-create
                 (generate-new-buffer-name "*ena:ses pandas*"))))
    ;; fetch TSV (tab separated values) via stdout
    (ena:kernel-request-stream
     (ena:get-kernel)
     (concat dataframe ".to_csv(__import__('sys').stdout, sep='\\t')")
     (lambda (tsv buffer)
       (with-current-buffer buffer
         (flet ((y-or-n-p
                 (prompt)
                 (if (string-prefix-p "Yank will insert " prompt)
                     t
                   (error "Unexpected prompt: %s" prompt))))
           ;; Import DataFrame as TSV
           (ses-yank-tsf tsv nil))
         ;; Force SES to update (equivalent to run `post-command-hook').
         (ses-command-hook)))
     (list buffer))
    ;; Open `ses-mode' buffer
    (with-current-buffer buffer
      (ses-mode))
    (pop-to-buffer buffer)))

(provide 'ena-pytools)

;;; ena-pytools.el ends here

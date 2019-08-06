;;; ena-mumamo.el --- MuMaMo for notebook

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-mumamo.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-mumamo.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-mumamo.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'mumamo)

(require 'ena-worksheet)



;;; Customization

(defcustom ena:mumamo-codecell-mode 'python-mode
  "Major Mode for Code Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ena)

(defcustom ena:mumamo-textcell-mode 'text-mode
  "Major Mode for Text Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ena)

(defcustom ena:mumamo-htmlcell-mode 'html-mode
  "Major Mode for HTML Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ena)

(defcustom ena:mumamo-markdowncell-mode 'markdown-mode
  "Major Mode for Markdown Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ena)

(defcustom ena:mumamo-rawcell-mode 'rst-mode
  "Major Mode for Raw Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ena)

(defcustom ena:mumamo-headingcell-mode 'text-mode
  "Major Mode for Heading Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ena)

(defcustom ena:mumamo-fallback-mode 'text-mode
  "Fallback Major Mode."
  :type '(symbol :tag "Major Mode")
  :group 'ena)

(defcustom ena:use-mumamo-indent-line-function-workaround t
  "Turn on workaround for `mumamo-indent-line-function'.

In code cell, hitting TAB or C-j at the end of input area causes
error from MuMaMo.  When this variable is non-`nil', EIN patches
`mumamo-indent-line-function' to workaround this problem.  This
workaround is on by default.

Note that python-mode's indentation function has other problems
with MuMaMo.  For example, hitting TAB twice, which decreases the
indentation level by one in normal Python buffer, causes similar
error in code cell.  The current workaround does not fix this
problem."
  :type 'boolean
  :group 'ena)

(defcustom ena:mumamo-indent-line-function-dummy-code "
def ena_dummy():
    return"
  "Dummy code block for `mumamo-indent-line-function' workaround.
This code block will be inserted at the end of cell input before
indentation and then removed afterward (so user will not see this
code).

This is ugly but... \"practicality beats purity\"...
I guess somebody should fix python.el and/or MuMaMo, in order to
remove this ugliness.

To make the workaround less aggressive, you can set a newline
\"\\n\" for this variable.  In that case, you will be affected by
`issue 24`_.

.. _issue 24: https://github.com/tkf/emacs-ipython-notebook/issues/24"
  :type 'boolean
  :group 'ena)



;;; Workaround

(defadvice mumamo-indent-line-function
  (around ena:mumamo-indent-line-function-workaround)
  "Workaround the indentation problem when the cursor is in the
code cell."
  (let ((cell (ena:worksheet-get-current-cell)))
    ;; Check if the current buffer is notebook AND the current cell is
    ;; code cell.
    (if (ena:codecell-p cell)
        (let ((cur (copy-marker (point)))
              (end (copy-marker (1+ (ena:cell-input-pos-max cell)))))
          ;;             v-- execute `delete-char' here
          ;; ... [] ......DUMMY
          ;;      ^- cur       ^- end (non-inclusive end of cell)
          ;;      ^- `ad-do-it' here
          (unwind-protect
              (progn
                (goto-char (1- end))
                (insert ena:mumamo-indent-line-function-dummy-code)
                (goto-char cur)
                ad-do-it)
            (save-excursion
              (let ((len (length ena:mumamo-indent-line-function-dummy-code)))
                (goto-char (- end 1 len))
                (delete-char len)))))
      ad-do-it)))

(defun ena:mumamo-indent-line-function-workaround-turn-on ()
  "Activate advice for `mumamo-indent-line-function'.
Called via `ena:notebook-mumamo-mode-hook'."
  (when ena:use-mumamo-indent-line-function-workaround
    (ad-enable-advice 'mumamo-indent-line-function 'around
                      'ena:mumamo-indent-line-function-workaround)
    (ad-activate 'mumamo-indent-line-function)))

(defun ena:mumamo-imenu-setup-maybe ()
  "Set `imenu-create-index-function' if the current buffer is the
notebook buffer.
This function is called via `after-change-major-mode-hook', to set
the variable every time visiting the different chunks.

.. note:: Making `imenu-create-index-function' permanent-local
   also solves the problem.  However, this will make the variable
   permanent-local in *any* buffer, including the buffers
   irrelevant to EIN.  Therefore, the current approach is taken.

This is the same workaround as `ena:ac-setup-maybe'."
  (when (ena:worksheet-buffer-p)
    (ena:worksheet-imenu-setup)))

(add-hook 'after-change-major-mode-hook 'ena:mumamo-imenu-setup-maybe)



;;; `ena:notebook-mumamo-mode'

(define-derived-mode ena:notebook-bg-mode fundamental-mode "ena:bg"
  "Background mode for `ena:notebook-mumamo-mode'."
  (setq font-lock-defaults '(nil t))
  (font-lock-mode))

(define-mumamo-multi-major-mode ena:notebook-mumamo-mode
  "IPython notebook mode."
  ("IPython notebook familiy" ena:notebook-bg-mode
   (ena:mumamo-chunk-codecell
    ena:mumamo-chunk-textcell
    ena:mumamo-chunk-htmlcell
    ena:mumamo-chunk-markdowncell
    ena:mumamo-chunk-rawcell
    ena:mumamo-chunk-headingcell
    )))

(add-hook 'ena:notebook-mumamo-mode-hook
          'ena:mumamo-indent-line-function-workaround-turn-on)



;;; Chunk functions

(defmacro ena:mumamo-define-chunk (name)
  (let ((funcname (intern (format "ena:mumamo-chunk-%s" name)))
        (mode (intern (format "ena:mumamo-%s-mode" name)))
        (cell-p (intern (format "ena:%s-p" name))))
    `(defun ,funcname (pos max)
       (mumamo-possible-chunk-forward
        pos max
        (lambda (pos max) "CHUNK-START-FUN"
          (ena:log 'blather "CHUNK-START-FUN(pos=%s max=%s)" pos max)
          (ena:aif (ena:mumamo-find-edge pos max nil #',cell-p)
              (list it (if (functionp ,mode)
                           ,mode
                         ena:mumamo-fallback-mode)
                    nil)))
        (lambda (pos max) "CHUNK-END-FUN"
          (ena:log 'blather "CHUNK-END-FUN(pos=%s max=%s)" pos max)
          (ena:mumamo-find-edge pos max t #',cell-p))))))

(ena:mumamo-define-chunk codecell)
(ena:mumamo-define-chunk textcell)
(ena:mumamo-define-chunk htmlcell)
(ena:mumamo-define-chunk markdowncell)
(ena:mumamo-define-chunk rawcell)
(ena:mumamo-define-chunk headingcell)

(defun ena:mumamo-find-edge (pos max end cell-p)
  "Helper function for `ena:mumamo-chunk-codecell'.

Return the point of beginning of the input element of cell after
the point POS.  Return `nil' if it cannot be found before the point
MAX.  If END is non-`nil', end of the input element is returned."
  (ena:log 'blather "EIN:MUMAMO-FIND-EDGE(pos=%s max=%s end=%s cell-p=%s)"
           pos max end cell-p)
  (let* ((ewoc-node
          (ena:worksheet-get-nearest-cell-ewoc-node pos max cell-p))
         (_ (ena:log 'blather "(null ewoc-node) = %s" (null ewoc-node)))
         (cell (ena:aand ewoc-node
                         (ena:$node-data (ewoc-data it))))
         (_ (ena:log 'blather "(null cell) = %s" (null cell)))
         (find
          (lambda (c)
            (ena:aand c
                      (ena:cell-element-get it (if end :after-input :input))
                      (progn
                        (ena:log 'blather "(null it) = %s" (null it))
                        (ewoc-location it))
                      (if end it (1+ it)))))
         (input-pos (funcall find cell)))
    (ena:log 'blather "input-pos (1) = %s" input-pos)
    (when (and input-pos (< input-pos pos))
      (setq input-pos (ena:aand (ena:cell-next cell)
                                (when (funcall cell-p it) (funcall find it)))))
    (ena:log 'blather "input-pos (2) = %s" input-pos)
    (when (and input-pos (> input-pos max))
      (setq input-pos nil))
    (ena:log 'blather "input-pos (3) = %s" input-pos)
    input-pos))

(provide 'ena-mumamo)

;;; ena-mumamo.el ends here

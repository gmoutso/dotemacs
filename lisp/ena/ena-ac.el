;;; ena-ac.el --- Auto-complete extension

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-ac.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-ac.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-ac.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'auto-complete nil t)

(require 'ena-core)
(eval-when-compile (require 'ena-notebook)
                   (defvar ena:mumamo-codecell-mode))


;;; Configuration

(defvar ena:ac-sources (and (boundp 'ac-sources)
                            (default-value 'ac-sources))
  "Extra `ac-sources' used in notebook.")

(make-obsolete-variable 'ena:ac-max-cache nil "0.1.2")
(defcustom ena:ac-max-cache 1000
  "[This value is not used anymore!]
Maximum number of cache to store."
  :type 'integer
  :group 'ena)


;;; Chunk (adapted from auto-complete-chunk.el)

(defvar ena:ac-chunk-regex
  (rx (group (| (syntax whitespace)
                (syntax open-parenthesis)
                (syntax close-parenthesis)
                (syntax string-quote) ; Complete files for `open("path/..`
                bol))
      (? (syntax punctuation))          ; to complete ``~/PATH/...``
      (* (+ (| (syntax word) (syntax symbol)))
         (syntax punctuation))
      (+ (| (syntax word) (syntax symbol)))
      (? (syntax punctuation))
      point)
  "A regexp that matches to a \"chunk\" containing words and dots.")

(defun ena:ac-chunk-beginning ()
  "Return the position where the chunk begins."
  (ignore-errors
    (save-excursion
      (+ (re-search-backward ena:ac-chunk-regex) (length (match-string 1))))))

(defun ena:ac-chunk-candidates-from-list (chunk-list)
  "Return matched candidates in CHUNK-LIST."
  (let* ((start (ena:ac-chunk-beginning)))
    (when start
      (loop with prefix = (buffer-substring start (point))
            for cc in chunk-list
            when (string-prefix-p prefix cc)
            collect cc))))


;;; AC Source

(defvar ena:ac-direct-matches nil
  "Variable to store completion candidates for `auto-completion'.")
;; FIXME: Maybe this should be buffer-local?

(defun ena:ac-direct-get-matches ()
  (ena:ac-chunk-candidates-from-list ena:ac-direct-matches))

(ac-define-source ena-direct
  '((candidates . ena:ac-direct-get-matches)
    (requires . 0)
    (prefix . ena:ac-chunk-beginning)
    (symbol . "s")))

(ac-define-source ena-async
  '((candidates . ena:ac-direct-get-matches)
    (requires . 0)
    (prefix . ena:ac-chunk-beginning)
    (init . ena:ac-request-in-background)
    (symbol . "c")))

(define-obsolete-function-alias 'ac-complete-ena-cached 'ac-complete-ena-async
  "0.2.1")
(define-obsolete-variable-alias 'ac-source-ena-cached 'ac-source-ena-async
  "0.2.1")

(defun ena:ac-request-in-background ()
  (ena:and-let* ((kernel (ena:get-kernel))
                 ((ena:kernel-live-p kernel)))
    (ena:completer-complete
     kernel
     :callbacks
     (list :complete_reply
           (cons (lambda (_ content __)
                   (ena:ac-prepare-completion (plist-get content :matches)))
                 nil)))))


;;; Completer interface

(defun ena:ac-prepare-completion (matches)
  "Prepare `ac-source-ena-direct' using MATCHES from kernel.
Call this function before calling `auto-complete'."
  (when matches
    (setq ena:ac-direct-matches matches)))  ; let-binding won't work

(defun* ena:completer-finish-completing-ac
    (matched-text
     matches
     &key (expand ac-expand-on-auto-complete)
     &allow-other-keys)
  "Invoke completion using `auto-complete'.
Only the argument MATCHES is used.  MATCHED-TEXT is for
compatibility with `ena:completer-finish-completing-default'."
  ;; I don't need to check if the point is at right position, as in
  ;; `ena:completer-finish-completing-default' because `auto-complete'
  ;; checks it anyway.
  (ena:log 'debug "COMPLETER-FINISH-COMPLETING-AC: matched-text=%S matches=%S"
           matched-text matches)
  (ena:ac-prepare-completion matches)
  (when matches      ; No auto-complete drop-down list when no matches
    (let ((ac-expand-on-auto-complete expand))
      (ac-start))))
;; Why `ac-start'?  See: `jedi:complete'.


;;; Async document request hack

(defun ena:ac-request-document-for-selected-candidate ()
  "Request object information for the candidate at point.
This is called via `ac-next'/`ac-previous'/`ac-update' and set
`document' property of the current candidate string.  If server
replied within `ac-quick-help-delay' seconds, auto-complete will
popup help string."
  (let* ((candidate (ac-selected-candidate))
         (kernel (ena:get-kernel))
         (callbacks (list :object_info_reply
                          (cons #'ena:ac-set-document candidate))))
    (when (and candidate
               (ena:kernel-live-p kernel)
               (not (get-text-property 0 'document candidate)))
      (ena:log 'debug "Requesting object info for AC candidate %S"
               candidate)
      (ena:kernel-object-info-request kernel candidate callbacks))))

(defun ena:ac-set-document (candidate content -metadata-not-used-)
  (ena:log 'debug "EIN:AC-SET-DOCUMENT candidate=%S content=%S"
           candidate content)
  (put-text-property 0 (length candidate)
                     'document (ena:kernel-construct-help-string content)
                     candidate))

(defadvice ac-next (after ena:ac-next-request)
  "Monkey patch `auto-complete' internal function to request
help documentation asynchronously."
  (ena:ac-request-document-for-selected-candidate))

(defadvice ac-previous (after ena:ac-previous-request)
  "Monkey patch `auto-complete' internal function to request
help documentation asynchronously."
  (ena:ac-request-document-for-selected-candidate))

(defadvice ac-update (after ena:ac-update-request)
  "Monkey patch `auto-complete' internal function to request help
documentation asynchronously.  This will request info for the
first candidate when the `ac-menu' pops up."
  (ena:ac-request-document-for-selected-candidate))


;;; Setup

(defun ena:ac-superpack ()
  "Enable richer auto-completion.

* Enable auto-completion help by monkey patching `ac-next'/`ac-previous'"
  (interactive)
  (ad-enable-advice 'ac-next     'after 'ena:ac-next-request)
  (ad-enable-advice 'ac-previous 'after 'ena:ac-previous-request)
  (ad-enable-advice 'ac-update   'after 'ena:ac-update-request)
  (ad-activate 'ac-next)
  (ad-activate 'ac-previous)
  (ad-activate 'ac-update))

(defun ena:ac-setup ()
  "Call this function from mode hook (see `ena:ac-config')."
  (setq ac-sources (append '(ac-source-ena-async) ena:ac-sources)))

(defun ena:ac-setup-maybe ()
  "Setup `ac-sources' for mumamo.

.. note:: Setting `ena:notebook-mumamo-mode-hook' does not work
   because `ac-sources' in `ena:notebook-mumamo-mode'-enabled
   buffer is *chunk local*, rather than buffer local.

   Making `ac-sources' permanent-local also addresses issue of
   MuMaMo discarding `ac-sources'.  However, it effects to entire
   Emacs setting.  So this is not the right way to do it.

   Using `mumamo-make-variable-buffer-permanent' (i.e., adding
   `ac-sources' to `mumamo-per-buffer-local-vars' or
   `mumamo-per-main-major-local-vars') is also not appropriate.
   Adding `ac-sources' to them makes it impossible to different
   `ac-sources' between chunks, which is good for EIN but may not
   for other package."
  (and ena:%notebook%
       (ena:eval-if-bound 'ena:notebook-mumamo-mode)
       (eql major-mode ena:mumamo-codecell-mode)
       (ena:ac-setup)))

(defun ena:ac-config (&optional superpack)
  "Install auto-complete-mode for notebook modes.
Specifying non-`nil' to SUPERPACK enables richer auto-completion
\(see `ena:ac-superpack')."
  (add-hook 'after-change-major-mode-hook 'ena:ac-setup-maybe)
  (add-hook 'ena:notebook-mode-hook 'ena:ac-setup)
  (when superpack
    (ena:ac-superpack)))


(defvar ena:ac-config-once-called nil)

(defun ena:ac-config-once (&optional superpack)
  (unless ena:ac-config-once-called
    (setq ena:ac-config-once-called t)
    (ena:ac-config superpack)))

(provide 'ena-ac)

;;; ena-ac.el ends here

;;; ena-traceback.el --- Traceback module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-traceback.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-traceback.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-traceback.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ewoc)
(require 'ansi-color)

(require 'ena-core)

(defclass ena:traceback ()
  ((tb-data :initarg :tb-data :type list)
   (buffer-name :initarg :buffer-name :type string)
   (buffer :initarg :buffer :type buffer)
   (ewoc :initarg :ewoc :type ewoc)))

(ena:deflocal ena:%traceback% nil
  "Buffer local variable to store an instance of `ena:traceback'.")

(defvar ena:tb-buffer-name-template "*ena:tb %s/%s*")

(defun ena:tb-new (buffer-name)
  (ena:traceback "Traceback" :buffer-name buffer-name))

(defmethod ena:tb-get-buffer ((traceback ena:traceback))
  (unless (and (slot-boundp traceback :buffer)
               (buffer-live-p (oref traceback :buffer)))
    (let ((buf (get-buffer-create (oref traceback :buffer-name))))
      (oset traceback :buffer buf)))
  (oref traceback :buffer))

(defun ena:tb-pp (ewoc-data)
  (insert (ansi-color-apply ewoc-data)))

(defmethod ena:tb-render ((traceback ena:traceback) tb-data)
  (with-current-buffer (ena:tb-get-buffer traceback)
    (setq ena:%traceback% traceback)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t)
          (ewoc (ena:ewoc-create #'ena:tb-pp)))
      (erase-buffer)
      (oset traceback :ewoc ewoc)
      (oset traceback :tb-data tb-data)
      (mapc (lambda (data) (ewoc-enter-last ewoc data)) tb-data))
    (ena:traceback-mode)))

(defmethod ena:tb-popup ((traceback ena:traceback) tb-data)
  (ena:tb-render traceback tb-data)
  (pop-to-buffer (ena:tb-get-buffer traceback)))

;;;###autoload
(defun ena:tb-show ()
  "Show full traceback in traceback viewer."
  (interactive)
  (unless
      (ena:and-let* ((tb-data (ena:get-traceback-data))
                     (url-or-port (ena:get-url-or-port))
                     (kernel (ena:get-kernel))
                     (kr-id (ena:kernel-id kernel))
                     (tb-name (format ena:tb-buffer-name-template
                                      url-or-port kr-id)))
        (ena:tb-popup (ena:tb-new tb-name) tb-data)
        t)
    (error "No traceback is available.")))

(defmethod ena:tb-range-of-node-at-point ((traceback ena:traceback))
  (let* ((ewoc (oref traceback :ewoc))
         (ewoc-node (ewoc-locate ewoc))
         (beg (ewoc-location ewoc-node))
         (end (ena:aand (ewoc-next ewoc ewoc-node) (ewoc-location it))))
    (list beg end)))

(defmethod ena:tb-file-path-at-point ((traceback ena:traceback))
  (destructuring-bind (beg end)
      (ena:tb-range-of-node-at-point traceback)
    (let* ((file-tail
            (if (>= emacs-major-version 24)
                (next-single-property-change beg 'font-lock-face nil end)
              ;; For Emacs 23.x:
              (next-single-property-change beg 'face nil end)))
           (file (when file-tail
                   (buffer-substring-no-properties beg file-tail))))
      (if (string-match "\\.pyc$" file)
          (concat (file-name-sans-extension file) ".py")
        file))))

(defmethod ena:tb-file-lineno-at-point ((traceback ena:traceback))
  (destructuring-bind (beg end)
      (ena:tb-range-of-node-at-point traceback)
    (when (save-excursion
            (goto-char beg)
            (search-forward-regexp "^[-]+> \\([0-9]+\\)" end t))
      (string-to-number (match-string 1)))))

(defmethod ena:tb-jump-to-source-at-point ((traceback ena:traceback)
                                           &optional select)
  (let ((file (ena:tb-file-path-at-point traceback))
        (lineno (ena:tb-file-lineno-at-point traceback)))
    (assert (file-exists-p file) nil "File %s does not exist." file)
    (let ((buf (find-file-noselect file))
          (scroll (lambda ()
                    (goto-char (point-min))
                    (forward-line (1- lineno)))))
      (if select
          (progn (pop-to-buffer buf)
                 (funcall scroll))
        (with-selected-window (display-buffer buf)
          (funcall scroll))))))

(defun ena:tb-jump-to-source-at-point-command (&optional select)
  (interactive "P")
  (ena:tb-jump-to-source-at-point ena:%traceback% select))


;;; ena:traceback-mode

(defun ena:tb-prev-item ()
  (interactive)
  (ewoc-goto-prev (oref ena:%traceback% :ewoc) 1))

(defun ena:tb-next-item ()
  (interactive)
  (ewoc-goto-next (oref ena:%traceback% :ewoc) 1))

(define-derived-mode ena:traceback-mode fundamental-mode "ena:tb"
  (font-lock-mode))

(add-hook 'ena:traceback-mode-hook 'ena:truncate-lines-on)

(let ((map ena:traceback-mode-map))
  (define-key map (kbd "RET") 'ena:tb-jump-to-source-at-point-command)
  (define-key map "p" 'ena:tb-prev-item)
  (define-key map "n" 'ena:tb-next-item)
  (define-key map "q" 'bury-buffer))

(provide 'ena-traceback)

;;; ena-traceback.el ends here

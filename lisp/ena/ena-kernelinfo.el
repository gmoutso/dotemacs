;;; ena-kernelinfo.el --- Kernel info module

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-kernelinfo.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; ena-kernelinfo.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-kernelinfo.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ena-kernel)
(require 'eieio)

(defclass ena:kernelinfo ()
  ((kernel
    :initarg :kernel :type ena:$kernel
    :documentation "Kernel instance.")
   (get-buffers
    :initarg :get-buffers
    :documentation "A packed function to get buffers associated
with the kernel.  The buffer local `default-directory' variable
in these buffer will be synced with the kernel's cwd.")
   (hostname
    :initarg :hostname :type string
    :documentation "Host name of the machine where the kernel is running on.")
   (ccwd
    :initarg :ccwd :type string
    :documentation "cached CWD (last time checked CWD)."))
  :documentation "Info related (but unimportant) to kernel")

(defun ena:kernelinfo-new (kernel get-buffers)
  "Make a new `ena:kernelinfo' instance based on KERNEL and GET-BUFFERS."
  (let ((kerinfo (make-instance 'ena:kernelinfo)))
    (oset kerinfo :kernel kernel)
    (oset kerinfo :get-buffers get-buffers)
    (ena:kernelinfo-setup-hooks kerinfo)
    kerinfo))

(defun ena:kernelinfo-setup-hooks (kerinfo)
  "Add `ena:kernelinfo-update-*' to `ena:$kernel-after-*-hook'."
  (with-slots (kernel) kerinfo
    (push (cons #'ena:kernelinfo-update-all kerinfo)
          (ena:$kernel-after-start-hook kernel))
    (push (cons #'ena:kernelinfo-update-ccwd kerinfo)
          (ena:$kernel-after-execute-hook kernel))))

(defun ena:kernelinfo-update-all (kerinfo)
  "Update KERINFO slots by triggering all update functions."
  (ena:log 'debug "EIN:KERNELINFO-UPDATE-ALL")
  (ena:log 'debug "(ena:kernel-live-p kernel) = %S"
           (ena:kernel-live-p (oref kerinfo :kernel)))
  (ena:kernelinfo-update-ccwd kerinfo)
  (ena:kernelinfo-update-hostname kerinfo))

(defun ena:kernelinfo-update-ccwd (kerinfo)
  "Update cached current working directory (CCWD) and change
`default-directory' of kernel related buffers."
  (ena:kernel-request-stream
   (oref kerinfo :kernel)
   "__import__('sys').stdout.write(__import__('os').getcwd())"
   (lambda (cwd kerinfo)
     (with-slots (kernel get-buffers) kerinfo
       (setq cwd (ena:kernel-filename-from-python kernel cwd))
       (oset kerinfo :ccwd cwd)
       ;; sync buffer's `default-directory' with CWD
       (when (file-accessible-directory-p cwd)
         (mapc (lambda (buffer)
                 (with-current-buffer buffer
                   (setq default-directory (file-name-as-directory cwd))))
               (ena:filter #'buffer-live-p
                           (ena:funcall-packed get-buffers))))))
   (list kerinfo)))

(defun ena:kernelinfo-update-hostname (kerinfo)
  "Get hostname in which kernel is running and store it in KERINFO."
  (ena:kernel-request-stream
   (oref kerinfo :kernel)
   "__import__('sys').stdout.write(__import__('os').uname()[1])"
   (lambda (hostname kerinfo)
     (oset kerinfo :hostname hostname))
   (list kerinfo)))


(provide 'ena-kernelinfo)

;;; ena-kernelinfo.el ends here

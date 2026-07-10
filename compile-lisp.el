#!/usr/bin/env emacs --script
;; Script to byte-compile lisp directory with proper dependencies loaded

;; Load init.el first to ensure all packages are available
(load-file "~/.emacs.d/init.el")

;; Now byte-compile the lisp directory
(byte-recompile-directory "~/.emacs.d/lisp" 0 t)

(message "Byte compilation complete!")

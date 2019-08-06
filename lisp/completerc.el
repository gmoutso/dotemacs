;;; completerc.el --- load yas company etc           -*- lexical-binding: t; -*-

;; Copyright (C) 2017  George Moutsopoulos

;; Author: George Moutsopoulos <moutsopoulosg@evaluex003>

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; company
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))
(setq company-idle-delay nil)

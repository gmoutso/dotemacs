(require 'ein)
(require 'ein-loaddefs)
(require 'ein-notebook)
(require 'ein-subpackages)
(require 'ein-connect)
(define-key ein:connect-mode-map (kbd "C-c C-c") 'ein:connect-eval-buffer)
(setq ein:worksheet-enable-undo t)

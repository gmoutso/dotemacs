;;; ena-websocket.el --- Wrapper of websocket.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-websocket.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-websocket.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-websocket.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'websocket)

(require 'ena-core)


(defstruct ena:$websocket
  "A wrapper object of `websocket'.

`ena:$websocket-ws'               : an instance returned by `websocket-open'

`ena:$websocket-onmessage'        : function called with (PACKET &rest ARGS)'
`ena:$websocket-onclose'          : function called with (WEBSOCKET &rest ARGS)'
`ena:$websocket-onopen'           : function called with (&rest ARGS)'

`ena:$websocket-onmessage-args'   : optional arguments for onmessage callback'
`ena:$websocket-onclose-args'     : optional arguments for onclose callback'
`ena:$websocket-onopen-args'      : optional arguments for onopen callback'

`ena:$websocket-closed-by-client' : t/nil'
"
  ws
  onmessage
  onmessage-args
  onclose
  onclose-args
  onopen
  onopen-args
  closed-by-client)


(defun ena:websocket (url &optional onmessage onclose onopen
                          onmessage-args onclose-args onopen-args)
  (let ((websocket (make-ena:$websocket
                    :onmessage onmessage
                    :onclose onclose
                    :onopen onopen
                    :onmessage-args onmessage-args
                    :onclose-args onclose-args
                    :onopen-args onopen-args))
        (ws (websocket-open
             url
             :on-open
             (lambda (ws)
               (let ((websocket (websocket-client-data ws)))
                 (ena:aif (ena:$websocket-onopen websocket)
                     (apply it (ena:$websocket-onopen-args websocket)))))
             :on-message
             (lambda (ws frame)
               (let ((websocket (websocket-client-data ws))
                     (packet (websocket-frame-payload frame)))
                 (ena:aif (ena:$websocket-onmessage websocket)
                     (when packet
                       (apply it packet
                              (ena:$websocket-onmessage-args websocket))))))
             :on-close
             (lambda (ws)
               (let ((websocket (websocket-client-data ws)))
                 (ena:aif (ena:$websocket-onclose websocket)
                     (apply it websocket
                            (ena:$websocket-onclose-args websocket))))))))
    (setf (websocket-client-data ws) websocket)
    (setf (ena:$websocket-ws websocket) ws)
    websocket))


(defun ena:websocket-open-p (websocket)
  (eql (websocket-ready-state (ena:$websocket-ws websocket)) 'open))


(defun ena:websocket-send (websocket text)
  (websocket-send-text (ena:$websocket-ws websocket) text))


(defun ena:websocket-close (websocket)
  (setf (ena:$websocket-closed-by-client websocket) t)
  (websocket-close (ena:$websocket-ws websocket)))


(provide 'ena-websocket)

;;; ena-websocket.el ends here

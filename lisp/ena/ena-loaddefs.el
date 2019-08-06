;;; ena-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ena:connect-to-default-notebook ena:connect-buffer-to-notebook
;;;;;;  ena:connect-to-notebook-buffer ena:connect-to-notebook ena:connect-to-notebook-command)
;;;;;;  "ena-connect" "ena-connect.el" (20629 35398 551423 930000))
;;; Generated autoloads from ena-connect.el

(autoload 'ena:connect-to-notebook-command "ena-connect" "\
Connect to notebook.  When the prefix argument is given,
you can choose any notebook on your server including the ones
not yet opened.  Otherwise, already chose from already opened
notebooks.

\(fn &optional NOT-YET-OPENED)" t nil)

(autoload 'ena:connect-to-notebook "ena-connect" "\
Connect any buffer to notebook and its kernel.

\(fn NBPATH &optional BUFFER NO-RECONNECTION)" t nil)

(autoload 'ena:connect-to-notebook-buffer "ena-connect" "\
Connect any buffer to opened notebook and its kernel.

\(fn BUFFER-OR-NAME)" t nil)

(autoload 'ena:connect-buffer-to-notebook "ena-connect" "\
Connect BUFFER to NOTEBOOK.

\(fn NOTEBOOK &optional BUFFER NO-RECONNECTION)" nil nil)

(autoload 'ena:connect-to-default-notebook "ena-connect" "\
Connect to the default notebook specified by
`ena:connect-default-notebook'.  Set this to `python-mode-hook'
to automatically connect any python-mode buffer to the
notebook.

\(fn)" nil nil)

;;;***

;;;### (autoloads (ena:console-open) "ena-console" "ena-console.el"
;;;;;;  (20546 25932 437322 552000))
;;; Generated autoloads from ena-console.el

(autoload 'ena:console-open "ena-console" "\
Open IPython console.
To use this function, `ena:console-security-dir' and
`ena:console-args' must be set properly.
This function requires `Fabian Gallina's python.el`_ for now;
It should be possible to support python-mode.el.  Patches are welcome!

.. _`Fabian Gallina's python.el`: https://github.com/fgallina/python.el

\(fn)" t nil)

;;;***

;;;### (autoloads (ena:dev-bug-report-template ena:dev-stop-debug
;;;;;;  ena:dev-start-debug ena:dev-insert-mode-map) "ena-dev" "ena-dev.el"
;;;;;;  (20593 3171 499680 955000))
;;; Generated autoloads from ena-dev.el

(autoload 'ena:dev-insert-mode-map "ena-dev" "\
Insert mode-map into rst document.  For README.rst.

\(fn MAP-STRING)" nil nil)

(autoload 'ena:dev-start-debug "ena-dev" "\
Enable EIN debugging support.
When the prefix argument is given, debugging support for websocket
callback (`websocket-callback-debug-on-error') is enabled.

\(fn &optional WS-CALLBACK)" t nil)

(autoload 'ena:dev-stop-debug "ena-dev" "\
Disable debugging support enabled by `ena:dev-start-debug'.

\(fn)" t nil)

(autoload 'ena:dev-bug-report-template "ena-dev" "\
Open a buffer with bug report template.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-ena-notebook-buffers anything-ena-notebook-buffers
;;;;;;  helm-ena-kernel-history anything-ena-kernel-history) "ena-helm"
;;;;;;  "ena-helm.el" (20642 12065 658663 404000))
;;; Generated autoloads from ena-helm.el

(autoload 'anything-ena-kernel-history "ena-helm" "\
Search kernel execution history then insert the selected one.

\(fn)" t nil)

(autoload 'helm-ena-kernel-history "ena-helm" "\
Search kernel execution history then insert the selected one.

\(fn)" t nil)

(autoload 'anything-ena-notebook-buffers "ena-helm" "\
Choose opened notebook using anything.el interface.

\(fn)" t nil)

(autoload 'helm-ena-notebook-buffers "ena-helm" "\
Choose opened notebook using helm interface.

\(fn)" t nil)

;;;***

;;;### (autoloads (ena:iexec-mode) "ena-iexec" "ena-iexec.el" (20546
;;;;;;  25932 437322 552000))
;;; Generated autoloads from ena-iexec.el

(autoload 'ena:iexec-mode "ena-iexec" "\
Instant cell execution minor mode.
Code cell at point will be automatically executed after any
change in its input area.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ena:ipynb-mode) "ena-ipynb-mode" "ena-ipynb-mode.el"
;;;;;;  (20600 17207 518562 563000))
;;; Generated autoloads from ena-ipynb-mode.el

(autoload 'ena:ipynb-mode "ena-ipynb-mode" "\
A simple mode for ipynb file.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '(".*\\.ipynb\\'" . ena:ipynb-mode))

;;;***

;;;### (autoloads (ena:jedi-setup ena:jedi-dot-complete ena:jedi-complete)
;;;;;;  "ena-jedi" "ena-jedi.el" (20650 42078 657490 10000))
;;; Generated autoloads from ena-jedi.el

(autoload 'ena:jedi-complete "ena-jedi" "\
Run completion using candidates calculated by EIN and Jedi.

\(fn)" t nil)

(autoload 'ena:jedi-dot-complete "ena-jedi" "\
Insert \".\" and run `ena:jedi-complete'.

\(fn)" t nil)

(autoload 'ena:jedi-setup "ena-jedi" "\
Setup auto-completion using EIN and Jedi.el_ together.

Jedi.el_ is a Python auto-completion library for Emacs.
To use EIN and Jedi together, add the following in your Emacs setup.::

  (add-hook 'ena:connect-mode-hook 'ena:jedi-setup)

.. _Jedi.el: https://github.com/tkf/emacs-jedi

\(fn)" nil nil)

;;;***

;;;### (autoloads (ena:junk-rename ena:junk-new) "ena-junk" "ena-junk.el"
;;;;;;  (20571 14580 170606 716000))
;;; Generated autoloads from ena-junk.el

(autoload 'ena:junk-new "ena-junk" "\
Open a notebook to try random thing.
Notebook name is determined based on
`ena:junk-notebook-name-template'.

When prefix argument is given, it asks URL or port to use.

\(fn NAME URL-OR-PORT)" t nil)

(autoload 'ena:junk-rename "ena-junk" "\
Rename the current notebook based on `ena:junk-notebook-name-template'
and save it immediately.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil "ena-kernel" "ena-kernel.el" (20596 28965 566444
;;;;;;  588000))
;;; Generated autoloads from ena-kernel.el

(defalias 'ena:kernel-url-or-port 'ena:$kernel-url-or-port)

(defalias 'ena:kernel-id 'ena:$kernel-kernel-id)

;;;***

;;;### (autoloads (ena:notebook-multilang-mode) "ena-multilang" "ena-multilang.el"
;;;;;;  (20591 25156 462553 731000))
;;; Generated autoloads from ena-multilang.el

(autoload 'ena:notebook-multilang-mode "ena-multilang" "\
Notebook mode with multiple language fontification.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ena-notebook" "ena-notebook.el" (20604 29927
;;;;;;  731865 215000))
;;; Generated autoloads from ena-notebook.el

(defalias 'ena:notebook-name 'ena:$notebook-notebook-name)

;;;***

;;;### (autoloads (ena:notebooklist-load ena:notebooklist-open-notebook-global
;;;;;;  ena:notebooklist-list-notebooks ena:notebooklist-new-notebook-with-name
;;;;;;  ena:notebooklist-new-notebook ena:notebooklist-reload ena:notebooklist-open)
;;;;;;  "ena-notebooklist" "ena-notebooklist.el" (20604 21315 671448
;;;;;;  417000))
;;; Generated autoloads from ena-notebooklist.el

(autoload 'ena:notebooklist-open "ena-notebooklist" "\
Open notebook list buffer.

\(fn &optional URL-OR-PORT NO-POPUP)" t nil)

(autoload 'ena:notebooklist-reload "ena-notebooklist" "\
Reload current Notebook list.

\(fn)" t nil)

(autoload 'ena:notebooklist-new-notebook "ena-notebooklist" "\
Ask server to create a new notebook and open it in a new buffer.

\(fn &optional URL-OR-PORT CALLBACK CBARGS)" t nil)

(autoload 'ena:notebooklist-new-notebook-with-name "ena-notebooklist" "\
Open new notebook and rename the notebook.

\(fn NAME &optional URL-OR-PORT)" t nil)

(autoload 'ena:notebooklist-list-notebooks "ena-notebooklist" "\
Return a list of notebook path (NBPATH).  Each element NBPATH
is a string of the format \"URL-OR-PORT/NOTEBOOK-NAME\".

\(fn)" nil nil)

(autoload 'ena:notebooklist-open-notebook-global "ena-notebooklist" "\
Choose notebook from all opened notebook list and open it.
Notebook is specified by a string NBPATH whose format is
\"URL-OR-PORT/NOTEBOOK-NAME\".

When used in lisp, CALLBACK and CBARGS are passed to `ena:notebook-open'.

\(fn NBPATH &optional CALLBACK CBARGS)" t nil)

(autoload 'ena:notebooklist-load "ena-notebooklist" "\
Load notebook list but do not pop-up the notebook list buffer.

For example, if you want to load notebook list when Emacs starts,
add this in the Emacs initialization file::

  (add-to-hook 'after-init-hook 'ena:notebooklist-load)

or even this (if you want fast Emacs start-up)::

  ;; load notebook list if Emacs is idle for 3 sec after start-up
  (run-with-idle-timer 3 nil #'ena:notebooklist-load)

You should setup `ena:url-or-port' or `ena:default-url-or-port'
in order to make this code work.

See also:
`ena:connect-to-default-notebook', `ena:connect-default-notebook'.

\(fn &optional URL-OR-PORT)" nil nil)

;;;***

;;;### (autoloads (ena:org-store-link ena:org-open) "ena-org" "ena-org.el"
;;;;;;  (20604 29927 731865 215000))
;;; Generated autoloads from ena-org.el

(autoload 'ena:org-open "ena-org" "\
Open IPython notebook specified by LINK-PATH.
This function is to be used for FOLLOW function of
`org-add-link-type'.

\(fn LINK-PATH)" nil nil)

(autoload 'ena:org-store-link "ena-org" "\
Call `org-store-link-props' when in notebook buffer.
This function is to be used for `org-store-link-functions'.

Examples::

  ipynb:(:url-or-port 8888 :name \"My_Notebook\")
  ipynb:(:url-or-port \"http://notebook-server\" :name \"My_Notebook\")

Note that spaces will be escaped in org files.

As how IPython development team supports multiple directory in
IPython notebook server is unclear, it is not easy to decide the
format for notebook links.  Current approach is to use
S-expression based (rather verbose) serialization, so that
extending link spec without loosing backward compatibility is
easier.  For the examples of link format in general, see Info
node `(org) External links' and Info node `(org) Search options'

\(fn)" nil nil)

(eval-after-load "org" '(progn (org-add-link-type "ipynb" 'ena:org-open) (add-hook 'org-store-link-functions 'ena:org-store-link)))

;;;***

;;;### (autoloads (ena:pseudo-console-mode) "ena-pseudo-console"
;;;;;;  "ena-pseudo-console.el" (20546 25932 441322 552000))
;;; Generated autoloads from ena-pseudo-console.el

(autoload 'ena:pseudo-console-mode "ena-pseudo-console" "\
Pseudo console mode.  Hit RET to execute code.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ena:shared-output-eval-string ena:shared-output-show-code-cell-at-point
;;;;;;  ena:shared-output-pop-to-buffer) "ena-shared-output" "ena-shared-output.el"
;;;;;;  (20629 35398 555423 930000))
;;; Generated autoloads from ena-shared-output.el

(autoload 'ena:shared-output-pop-to-buffer "ena-shared-output" "\
Open shared output buffer.

\(fn)" t nil)

(autoload 'ena:shared-output-show-code-cell-at-point "ena-shared-output" "\
Show code cell at point in shared-output buffer.
It is useful when the output of the cell at point is truncated.
See also `ena:cell-max-num-outputs'.

\(fn)" t nil)

(autoload 'ena:shared-output-eval-string "ena-shared-output" "\
Evaluate a piece of code.  Prompt will appear asking the code to run.
This is handy when you want to execute something quickly without
making a cell.  If the code outputs something, it will go to the
shared output buffer.  You can open the buffer by the command
`ena:shared-output-pop-to-buffer'.

.. ARGS is passed to `ena:kernel-execute'.  Unlike `ena:kernel-execute',
   `:silent' is `nil' by default.

\(fn CODE &optional POPUP VERBOSE KERNEL &rest ARGS)" t nil)

;;;***

;;;### (autoloads (ena:tb-show) "ena-traceback" "ena-traceback.el"
;;;;;;  (20546 25932 441322 552000))
;;; Generated autoloads from ena-traceback.el

(autoload 'ena:tb-show "ena-traceback" "\
Show full traceback in traceback viewer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("debug-ena.el" "ena-ac.el" "ena-cell.el"
;;;;;;  "ena-completer.el" "ena-core.el" "ena-events.el" "ena-kernelinfo.el"
;;;;;;  "ena-kill-ring.el" "ena-log.el" "ena-multilang-fontify.el"
;;;;;;  "ena-mumamo.el" "ena-node.el" "ena-notification.el" "ena-output-area.el"
;;;;;;  "ena-pager.el" "ena-pkg.el" "ena-python.el" "ena-pytools.el"
;;;;;;  "ena-query.el" "ena-scratchsheet.el" "ena-smartrep.el" "ena-subpackages.el"
;;;;;;  "ena-utils.el" "ena-websocket.el" "ena-worksheet.el" "ena.el"
;;;;;;  "zeroena.el") (20650 45637 307359 381000))

;;;***

(provide 'ena-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ena-loaddefs.el ends here

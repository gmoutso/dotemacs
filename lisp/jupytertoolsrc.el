;;; jupytertoolsrc.el --- Helm interface for Jupyter kernels and REPLs -*- lexical-binding: t -*-

;;; Commentary:
;; Provides Helm sources for managing Jupyter kernels, REPLs, and Python workspace inspection.

;;; Code:

(require 'cl-lib)

;;
;; Listing kernelspecs, kernels and repls.
;;

(defun gm/jupyter-kernels--list-kernelspecs ()
  "Get alist of kernelspec display names to spec objects."
  (let* ((server (jupyter-current-server))
         (specs (jupyter-kernelspecs server)))
    (cl-loop for spec in specs
             collect (cons (plist-get (jupyter-kernelspec-plist spec) :display_name)
                           spec))))

  (defun gm/jupyter-kernels--list-kernels ()
    "Get alist of live kernel names to kernel IDs.
Queries the current server for running kernels and formats them for display."
    (with-helm-current-buffer
      (let* ((server (jupyter-current-server))
             (kernels (jupyter-api-get-kernel server)))
        (cl-loop
         with names = nil
         for kernel across kernels
         collect
         (cl-destructuring-bind
             (&key name id last_activity execution_state
                   connections &allow-other-keys)
             kernel
           (let* ((time (jupyter-decode-time last_activity))
                  (name (or (jupyter-server-kernel-name server id)
                            (let ((same (cl-remove-if-not
                                         (lambda (x) (string-prefix-p name x)) names)))
                              (when same
                                (setq name (format "%s<%d>" name (length same))))
                              (push name names)
                              name)))
                  (activity (jupyter-format-time-low-res time))
                  (conns (number-to-string connections))
                  (state execution_state)
                  (info (propertize (format "(%s, %s, %s connections)" state activity conns)
                                    'face 'shadow)))
             (cons (format "%-30s id:%s %s" name (propertize id 'face 'fixed-pitch) info)
                   id)))))))

(defun gm/jupyter-kernels--get-client-kernel-name (buffer)
  "Get the kernel name or ID for BUFFER's REPL client.
Returns the human-readable name if available, otherwise the kernel ID,
or \"unknown\" if neither can be determined."
  (let* ((server (jupyter-current-server))
         (client (with-current-buffer buffer jupyter-current-client))
         (id (and client
                  (jupyter-kernel-action client
                    (lambda (kernel)
                      (pcase-let (((cl-struct jupyter-server-kernel server id) kernel))
                        id))))))
    (or (jupyter-server-kernel-name server id) id)))


(defun gm/jupyter-kernels--format-repl-buffer (buffer)
  "Format BUFFER for display in Helm REPL list."
  (format "%-25s %s" (buffer-name buffer)
          (propertize (gm/jupyter-kernels--get-client-kernel-name buffer)
                      'face 'fixed-pitch)))

(defun gm/jupyter-kernels--list-repls ()
  "Get alist of live REPL buffers for Helm."
  (let ((buffers (jupyter-repl-available-repl-buffers)))
      (mapcar (lambda (b) (cons (gm/jupyter-kernels--format-repl-buffer b) b)) buffers)))

;;
;; specs, kernels and repl ACTIONS
;;

(defun gm/jupyter-kernels--pop-to-repl-and-associate (buffer)
  "Pop to REPL BUFFER and optionally associate it."
  (let ((client (buffer-local-value 'jupyter-current-client buffer)))
    (when (and (eq (jupyter-kernel-language-mode client) major-mode)
               (y-or-n-p "Associate with current buffer? "))
      (jupyter-repl-associate-buffer client))
    (pop-to-buffer buffer)))


(defun gm/jupyter-kernels--create-repl (id &optional guess-name default-name no-ask-associate)
  "Create a REPL for existing kernel with ID.
If GUESS-NAME is non-nil, do not prompt for REPL name.
DEFAULT-NAME is the default name to use if guessing.
If NO-ASK-ASSOCIATE is non-nil, do not associate."
  (let* ((ask-name (not guess-name))
	 (server (jupyter-current-server))
         (buffername (buffer-name))
         (name/byserver (plist-get (jupyter-api-get-kernel server id) :name))
         (name/bykernelnames (jupyter-server-kernel-name server id))
         (default-name (or default-name name/bykernelnames
                           (file-name-sans-extension buffername) name/byserver))
         (associate (or no-ask-associate (y-or-n-p "Associate with buffer? ")))
         (replname (if ask-name
                       (read-string (format "REPL Name (%s): " default-name)
                                    nil nil default-name)
                     default-name)))
    (jupyter-connect-server-repl server id replname associate nil t)))


(defun gm/jupyter-kernels--create-kernel-and-repl (spec)
  "Create kernel based on SPEC, create REPL, name both and associate."
  (let* ((server (jupyter-current-server))
         (replname (read-string "Kernel/REPL name: " (buffer-name)))
         (associate (y-or-n-p "Associate with buffer? ")))
    (jupyter-run-server-repl server spec replname associate nil t)
    (jupyter-server-name-client-kernel jupyter-current-client replname)))


(defun gm/jupyter-kernels--ensure-server ()
  "Get and authenticate current Jupyter server.
Uses `current-prefix-arg' to determine whether to prompt for server."
  (let ((server (jupyter-current-server current-prefix-arg)))
    (jupyter-api-ensure-authenticated server)
    server))

;;
;; vertico/consult switching
;;

(defun gm/jupyter-kernels-new-kernel ()
  (interactive)
  (gm/jupyter-kernels--ensure-server)
  (let* ((specs (gm/jupyter-kernels--list-kernelspecs))
	 (spec-names (mapcar #'car specs))
	 (spec-name (completing-read "Select kernel spec: " spec-names nil t))
	 (spec (cdr (assoc spec-name specs))))
    (gm/jupyter-kernels--create-kernel-and-repl spec)))

(defun gm/jupyter-kernels-pop-to-buffer ()
  (interactive)
  (gm/jupyter-kernels--ensure-server)
  (let* ((repls (gm/jupyter-kernels--list-repls))
	 (repl-names (mapcar #'car repls)))
    (when repl-names
      (let* ((repl-name (completing-read "Select REPL: " repl-names nil t))
	     (buffer (cdr (assoc repl-name repls))))
	(gm/jupyter-kernels--pop-to-repl-and-associate buffer)))))

(defun gm/jupyter-kernels-new-repl ()
  (interactive)
  (gm/jupyter-kernels--ensure-server)
  (let* ((kernels (gm/jupyter-kernels--list-kernels))
	 (kernel-names (mapcar #'car kernels)))
    (if kernel-names 
	(let* ((kernel-name (completing-read "Select kernel: " kernel-names nil t))
	       (kernel-id (cdr (assoc kernel-name kernels))))
	  (gm/jupyter-kernels--create-repl kernel-id))
      (message "No running kernels found. Please start a kernel first."))))


;;
;; Consult integration
;;

(with-eval-after-load 'consult
  (defvar gm/consult--source-jupyter-repls
    `(:name "REPLs"
      :narrow ?r
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :items ,(lambda ()
                (mapcar (lambda (pair)
                          (propertize (car pair) 'consult--candidate (cdr pair)))
                        (gm/jupyter-kernels--list-repls))))
    "Consult source for Jupyter REPL buffers.")

  (defvar gm/consult--source-jupyter-specs
    `(:name "Kernel Specs"
      :narrow ?s
      :category jupyter-spec
      :items ,(lambda ()
                (mapcar #'car (gm/jupyter-kernels--list-kernelspecs))))
    "Consult source for Jupyter kernel specs.")

  (defvar gm/consult--source-jupyter-kernels
    `(:name "Live Kernels"
      :narrow ?k
      :category jupyter-kernel
      :items ,(lambda ()
                (mapcar #'car (gm/jupyter-kernels--list-kernels))))
    "Consult source for live Jupyter kernels.")

  (defun gm/jupyter-kernels-consult ()
    "Launch consult interface for Jupyter kernels and REPLs.
Offers three options:
1. Switch to an existing REPL (narrow: r)
2. Launch a new kernel from a kernelspec (narrow: s)
3. Connect to an existing running kernel (narrow: k)"
    (interactive)
    (gm/jupyter-kernels--ensure-server)
    (let* ((selected (consult--multi
                      '(gm/consult--source-jupyter-repls
                        gm/consult--source-jupyter-specs
                        gm/consult--source-jupyter-kernels)
                      :prompt "Jupyter: "
                      :require-match t
                      :sort nil))
           (source (car selected))
           (candidate (cdr selected)))
      (cond
       ;; REPL buffer selected
       ((eq source 'gm/consult--source-jupyter-repls)
        (let ((buffer (get-text-property 0 'consult--candidate candidate)))
          (gm/jupyter-kernels--pop-to-repl-and-associate buffer)))
       ;; Kernel spec selected
       ((eq source 'gm/consult--source-jupyter-specs)
        (let* ((specs (gm/jupyter-kernels--list-kernelspecs))
               (spec (cdr (assoc candidate specs))))
          (gm/jupyter-kernels--create-kernel-and-repl spec)))
       ;; Live kernel selected
       ((eq source 'gm/consult--source-jupyter-kernels)
        (let* ((kernels (gm/jupyter-kernels--list-kernels))
               (kernel-id (cdr (assoc candidate kernels))))
          (gm/jupyter-kernels--create-repl kernel-id)))))))

;;
;; Helm integration
;;

(with-eval-after-load 'helm
  ;; Helm-specific helper functions

  ;; (defun gm/jupyter-kernels--list-kernelspecs-helm-candidates ()
  ;;   "Get kernelspec candidates for Helm from current buffer's server."
  ;;   (with-helm-current-buffer
  ;;     (gm/jupyter-kernels--list-kernelspecs)))

  ;; (defun gm/jupyter-kernels--list-repls ()
  ;;   (with-helm-current-buffer
  ;;     (gm/jupyter-kernels--list-repls)))
  
  ;; Helm sources
  
  (defvar gm/helm-source-jupyter-server-kernel-list
    (helm-build-sync-source "Live Kernels"
      :candidates 'gm/jupyter-kernels--list-kernels
      :action 'gm/jupyter-kernels--create-repl)
    "Helm source for listing live Jupyter kernels.")

  (defvar gm/helm-source-jupyter-server-spec-list
    (helm-build-sync-source "Kernel Specs"
      :candidates 'gm/jupyter-kernels--list-kernelspecs
      :action 'gm/jupyter-kernels--create-kernel-and-repl)
    "Helm source for launching new kernels from specs.")

  (defvar gm/helm-source-jupyter-server-repl-list
    (helm-build-sync-source "REPLs"
      :candidates 'gm/jupyter-kernels--list-repls
      :action 'gm/jupyter-kernels--pop-to-repl-and-associate)
    "Helm source for listing active REPL buffers.")

  ;; Interactive commands
  
  (defun gm/jupyter-kernels ()
    "Launch Helm interface for Jupyter kernels and REPLs.
Offers three options:
1. Switch to an existing REPL
2. Launch a new kernel from a kernelspec
3. Connect to an existing running kernel"
    (interactive)
    (gm/jupyter-kernels--ensure-server)
    (helm :sources '(gm/helm-source-jupyter-server-repl-list
                     gm/helm-source-jupyter-server-spec-list
                     gm/helm-source-jupyter-server-kernel-list)))

  (defun gm/jupyter-whos ()
    "Display Python workspace variables using Helm.
Shows variables from %whos magic command with actions to insert,
view definition, or display with %page."
    (interactive)
    (let ((data (gm/jupyter-repl-python-whos-trimmed)))
      (helm :sources (helm-build-in-buffer-source "Python Workspace"
                       :data data
                       :display-to-real (lambda (line) (car (split-string line)))
                       :action '(("Insert" . insert)
                                 ("Definition" . gm/org-find-definition)
                                 ("Show" . gm/jupyter-page-object)))))))

;;
;; Python workspace inspection
;;

(defun gm/jupyter-repl-python-whos ()
  "Get Python workspace variables using %whos magic."
  (let* ((jupyter-current-client (or jupyter-current-client
                                     (jupyter-org-with-src-block-client jupyter-current-client)))
         (code "import io\nfrom contextlib import redirect_stdout\nf = io.StringIO()\nwith redirect_stdout(f):\n    %whos\nf.getvalue()")
         (value (jupyter-eval code)))
    (read (princ value))))

(defun gm/jupyter-repl-python-whos-trimmed ()
  "Get Python workspace variables with header lines removed."
  (let ((value (gm/jupyter-repl-python-whos)))
    (with-temp-buffer
      (insert value)
      (goto-char (point-min))
      (kill-whole-line 2)
      (buffer-string))))

(defun gm/jupyter-page-object (strobject)
  "Display object STROBJECT using Jupyter's %page magic."
  (jupyter-eval-string (format "%%page %s" strobject)))

;;
;; Super function that chooses between Helm and Consult
;;

(defun gm/jupyter-kernels-interface ()
  "Launch Jupyter kernels interface using Helm or Consult.
Automatically detects which completion framework is available,
preferring Helm if both are loaded."
  (interactive)
  (cond
   ((featurep 'helm)
    (gm/jupyter-kernels))
   ((featurep 'consult)
    (gm/jupyter-kernels-consult))
   (t
    (user-error "Neither Helm nor Consult is loaded. Please load one of them first"))))

(provide 'jupytertoolsrc)
;;; jupytertoolsrc.el ends here


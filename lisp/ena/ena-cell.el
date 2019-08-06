;;; ena-cell.el --- Cell module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-cell.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-cell.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-cell.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Implementation note.  Current implementation of cell has redundant
;;  and not-guaranteed-to-be consistent information: `element' and
;;  `ena:$node'.  This part must be moved to ena-node.el module to
;;  make it well capsuled.

;; IPython has cell.js, codecell.js and textcell.js.
;; But let's start with one file.

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ansi-color)
(require 'comint)

(require 'ena-core)
(require 'ena-log)
(require 'ena-node)
(require 'ena-kernel)
(require 'ena-output-area)


;;; Faces

(defface ena:cell-input-prompt
  '((t :inherit header-line))
  "Face for cell input prompt"
  :group 'ena)

(defface ena:cell-input-area
  '((((class color) (background light))
     :background "honeydew1")
    (((class color) (background dark))
     :background "#383838"))
  "Face for cell input area"
  :group 'ena)

(defface ena:cell-heading-1
  '((t :height 1.1 :inherit ena:cell-heading-2))
  "Face for level 1 heading."
  :group 'ena)

(defface ena:cell-heading-2
  '((t :height 1.1 :inherit ena:cell-heading-3))
  "Face for level 2 heading."
  :group 'ena)

(defface ena:cell-heading-3
  '((t :height 1.1 :inherit ena:cell-heading-4))
  "Face for level 3 heading."
  :group 'ena)

(defface ena:cell-heading-4
  '((t :height 1.1 :inherit ena:cell-heading-5))
  "Face for level 4 heading."
  :group 'ena)

(defface ena:cell-heading-5
  '((t :height 1.1 :inherit ena:cell-heading-6))
  "Face for level 5 heading."
  :group 'ena)

(defface ena:cell-heading-6
  '((t :weight bold :inherit (variable-pitch ena:cell-input-area)))
  "Face for level 6 heading."
  :group 'ena)

(defface ena:cell-output-prompt
  '((t :inherit header-line))
  "Face for cell output prompt"
  :group 'ena)

(defface ena:cell-output-stderr
  '((((class color) (background light))
     :background "PeachPuff")
    (((class color) (background dark))
     :background "#8c5353"))
  "Face for stderr cell output"
  :group 'ena)

(defface ena:pos-tip-face
  '((t (:inherit 'popup-tip-face)))
  "Face for tooltip when using pos-tip backend."
  :group 'ena)


;;; Customization

(defcustom ena:cell-traceback-level 1
  "Number of traceback stack to show.
Hidden tracebacks are not discarded.  You can always view them
using the command `ena:notebook-view-traceback'."
  :type '(choice (integer :tag "Number of stack to show" 1)
                 (const :tag "Show all traceback" nil))
  :group 'ena)

(defcustom ena:cell-max-num-outputs nil
  "Number of maximum outputs to be shown by default.
To view full output, use `ena:notebook-show-in-shared-output'."
  :type '(choice (integer :tag "Number of outputs to show" 5)
                 (const :tag "Show all traceback" nil))
  :group 'ena)

(defcustom ena:cell-autoexec-prompt "⚡"
  "String shown in the cell prompt when the auto-execution flag
is on.  See also `ena:connect-aotoexec-lighter'."
  :type 'string
  :group 'ena)

(defcustom ena:slice-image nil
  "[EXPERIMENTAL] When non-`nil', use `insert-sliced-image' when
drawing images.  If it is of the form of ``(ROWS COLS)``, it is
passed to the corresponding arguments of `insert-sliced-image'.

.. FIXME: ROWS and COLS must be determined dynamically by measuring
   the size of iamge and Emacs window.

See also: https://github.com/tkf/emacs-ipython-notebook/issues/94"
  :type 'boolean
  :group 'ena)



;;; EIEIO related utils

(defmacro ena:oset-if-empty (obj slot value)
  `(unless (and (slot-boundp ,obj ,slot) (oref ,obj ,slot))
     (oset ,obj ,slot ,value)))

(defmacro ena:oref-safe (obj slot)
  `(when (slot-boundp ,obj ,slot)
     (oref ,obj ,slot)))


;;; Utils
(defun ena:insert-image (&rest args)
  (let ((img (apply #'create-image args)))
    (if ena:slice-image
        (destructuring-bind (&optional rows cols)
            (when (listp ena:slice-image) ena:slice-image)
          (insert-sliced-image img nil nil (or rows 20) cols))
      (insert-image img))))


;;; Cell classes

(defclass ena:basecell ()
  ((cell-type :initarg :cell-type :type string)
   (read-only :initarg :read-only :initform nil :type boolean)
   (ewoc :initarg :ewoc :type ewoc)
   (element :initarg :element :initform nil :type list
    :documentation "ewoc nodes")
   (element-names :initarg :element-names)
   (input :initarg :input :type string
    :documentation "Place to hold data until it is rendered via `ewoc'.")
   (outputs :initarg :outputs :initform nil :type list)
   (events :initarg :events :type ena:events)
   (cell-id :initarg :cell-id :initform (ena:utils-uuid) :type string))
  "Notebook cell base class")

(defclass ena:codecell (ena:basecell)
  ((cell-type :initarg :cell-type :initform "code")
   (kernel :initarg :kernel :type ena:$kernel)
   (element-names :initform (:prompt :input :output :footer))
   (input-prompt-number :initarg :input-prompt-number
                        :documentation "\
Integer or \"*\" (running state).
Implementation note:
Typed `:input-prompt-number' becomes a problem when reading a
notebook that saved "*".  So don't add `:type'!")
   (collapsed :initarg :collapsed :initform nil :type boolean)
   (running :initarg :running :initform nil :type boolean)
   (dynamic :initarg :dynamic :initform nil :type boolean
            :documentation "\
Whether cell output is evaluated dynamically or not.

Only Emacs lisp type output data will be affected by this
slot (Javascript will not be evaluated).  This value must be set
to `t' when executing cell.  See `ena:notebook-execute-cell'.
In the implantation of IPython web client it is passed around via
argument, but since it is difficult to pass argument to EWOC
pretty printer, `ena:codecell' instance holds this setting in a
slot.")
   (autoexec :initarg :autoexec :initform nil :type boolean
             :documentation "Auto-execution flag.

This cell is executed when the connected buffer is saved,
provided that (1) this flag is `t' and (2) corresponding
auto-execution mode flag in the connected buffer is `t'.")))

(defclass ena:textcell (ena:basecell)
  ((cell-type :initarg :cell-type :initform "text")
   (element-names :initform (:prompt :input :footer))))

(defclass ena:htmlcell (ena:textcell)
  ((cell-type :initarg :cell-type :initform "html")))

(defclass ena:markdowncell (ena:textcell)
  ((cell-type :initarg :cell-type :initform "markdown")))

(defclass ena:rawcell (ena:textcell)
  ((cell-type :initarg :cell-type :initform "raw")))

(defclass ena:headingcell (ena:textcell)
  ((cell-type :initarg :cell-type :initform "heading")
   (level :initarg :level :initform 1)))


;;; Cell factory

(defun ena:cell-class-from-type (type)
  (ena:case-equal type
    (("code") 'ena:codecell)
    (("text") 'ena:textcell)
    (("html") 'ena:htmlcell)
    (("markdown") 'ena:markdowncell)
    (("raw") 'ena:rawcell)
    (("heading") 'ena:headingcell)
    ;; Defined in ena-shared-output.el:
    (("shared-output") 'ena:shared-output-cell)
    (t (error "No cell type called %S" type))))

(defun ena:cell-from-type (type &rest args)
  (apply (ena:cell-class-from-type type) "Cell" args))

(defun ena:cell-from-json (data &rest args)
  (ena:cell-init (apply #'ena:cell-from-type
                        (plist-get data :cell_type) args) data))

(defmethod ena:cell-init ((cell ena:codecell) data)
  (ena:oset-if-empty cell :outputs (plist-get data :outputs))
  (ena:oset-if-empty cell :input (plist-get data :input))
  (ena:aif (plist-get data :prompt_number)
      (ena:oset-if-empty cell :input-prompt-number it))
  (ena:oset-if-empty cell :collapsed
                     (let ((v (plist-get data :collapsed)))
                       (if (eql v json-false) nil v)))
  cell)

(defmethod ena:cell-init ((cell ena:textcell) data)
  (ena:aif (plist-get data :source)
      (oset cell :input it))
  cell)

(defmethod ena:cell-init ((cell ena:headingcell) data)
  (call-next-method)
  (ena:aif (plist-get data :level)
      (oset cell :level it))
  cell)

(defmethod ena:cell-convert ((cell ena:basecell) type)
  (let ((new (ena:cell-from-type type)))
    ;; copy attributes
    (loop for k in '(:read-only :ewoc)
          do (set-slot-value new k (slot-value cell k)))
    ;; copy input
    (oset new :input (if (ena:cell-active-p cell)
                         (ena:cell-get-text cell)
                       (oref cell :input)))
    ;; copy output when the new cell has it
    (when (memq :output (oref new :element-names))
      (oset new :outputs (mapcar 'identity (oref cell :outputs))))
    new))

(defmethod ena:cell-convert ((cell ena:codecell) type)
  (let ((new (call-next-method)))
    (when (and (ena:codecell-child-p new)
               (slot-boundp cell :kernel))
      (oset new :kernel (oref cell :kernel)))
    new))

(defmethod ena:cell-convert ((cell ena:headingcell) type)
  (let ((new (call-next-method)))
    (when (ena:headingcell-p new)
      (oset new :level (oref cell :level)))
    new))

(defmethod ena:cell-copy ((cell ena:basecell))
  (ena:cell-convert cell (oref cell :cell-type)))

(defmethod ena:cell-convert-inplace ((cell ena:basecell) type)
  "Convert CELL to TYPE and redraw corresponding ewoc nodes."
  (let ((new (ena:cell-convert cell type)))
    ;; copy element attribute
    (loop for k in (oref new :element-names)
          with old-element = (oref cell :element)
          do (oset new :element
                   (plist-put (oref new :element) k
                              (plist-get old-element k))))
    ;; setting ewoc nodes
    (loop for en in (ena:cell-all-element cell)
          for node = (ewoc-data en)
          do (setf (ena:$node-data node) new))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))         ; disable undo recording
      ;; delete ewoc nodes that is not copied
      (apply
       #'ewoc-delete (oref new :ewoc)
       (apply
        #'append
        (loop for name in (oref cell :element-names)
              unless (memq name (oref new :element-names))
              collect (let ((ens (ena:cell-element-get cell name)))
                        (if (listp ens) ens (list ens))))))
      ;; draw ewoc node
      (loop with ewoc = (oref new :ewoc)
            for en in (ena:cell-all-element new)
            do (ewoc-invalidate ewoc en)))
    new))

(defmethod ena:cell-change-level ((cell ena:headingcell) level)
  (assert (integerp level))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))         ; disable undo recording
    (oset cell :level level)
    ;; draw ewoc node
    (loop with ewoc = (oref cell :ewoc)
          for en in (ena:cell-all-element cell)
          do (ewoc-invalidate ewoc en))))


;;; Getter/setter

(defmethod ena:cell-num-outputs ((cell ena:codecell))
  (length (oref cell :outputs)))

(defmethod ena:cell-num-outputs ((cell ena:textcell))
  0)

(defmethod ena:cell-element-get ((cell ena:basecell) prop &rest args)
  "Return ewoc node named PROP in CELL.
If PROP is `:output' a list of ewoc nodes is returned.
A specific node can be specified using optional ARGS."
  (if (memq prop (oref cell :element-names))
      (plist-get (oref cell :element) prop)
    (error "PROP %s is not supported." prop)))

(defmethod ena:cell-element-get ((cell ena:codecell) prop &optional index)
  (let ((element (oref cell :element)))
    (if index
        (progn
          (assert (eql prop :output))
          (nth index (plist-get element prop)))
      (case prop
        (:after-input
         (ena:aif (nth 0 (plist-get element :output))
             it
           (plist-get element :footer)))
        (:after-output (plist-get element :footer))
        (:before-input (plist-get element :prompt))
        (:before-output (plist-get element :input))
        (:last-output
         (ena:aif (plist-get element :output)
             (car (last it))
           (plist-get element :input)))
        (t (call-next-method))))))

(defmethod ena:cell-element-get ((cell ena:textcell) prop)
  (let ((element (oref cell :element)))
    (case prop
      (:after-input (plist-get element :footer))
      (:before-input (plist-get element :prompt))
      (t (call-next-method)))))

(defmethod ena:cell-all-element ((cell ena:basecell))
  (list (ena:cell-element-get cell :prompt)
        (ena:cell-element-get cell :input)
        (ena:cell-element-get cell :footer)))

(defmethod ena:cell-all-element ((cell ena:codecell))
  (append (call-next-method)
          (ena:cell-element-get cell :output)))

(defmethod ena:cell-language ((cell ena:basecell))
  "Programming language used for CELL.
Return language name as a string or `nil' when not defined.

\(fn cell)")

(defmethod ena:cell-language ((cell ena:codecell)) nil "python")
(defmethod ena:cell-language ((cell ena:markdowncell)) nil "markdown")
(defmethod ena:cell-language ((cell ena:htmlcell)) nil "html")
(defmethod ena:cell-language ((cell ena:rawcell)) nil "rst")


;; EWOC

(defun ena:cell-make-element (make-node num-outputs)
  (let ((buffer-undo-list t))           ; disable undo recording
    (list
     :prompt (funcall make-node 'prompt)
     :input  (funcall make-node 'input)
     :output (loop for i from 0 below num-outputs
                   collect (funcall make-node 'output i))
     :footer (funcall make-node 'footer))))

(defmethod ena:cell-enter-last ((cell ena:basecell))
  (let* ((ewoc (oref cell :ewoc))
         ;; Use `cell' as data for ewoc.  Use the whole cell data even
         ;; if it is not used, to access it from the notebook buffer.
         ;; It is equivalent to `this.element.data("cell", this)' in
         ;; IPython.Cell (see cell.js).
         (make-node
          (lambda (&rest path)
            (ewoc-enter-last ewoc (ena:node-new `(cell ,@path) cell))))
         (element (ena:cell-make-element make-node
                                         (ena:cell-num-outputs cell))))
    (oset cell :element element)
    cell))

(defmethod ena:cell-enter-first ((cell ena:basecell))
  (let* ((ewoc (oref cell :ewoc))
         (node nil)
         (make-node
          (lambda (&rest path)
            (let ((ewoc-data (ena:node-new `(cell ,@path) cell)))
              (setq node
                    (if node
                        (ewoc-enter-after ewoc node ewoc-data)
                      (ewoc-enter-first ewoc ewoc-data))))))
         (element (ena:cell-make-element make-node
                                         (ena:cell-num-outputs cell))))
    (oset cell :element element)
    cell))

(defmethod ena:cell-insert-below ((base-cell ena:basecell) other-cell)
  (let* ((ewoc (oref base-cell :ewoc))
         (node (ena:cell-element-get base-cell :footer))
         (make-node
          (lambda (&rest path)
            (setq node (ewoc-enter-after
                        ewoc node (ena:node-new `(cell ,@path) other-cell)))))
         (element (ena:cell-make-element make-node
                                         (ena:cell-num-outputs other-cell))))
    (oset other-cell :element element)
    other-cell))

(defun ena:cell-pp (path data)
  (case (car path)
    (prompt (ena:cell-insert-prompt data))
    (input  (ena:cell-insert-input data))
    (output (ena:cell-insert-output (cadr path) data))
    (footer (ena:cell-insert-footer data))))

(defmethod ena:cell-insert-prompt ((cell ena:codecell))
  "Insert prompt of the CELL in the buffer.
Called from ewoc pretty printer via `ena:cell-pp'."
  ;; Newline is inserted in `ena:cell-insert-input'.
  (ena:insert-read-only
   (concat
    (format "In [%s]:" (or (ena:oref-safe cell :input-prompt-number)  " "))
    (when (oref cell :autoexec) " %s" ena:cell-autoexec-prompt))
   'font-lock-face 'ena:cell-input-prompt))

(defmethod ena:cell-insert-prompt ((cell ena:textcell))
  (ena:insert-read-only
   (format "%s:" (oref cell :cell-type))
   'font-lock-face 'ena:cell-input-prompt))

(defmethod ena:cell-insert-prompt ((cell ena:headingcell))
  (ena:insert-read-only
   (format "h%s:" (oref cell :level))
   'font-lock-face 'ena:cell-input-prompt))

(defmethod ena:cell-insert-input ((cell ena:basecell))
  "Insert input of the CELL in the buffer.
Called from ewoc pretty printer via `ena:cell-pp'."
  (let ((start (1+ (point))))
    ;; Newlines must allow insertion before/after its position.
    (insert (propertize "\n" 'read-only t 'rear-nonsticky t)
            (or (ena:oref-safe cell :input) "")
            (propertize "\n" 'read-only t))
    ;; Highlight background using overlay.
    (let ((ol (make-overlay start (point))))
      (overlay-put ol 'face (ena:cell-get-input-area-face cell))
      ;; `evaporate' = `t': Overlay is deleted when the region become empty.
      (overlay-put ol 'evaporate t))))

(defmethod ena:cell-get-input-area-face ((cell ena:basecell))
  "Return the face (symbol) for input area."
  'ena:cell-input-area)

(defmethod ena:cell-get-input-area-face ((cell ena:headingcell))
  (intern (format "ena:cell-heading-%d" (oref cell :level))))

(defun ena:cell-insert-output (index cell)
  "Insert INDEX-th output of the CELL in the buffer.
Called from ewoc pretty printer via `ena:cell-pp'."
  (if (or (oref cell :collapsed)
          (and ena:cell-max-num-outputs
               (>= index ena:cell-max-num-outputs)))
      (progn
        (when (and (not (oref cell :collapsed))
                   (= index ena:cell-max-num-outputs)
                   (> (point) (point-at-bol)))
          ;; The first output which exceeds `ena:cell-max-num-outputs'.
          (ena:insert-read-only "\n"))
        (ena:insert-read-only "."))
    (let ((out (nth index (oref cell :outputs))))
      ;; Handle newline for previous stream output.
      ;; In IPython JS, it is handled in `append_stream' because JS
      ;; does not need to care about newline (DOM does it for JS).
      ;; FIXME: Maybe I should abstract ewoc in some way and get rid
      ;;        of this.
      (let ((last-out (and (> index 0)
                           (nth (1- index) (oref cell :outputs)))))
        ;; If previous output is stream type, consider adding newline
        (when (and last-out
                   (equal (plist-get last-out :output_type) "stream"))
          ;; Check if the last output is from the same stream.
          ;; If so, do *NOT* insert newline, otherwise insert newline.
          (unless (and (equal (plist-get out :output_type) "stream")
                       (equal (plist-get out      :stream)
                              (plist-get last-out :stream)))
            (ena:cell-append-stream-text-fontified "\n" last-out))))
      ;; Finally insert real data
      (ena:case-equal (plist-get out :output_type)
        (("pyout")        (ena:cell-append-pyout        cell out))
        (("pyerr")        (ena:cell-append-pyerr        cell out))
        (("display_data") (ena:cell-append-display-data cell out))
        (("stream")       (ena:cell-append-stream       cell out))))))

(defmethod ena:cell-insert-footer ((cell ena:basecell))
  "Insert footer (just a new line) of the CELL in the buffer.
Called from ewoc pretty printer via `ena:cell-pp'."
  (ena:insert-read-only "\n"))

(defmethod ena:cell-insert-footer ((cell ena:codecell))
  (if (or (oref cell :collapsed)
          (and ena:cell-max-num-outputs
               (> (ena:cell-num-outputs cell) ena:cell-max-num-outputs)))
      ;; Add a newline after the last ".".
      (ena:insert-read-only "\n")
    (let ((last-out (car (last (oref cell :outputs)))))
      (when (equal (plist-get last-out :output_type) "stream")
        (ena:cell-append-stream-text-fontified "\n" last-out))))
  (call-next-method))


(defun ena:cell-node-p (node &optional element-name)
  (let* ((path (ena:$node-path node))
         (p0 (car path))
         (p1 (cadr path))
         (cell (ena:$node-path node)))
    (and cell (eql p0 'cell) (or (not element-name) (eql p1 element-name)))))

(defun ena:cell-ewoc-node-p (ewoc-node &optional element-name)
  (ena:cell-node-p (ewoc-data ewoc-node) element-name))

(defun ena:cell-from-ewoc-node (ewoc-node)
  (ena:aand ewoc-node (ewoc-data it) (ena:$node-data it)))

(defmethod ena:cell-input-pos-min ((cell ena:basecell))
  "Return editable minimum point in the input area of the CELL.
If the input area of the CELL does not exist, return `nil'"
  (let* ((input-node (ena:cell-element-get cell :input)))
    ;; 1+ for skipping newline
    (when input-node (1+ (ewoc-location input-node)))))

(defmethod ena:cell-input-pos-max ((cell ena:basecell))
  "Return editable maximum point in the input area of the CELL.
If the input area of the CELL does not exist, return `nil'"
  (let* ((ewoc (oref cell :ewoc))
         (input-node (ena:cell-element-get cell :input)))
    ;; 1- for skipping newline
    (when input-node (1- (ewoc-location (ewoc-next ewoc input-node))))))

(defmethod ena:cell-get-text ((cell ena:basecell))
  "Grab text in the input area of the cell at point."
  (if (ena:cell-active-p cell)
      (let* ((beg (ena:cell-input-pos-min cell))
             (end (ena:cell-input-pos-max cell)))
        (buffer-substring beg end))
    (oref cell :input)))

(defmethod ena:cell-set-text ((cell ena:basecell) text)
  (let* ((input-node (ena:cell-element-get cell :input))
         (ewoc (oref cell :ewoc))
           ;; 1+/1- is for skipping newline
         (beg (1+ (ewoc-location input-node)))
         (end (1- (ewoc-location (ewoc-next ewoc input-node)))))
    (save-excursion
      ;; probably it is better to set :input and update via ewoc?
      (goto-char beg)
      (delete-region beg end)
      (insert text))))

(defmethod ena:cell-save-text ((cell ena:basecell))
  (oset cell :input (ena:cell-get-text cell)))

(defmethod ena:cell-deactivate ((cell ena:basecell))
  (oset cell :element nil)
  cell)

(defmethod ena:cell-active-p ((cell ena:basecell))
  (oref cell :element))

(defmethod ena:cell-running-set ((cell ena:codecell) running)
  ;; FIXME: change the appearance of the cell
  (oset cell :running running))

(defmethod ena:cell-set-collapsed ((cell ena:codecell) collapsed)
  "Set `:collapsed' slot of CELL and invalidate output ewoc nodes."
  (unless (eq (oref cell :collapsed) collapsed)
    (oset cell :collapsed collapsed)
    (apply #'ewoc-invalidate
           (oref cell :ewoc)
           (ena:cell-element-get cell :output))))

(defmethod ena:cell-collapse ((cell ena:codecell))
  (ena:cell-set-collapsed cell t))

(defmethod ena:cell-expand ((cell ena:codecell))
  (ena:cell-set-collapsed cell nil))

(defmethod ena:cell-toggle-output ((cell ena:codecell))
  "Toggle `:collapsed' slot of CELL and invalidate output ewoc nodes."
  (ena:cell-set-collapsed cell (not (oref cell :collapsed))))

(defmethod ena:cell-invalidate-prompt ((cell ena:codecell))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))           ; disable undo recording
    (ewoc-invalidate (oref cell :ewoc)
                     (ena:cell-element-get cell :prompt))))

(defmethod ena:cell-set-input-prompt ((cell ena:codecell) &optional number)
  (oset cell :input-prompt-number number)
  (ena:cell-invalidate-prompt cell))

(defmethod ena:cell-set-autoexec ((cell ena:codecell) bool)
  "Set auto-execution flag of CELL to BOOL and invalidate the
prompt EWOC node."
  (oset cell :autoexec bool)
  (ena:cell-invalidate-prompt cell))

(defmethod ena:cell-autoexec-p ((cell ena:basecell))
  "Auto-execution flag set to CELL.
Return `nil' always for non-code cells."
  nil)

(defmethod ena:cell-autoexec-p ((cell ena:codecell))
  (oref cell :autoexec))

(defmethod ena:cell-toggle-autoexec ((cell ena:codecell))
  "Toggle auto-execution flag of CELL to BOOL and invalidate the
prompt EWOC node."
  (ena:cell-set-autoexec cell (not (ena:cell-autoexec-p cell))))

(defmethod ena:cell-goto ((cell ena:basecell) &optional relpos prop)
  "Go to the input area of the given CELL.
RELPOS is the position relative to the input area.  Default is 0.
PROP is a name of cell element.  Default is `:input'.

\(fn cell relpos prop)"
  (unless relpos (setq relpos 0))
  (unless prop (setq prop :input))
  (ewoc-goto-node (oref cell :ewoc) (ena:cell-element-get cell prop))
  (let ((offset (case prop
                  ((:input :before-output) 1)
                  (:after-input -1)
                  (t 0))))
    (forward-char (+ relpos offset))))

(defmethod ena:cell-relative-point ((cell ena:basecell) &optional pos)
  "Return the point relative to the input area of CELL.
If the position POS is not given, current point is considered."
  (unless pos (setq pos (point)))
  (- pos (1+ (ewoc-location (ena:cell-element-get cell :input)))))

(defmethod ena:cell-location ((cell ena:basecell) &optional elm end)
  "Return the starting location of CELL.
ELM is a name (keyword) of element that `ena:cell-element-get'
understands.  Note that you can't use `:output' since it returns
a list.  Use `:after-input' instead.
If END is non-`nil', return the location of next element."
  (unless elm (setq elm :prompt))
  (let ((element (oref cell :element)))
    (when end
      (setq elm (case elm
                  (:prompt :input)
                  (:input :after-input)
                  (:output :after-output)))
      (unless elm
        (setq cell (ena:cell-next cell))
        (setq elm :prompt)))
    (if cell
        (ewoc-location (ena:cell-element-get cell elm))
      (assert end)
      (point-max))))

(defmethod ena:cell-buffer ((cell ena:basecell))
  "Return a buffer associated by CELL (if any)."
  (ena:aand (ena:oref-safe cell :ewoc) (ewoc-buffer it)))


;; Data manipulation

(defmethod ena:cell-clear-output ((cell ena:codecell) stdout stderr other)
  ;; codecell.js in IPytohn implements it using timeout and callback.
  ;; As it is unclear why timeout is needed, just clear output
  ;; instantaneously for now.
  (ena:log 'debug "cell-clear-output stdout=%s stderr=%s other=%s"
           stdout stderr other)
  (let ((ewoc (oref cell :ewoc))
        (output-nodes (ena:cell-element-get cell :output)))
    (if (and stdout stderr other)
        (progn
          ;; clear all
          (let ((inhibit-read-only t)
                (buffer-undo-list t))   ; disable undo recording
            (apply #'ewoc-delete ewoc output-nodes))
          (plist-put (oref cell :element) :output nil)
          (oset cell :outputs nil))
      (let* ((ewoc-node-list
              (append
               (when stdout (ena:node-filter output-nodes :is 'output-stdout))
               (when stderr (ena:node-filter output-nodes :is 'output-stderr))
               (when stdout (ena:node-filter output-nodes
                                             :is 'output-subarea
                                             :not 'output-stderr
                                             :not 'output-stdout))))
             (indices
              (mapcar (lambda (n) (last (ena:$node-path (ewoc-data n))))
                      ewoc-node-list)))
        ;; remove from buffer
        (let ((inhibit-read-only t)
              (buffer-undo-list t))   ; disable undo recording
          (apply #'ewoc-delete ewoc ewoc-node-list))
        ;; remove from `:element'
        (let* ((element (oref cell :element))
               (old-output (plist-get element :output))
               (new-ouptut (ena:remove-by-index old-output indices)))
          (plist-put element :output new-ouptut))
        ;; remove cleared outputs from internal data
        (oset cell :outputs
              (ena:remove-by-index (oref cell :outputs) indices))))
    ;; Footer may have extra (possibly colored) newline due to the
    ;; last output type.  So invalidate it here.
    ;; See `ena:cell-insert-footer' (for codecell).
    (ewoc-invalidate ewoc (ena:cell-element-get cell :footer))))

(defun ena:cell-output-json-to-class (json)
  (ena:case-equal (plist-get json :output_type)
    (("pyout")
     '(output-subarea))
    (("pyerr")
     '(output-subarea))
    (("display_data")
     '(output-subarea))
    (("stream")
     (list 'output-stream 'output-subarea
           (intern (format "output-%s" (plist-get json :stream)))))))

(defmethod ena:cell-append-output ((cell ena:codecell) json dynamic)
  (ena:cell-expand cell)
  ;; (ena:flush-clear-timeout)
  (oset cell :outputs
        (append (oref cell :outputs) (list json)))
  ;; enter last output element
  (let* ((inhibit-read-only t)
         (buffer-undo-list t)           ; disable undo recording
         (ewoc (oref cell :ewoc))
         (index (1- (ena:cell-num-outputs cell)))
         (path `(cell output ,index))
         (class (ena:cell-output-json-to-class json))
         (data (ena:node-new path cell class))
         (last-node (ena:cell-element-get cell :last-output))
         (ewoc-node (ewoc-enter-after ewoc last-node data))
         (element (oref cell :element)))
    (plist-put element :output
               (append (plist-get element :output) (list ewoc-node)))
    (ewoc-invalidate ewoc (ena:cell-element-get cell :footer))))

(defmethod ena:cell-append-pyout ((cell ena:codecell) json)
  "Insert pyout type output in the buffer.
Called from ewoc pretty printer via `ena:cell-insert-output'."
  (ena:insert-read-only (format "Out [%s]:"
                                (or (plist-get json :prompt_number) " "))
                        'font-lock-face 'ena:cell-output-prompt)
  (ena:insert-read-only "\n")
  (ena:cell-append-mime-type json (oref cell :dynamic))
  (ena:insert-read-only "\n"))

(defmethod ena:cell-append-pyerr ((cell ena:codecell) json)
  "Insert pyerr type output in the buffer.
Called from ewoc pretty printer via `ena:cell-insert-output'."
  (mapc (lambda (tb)
          (ena:cell-append-text tb)
          (ena:cell-append-text "\n"))
        (let ((tb (plist-get json :traceback))
              (level ena:cell-traceback-level))
          (if (and level (> (- (length tb) 2) level))
              (cons "\nTruncated Traceback (Use C-c C-x to view full TB):"
                    (last tb (1+ level)))
            tb)))
  (ena:insert-read-only "\n"))

(ena:deflocal ena:%cell-append-stream-last-cell% nil
  "The last cell in which `ena:cell-append-stream' is used.")

(defmethod ena:cell-append-stream ((cell ena:codecell) json)
  "Insert stream type output in the buffer.
Called from ewoc pretty printer via `ena:cell-insert-output'."
  (unless (plist-get json :stream)
    (plist-put json :stream "stdout"))
  (unless (eq cell ena:%cell-append-stream-last-cell%)
    ;; Avoid applying unclosed ANSI escape code in the cell.  Note
    ;; that I don't need to distinguish stdout/stderr because it looks
    ;; like normal terminal does not.
    (setq ansi-color-context nil))
  (let ((start (point)))
    (ena:cell-append-stream-text-fontified (plist-get json :text) json)
    (comint-carriage-motion start (point)))
  ;; NOTE: newlines for stream is handled in `ena:cell-insert-output'.
  ;; So do not insert newline here.
  (setq ena:%cell-append-stream-last-cell% cell))

(defun ena:cell-append-stream-text-fontified (text json)
  "Insert TEXT with font properties defined by JSON data."
  (if (equal (plist-get json :stream) "stderr")
      (ena:cell-append-text text 'font-lock-face 'ena:cell-output-stderr)
    (ena:cell-append-text text)))

(defmethod ena:cell-append-display-data ((cell ena:codecell) json)
  "Insert display-data type output in the buffer.
Called from ewoc pretty printer via `ena:cell-insert-output'."
  (ena:cell-append-mime-type json (oref cell :dynamic))
  (ena:insert-read-only "\n"))

(defcustom ena:output-type-preference
  (if (and (fboundp 'shr-insert-document)
           (fboundp 'libxml-parse-xml-region))
      #'ena:output-type-prefer-pretty-text-over-html
    '(emacs-lisp svg png jpeg text html latex javascript))
  "Output types to be used in notebook.
First output-type found in this list will be used.
This variable can be a list or a function returning a list given
DATA plist.
See also `ena:output-type-prefer-pretty-text-over-html'.

**Example**:
If you prefer HTML type over text type, you can set it as::

    (setq ena:output-type-preference
          '(emacs-lisp svg png jpeg html text latex javascript))

Note that ``html`` comes before ``text``."
  :group 'ena)

(defun ena:output-type-prefer-pretty-text-over-html (data)
  "Use text type if it is a \"prettified\" text instead of HTML.
This is mostly for *not* using HTML table for pandas but using
HTML for other object.

If the text type output contains a newline, it is assumed be a
prettified text thus be used instead of HTML type."
  (if (ena:aand (plist-get data :text) (string-match-p "\n" it))
      '(emacs-lisp svg png jpeg text html latex javascript)
    '(emacs-lisp svg png jpeg html text latex javascript)))

(defun ena:cell-append-mime-type (json dynamic)
  (loop
   for key in (cond
               ((functionp ena:output-type-preference)
                (funcall ena:output-type-preference json))
               (t ena:output-type-preference))
   for type = (intern (format ":%s" key)) ; something like `:text'
   for value = (plist-get json type)      ; FIXME: optimize
   when (plist-member json type)
   return
   (case key
     ;; NOTE: Normally `javascript' and `html' will not be inserted as
     ;; they come out after `text'.  Maybe it is better to inform user
     ;; when one of them is inserted.
     (javascript
      (when dynamic
        (ena:log 'info (concat "ena:cell-append-mime-type does not support "
                               "dynamic javascript. got: %s") value))
      (ena:insert-read-only (plist-get json type)))
     (emacs-lisp
      (when dynamic
        (ena:cell-safe-read-eval-insert (plist-get json type))))
     (html
      (funcall (ena:output-area-get-html-renderer) (plist-get json type)))
     ((latex text)
      (ena:insert-read-only (plist-get json type)))
     (svg
      (ena:insert-image value key t))
     ((png jpeg)
      (ena:insert-image (base64-decode-string value) key t)))))

(defun ena:cell-append-text (data &rest properties)
  ;; escape ANSI in plaintext:
  (apply #'ena:insert-read-only (ansi-color-apply data) properties))

(defun ena:cell-safe-read-eval-insert (text)
  (ena:insert-read-only
   (condition-case err
       (save-excursion
         ;; given code can be `pop-to-buffer' or something.
         (format "%S" (eval (read text))))
     (error
      (ena:log 'warn "Got an error while executing: '%s'"
               text)
      (format "Error: %S" err)))))

(defmethod ena:cell-to-json ((cell ena:codecell) &optional discard-output)
  "Return json-ready alist."
  `((input . ,(ena:cell-get-text cell))
    (cell_type . "code")
    ,@(ena:aif (ena:oref-safe cell :input-prompt-number)
          `((prompt_number . ,it)))
    (outputs . ,(if discard-output [] (apply #'vector (oref cell :outputs))))
    (language . "python")
    (collapsed . ,(if (oref cell :collapsed) t json-false))))

(defmethod ena:cell-to-json ((cell ena:textcell) &optional discard-output)
  `((cell_type . ,(oref cell :cell-type))
    (source    . ,(ena:cell-get-text cell))))

(defmethod ena:cell-to-json ((cell ena:headingcell) &optional discard-output)
  (let ((json (call-next-method)))
    (append json `((level . ,(oref cell :level))))))

(defmethod ena:cell-next ((cell ena:basecell))
  "Return next cell of the given CELL or nil if CELL is the last one."
  (ena:aif (ewoc-next (oref cell :ewoc)
                      (ena:cell-element-get cell :footer))
      (let ((cell (ena:$node-data (ewoc-data it))))
        (when (ena:basecell-child-p cell)
          cell))))

(defmethod ena:cell-prev ((cell ena:basecell))
  "Return previous cell of the given CELL or nil if CELL is the first one."
  (ena:aif (ewoc-prev (oref cell :ewoc)
                      (ena:cell-element-get cell :prompt))
      (let ((cell (ena:$node-data (ewoc-data it))))
        (when (ena:basecell-child-p cell)
          cell))))


;;; Kernel related calls.

(defmethod ena:cell-set-kernel ((cell ena:codecell) kernel)
  (oset cell :kernel kernel))


(defmethod ena:cell-execute ((cell ena:codecell))
  (ena:cell-execute-internal cell
                             (oref cell :kernel)
                             (ena:cell-get-text cell)
                             :silent nil))

(defmethod ena:cell-execute-internal ((cell ena:codecell)
                                      kernel code &rest args)
  (ena:cell-clear-output cell t t t)
  (ena:cell-set-input-prompt cell "*")
  (ena:cell-running-set cell t)
  (oset cell :dynamic t)
  (apply #'ena:kernel-execute kernel code (ena:cell-make-callbacks cell) args))

(defmethod ena:cell-make-callbacks ((cell ena:codecell))
  (list
   :execute_reply  (cons #'ena:cell--handle-execute-reply  cell)
   :output         (cons #'ena:cell--handle-output         cell)
   :clear_output   (cons #'ena:cell--handle-clear-output   cell)
   :set_next_input (cons #'ena:cell--handle-set-next-input cell)))

(defmethod ena:cell--handle-execute-reply ((cell ena:codecell) content
                                           -metadata-not-used-)
  (ena:cell-set-input-prompt cell (plist-get content :execution_count))
  (ena:cell-running-set cell nil)
  (let ((events (oref cell :events)))
    (ena:events-trigger events 'set_dirty.Worksheet (list :value t :cell cell))
    (ena:events-trigger events 'maybe_reset_undo.Worksheet cell)))

(defmethod ena:cell--handle-set-next-input ((cell ena:codecell) text)
  (let ((events (oref cell :events)))
    (ena:events-trigger events 'set_next_input.Worksheet
                        (list :cell cell :text text))
    (ena:events-trigger events 'maybe_reset_undo.Worksheet cell)))



;;; Output area

;; These function should go to ena-output-area.el.  But as cell and
;; EWOC is connected in complicated way, I will leave them in
;; ena-cell.el.

(defmethod ena:cell--handle-output ((cell ena:codecell) msg-type content
                                    -metadata-not-used-)
  (let* ((json (list :output_type msg-type)))
    (ena:case-equal msg-type
      (("stream")
       (plist-put json :text (plist-get content :data))
       (plist-put json :stream (plist-get content :name)))
      (("display_data" "pyout")
       (when (equal msg-type "pyout")
         (plist-put json :prompt_number (plist-get content :execution_count)))
       (setq json (ena:output-area-convert-mime-types
                   json (plist-get content :data))))
      (("pyerr")
       (plist-put json :ename (plist-get content :ename))
       (plist-put json :evalue (plist-get content :evalue))
       (plist-put json :traceback (plist-get content :traceback))))
    (ena:cell-append-output cell json t)
    ;; (oset cell :dirty t)
    (ena:events-trigger (oref cell :events) 'maybe_reset_undo.Worksheet cell)))


(defun ena:output-area-convert-mime-types (json data)
  (loop for (prop . mime) in '((:text       . :text/plain)
                               (:html       . :text/html)
                               (:svg        . :image/svg+xml)
                               (:png        . :image/png)
                               (:jpeg       . :image/jpeg)
                               (:latex      . :text/latex)
                               (:json       . :application/json)
                               (:javascript . :application/javascript)
                               (:emacs-lisp . :application/emacs-lisp))
        when (plist-member data mime)
        do (plist-put json prop (plist-get data mime)))
  json)


(defmethod ena:cell--handle-clear-output ((cell ena:codecell) content
                                          -metadata-not-used-)
  (ena:cell-clear-output cell
                         (plist-get content :stdout)
                         (plist-get content :stderr)
                         (plist-get content :other))
  (ena:events-trigger (oref cell :events) 'maybe_reset_undo.Worksheet cell))


;;; Misc.

(defmethod ena:cell-has-image-ouput-p ((cell ena:codecell))
  "Return `t' if given cell has image output, `nil' otherwise."
  (loop for out in (oref cell :outputs)
        when (or (plist-member out :svg)
                 (plist-member out :png)
                 (plist-member out :jpeg))
        return t))

(defmethod ena:cell-has-image-ouput-p ((cell ena:textcell))
  nil)

(defmethod ena:cell-get-tb-data ((cell ena:codecell))
  (loop for out in (oref cell :outputs)
        when (equal (plist-get out :output_type) "pyerr")
        return (plist-get out :traceback)))

(provide 'ena-cell)

;;; ena-cell.el ends here

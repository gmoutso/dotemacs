;;; ena-worksheet.el --- Worksheet module

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ena-worksheet.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ena-worksheet.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ena-worksheet.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ewoc)

(require 'ena-core)
(require 'ena-cell)
(require 'ena-kill-ring)


;;; Configuration

(define-obsolete-variable-alias
  'ena:notebook-enable-undo 'ena:worksheet-enable-undo "0.2.0")

(defcustom ena:worksheet-enable-undo 'yes
  "Configure undo in notebook buffers.

`no' : symbol
    Do not use undo in notebook buffers.  It is the safest option.
`yes' : symbol
    Enable undo in notebook buffers.  You can't undo after
    modification of cell (execution, add, remove, etc.).  This
    is default.
`full' : symbol
    Enable full undo in notebook buffers.  It is powerful but
    sometime (typically after the cell specific commands) undo
    mess up notebook buffer.  Use it on your own risk.  When the
    buffer is messed up, you can just redo and continue editing,
    or save it once and reopen it if you want to be careful.

You need to reopen the notebook buffer to reflect the change of
this value."
  :type '(choice (const :tag "No" no)
                 (const :tag "Yes" yes)
                 (const :tag "Full" full))
  :group 'ena)


;;; Configuration getter

(defun ena:worksheet-empty-undo-maybe ()
  "Empty `buffer-undo-list' if `ena:worksheet-enable-undo' is `yes'."
  (when (eq ena:worksheet-enable-undo 'yes)
    (setq buffer-undo-list nil)))


;;; Class and variable

(defvar ena:worksheet-buffer-name-template "*ena: %s/%s*")

(defclass ena:worksheet ()
  ((nbformat :initarg :nbformat :type integer)
   (get-notebook-name :initarg :get-notebook-name :type cons)
   ;; This slot introduces too much complexity so therefore must be
   ;; removed later.  This is here only for backward compatible
   ;; reason.
   (discard-output-p :initarg :discard-output-p)
   (saved-cells :initarg :saved-cells :initform nil
                :documentation
                "Slot to cache cells for worksheet without buffer")
   (dont-save-cells :initarg :dont-save-cells :initform nil :type boolean
                    :documentation "Don't cache cells when this flag is on.")
   (ewoc :initarg :ewoc :type ewoc)
   (kernel :initarg :kernel :type ena:$kernel)
   (dirty :initarg :dirty :type boolean :initform nil)
   (metadata :initarg :metadata :initform nil)
   (events :initarg :events)))

(ena:deflocal ena:%worksheet% nil
  "Buffer local variable to store an instance of `ena:worksheet'.")


;;; Initialization of object and buffer

(defun ena:worksheet-new (nbformat get-notebook-name discard-output-p
                                   kernel events &rest args)
  (apply #'make-instance 'ena:worksheet
         :nbformat nbformat :get-notebook-name get-notebook-name
         :discard-output-p discard-output-p :kernel kernel :events events
         args))

(defmethod ena:worksheet-bind-events ((ws ena:worksheet))
  (with-slots (events) ws
    ;; Bind events for sub components:
    (mapc (lambda (cell) (oset cell :events events))
          (ena:worksheet-get-cells ws))))

(defun ena:worksheet-class-bind-events (events)
  "Binds event handlers which are not needed to be bound per instance."
  (ena:events-on events
                 'maybe_reset_undo.Worksheet
                 (lambda (-ignore- cell)
                   (ena:with-live-buffer (ena:cell-buffer cell)
                     (ena:worksheet-empty-undo-maybe))))
  (ena:events-on events 'set_next_input.Worksheet
                 #'ena:worksheet--set-next-input)
  (ena:events-on events 'set_dirty.Worksheet #'ena:worksheet--set-dirty))

(defun ena:worksheet--set-next-input (-ignore- data)
  (destructuring-bind (&key cell text) data
    (ena:with-live-buffer (ena:cell-buffer cell)
      (ena:and-let* ((ws ena:%worksheet%)
                     (new-cell
                      (ena:worksheet-insert-cell-below ws 'code cell)))
        (ena:cell-set-text new-cell text)
        (oset ws :dirty t)))))

(defun ena:worksheet--set-dirty (-ignore- data)
  "Set dirty flag of worksheet in which CELL in DATA locates."
  (destructuring-bind (&key value cell) data
    (ena:with-live-buffer (ena:cell-buffer cell)
      (ena:worksheet-set-modified-p ena:%worksheet% value))))

(defmethod ena:worksheet-notebook-name ((ws ena:worksheet))
  (ena:funcall-packed (oref ws :get-notebook-name)))

(defmethod ena:worksheet-url-or-port ((ws ena:worksheet))
  (ena:kernel-url-or-port (oref ws :kernel)))

(defmethod ena:worksheet-name ((ws ena:worksheet))
  (plist-get (oref ws :metadata) :name))

(defmethod ena:worksheet-set-name ((ws ena:worksheet) name)
  "Set worksheet name.

\(fn ws name)"
  (assert (stringp name) nil "NAME must be a string.  Got: %S" name)
  (oset ws :metadata (plist-put (oref ws :metadata) :name name)))

(defmethod ena:worksheet-full-name ((ws ena:worksheet))
  (let ((nb-name (ena:worksheet-notebook-name ws)))
    (ena:aif (ena:worksheet-name ws)
        (concat nb-name "/" it)
      nb-name)))

(defmethod ena:worksheet-buffer ((ws ena:worksheet))
  (ena:and-let* (((slot-boundp ws :ewoc))
                 (ewoc (oref ws :ewoc))
                 (buffer (ewoc-buffer ewoc))
                 ((buffer-live-p buffer)))
    buffer))

(defmethod ena:worksheet--buffer-name ((ws ena:worksheet))
  (format ena:worksheet-buffer-name-template
          (ena:worksheet-url-or-port ws)
          (ena:worksheet-full-name ws)))

(defmethod ena:worksheet--get-buffer ((ws ena:worksheet))
  (or (ena:worksheet-buffer ws)
      (generate-new-buffer (ena:worksheet--buffer-name ws))))

(defmethod ena:worksheet-set-buffer-name ((ws ena:worksheet))
  (ena:with-live-buffer (ena:worksheet-buffer ws)
    (rename-buffer (ena:worksheet--buffer-name ws) t)))

(defmethod ena:worksheet-set-modified-p ((ws ena:worksheet) dirty)
  (ena:with-live-buffer (ena:worksheet-buffer ws)
    (set-buffer-modified-p dirty))
  (oset ws :dirty dirty))

(defmethod ena:worksheet-render ((ws ena:worksheet))
  (with-current-buffer (ena:worksheet--get-buffer ws)
    (setq ena:%worksheet% ws)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((ewoc (ena:ewoc-create 'ena:worksheet-pp
                                   (ena:propertize-read-only "\n")
                                   nil t))
            (cells (oref ws :saved-cells)))
        (oset ws :ewoc ewoc)
        (if cells
            (mapc (lambda (c)
                    (oset c :ewoc ewoc)
                    (ena:cell-enter-last c))
                  cells)
          (ena:worksheet-insert-cell-below ws 'code nil t))))
    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)  ; clear undo history
    (when (eq ena:worksheet-enable-undo 'no)
      (setq buffer-undo-list t))
    (ena:worksheet-bind-events ws)
    (ena:worksheet-set-kernel ws)
    (ena:log 'info "Worksheet %s is ready" (ena:worksheet-full-name ws))))

(defun ena:worksheet-pp (ewoc-data)
  (let ((path (ena:$node-path ewoc-data))
        (data (ena:$node-data ewoc-data)))
    (case (car path)
      (cell (ena:cell-pp (cdr path) data)))))


;;; Persistance and loading

(defmethod ena:worksheet-from-json ((ws ena:worksheet) data)
  (destructuring-bind (&key cells metadata &allow-other-keys) data
    (oset ws :metadata metadata)
    (oset ws :saved-cells
          (mapcar (lambda (data) (ena:cell-from-json data)) cells)))
  ws)

(defmethod ena:worksheet-to-json ((ws ena:worksheet))
  "Convert worksheet WS into JSON ready alist.
It sets buffer internally so that caller doesn not have to set
current buffer."
  (let* ((discard-output-p (oref ws :discard-output-p))
         (cells (ena:with-possibly-killed-buffer (ena:worksheet-buffer ws)
                  (mapcar (lambda (c)
                            (ena:cell-to-json
                             c (ena:funcall-packed discard-output-p c)))
                          (ena:worksheet-get-cells ws)))))
    `((cells . ,(apply #'vector cells))
      ,@(ena:aand (oref ws :metadata) `((metadata . ,it))))))

(defmethod ena:worksheet-save-cells ((ws ena:worksheet) &optional deactivate)
  "Save cells in worksheet buffer in cache before killing the buffer.

.. warning:: After called with non-nil DEACTIVATE flag is given,
   cells in worksheet cannot be used anymore.  Use only just
   before killing the buffer.

You don't need to set current buffer to call this function.
Do nothing when the worksheet WS has no buffer.

If the `:dont-save-cells' slot is non-nil (meaning that
`ena:worksheet-dont-save-cells' has been called), cells in the
worksheet buffer are not saved.  When the DEACTIVATE option is
given, cached cells are deactivated instead of the cells in
buffer.  Calling this function unconditionally resets
`:dont-save-cells' flag to nil to make caching work when the
worksheet WS is reopened.

\(fn ws deactivate)"
  (when (ena:worksheet-has-buffer-p ws)
    (unless (oref ws :dont-save-cells)
      (let ((cells (ena:worksheet-get-cells ws)))
        (with-current-buffer (ena:worksheet-buffer ws)
          (mapc #'ena:cell-save-text cells))
        (when deactivate (mapc #'ena:cell-deactivate cells))
        (oset ws :saved-cells cells)))
    (when deactivate
      (mapc #'ena:cell-deactivate (oref ws :saved-cells))))
  (oset ws :dont-save-cells nil))

(defmethod ena:worksheet-dont-save-cells ((ws ena:worksheet))
  "Turn on `:dont-save-cells' flag so that next call on
`ena:worksheet-save-cells' actually do nothing.

\(fn ws)"
  (oset ws :dont-save-cells t))


;;; Cell indexing, retrieval, etc.

(defmethod ena:worksheet-cell-from-type ((ws ena:worksheet) type &rest args)
  "Create a cell of TYPE (symbol or string)."
  ;; FIXME: unify type of TYPE to symbol or string.
  (apply #'ena:cell-from-type
         (format "%s" type)
         :ewoc (oref ws :ewoc)
         :events (oref ws :events)
         args))

(defmethod ena:worksheet-get-cells ((ws ena:worksheet))
  (if (ena:worksheet-has-buffer-p ws)
      (let* ((ewoc (oref ws :ewoc))
             (nodes (ewoc-collect ewoc
                                  (lambda (n) (ena:cell-node-p n 'prompt)))))
        (mapcar #'ena:$node-data nodes))
    (oref ws :saved-cells)))

(defmethod ena:worksheet-ncells ((ws ena:worksheet))
  (length (ena:worksheet-get-cells ws)))

(defun ena:worksheet-get-ewoc (&optional ws)
  (ena:aand (or ws ena:%worksheet%) (oref it :ewoc)))

(defun ena:worksheet-get-current-ewoc-node (&optional pos)
  (ena:aand (ena:worksheet-get-ewoc) (ewoc-locate it pos)))

(defun ena:worksheet-get-nearest-cell-ewoc-node (&optional pos max cell-p)
  (ena:and-let* ((ewoc-node (ena:worksheet-get-current-ewoc-node pos)))
    ;; FIXME: can be optimized using the argument `max'
    (while (and ewoc-node
                (not (and (ena:cell-ewoc-node-p ewoc-node)
                          (if cell-p
                              (funcall cell-p
                                       (ena:cell-from-ewoc-node ewoc-node))
                            t))))
      (setq ewoc-node (ewoc-next (oref ena:%worksheet% :ewoc) ewoc-node)))
    ewoc-node))

(defun* ena:worksheet-get-current-cell (&key pos noerror
                                             (cell-p #'ena:basecell-child-p))
  "Return a cell at POS.  If POS is not given, it is assumed be the
current cursor position.  When the current buffer is not worksheet
buffer or there is no cell in the current buffer, return `nil'."
  (let ((cell (ena:cell-from-ewoc-node
               (ena:worksheet-get-current-ewoc-node pos))))
    (if (funcall cell-p cell)
        cell
      (unless noerror
        (error "No cell found at pos=%s" pos)))))

(defun ena:worksheet-at-codecell-p ()
  (ena:worksheet-get-current-cell :noerror t :cell-p #'ena:codecell-p))

(defun ena:worksheet-get-cells-in-region (beg end)
  (ena:clip-list (ena:aand ena:%worksheet% (ena:worksheet-get-cells it))
                 (ena:worksheet-get-current-cell :pos beg)
                 (ena:worksheet-get-current-cell :pos end)))

(defun* ena:worksheet-get-cells-in-region-or-at-point
    (&key noerror (cell-p #'ena:basecell-child-p))
  (or (ena:filter cell-p
                  (if (region-active-p)
                      (ena:worksheet-get-cells-in-region (region-beginning)
                                                         (region-end))
                    (list (ena:worksheet-get-current-cell))))
      (unless noerror
        (error "Cell not found"))))


;;; Insertion and deletion of cells

(defun ena:worksheet--get-ws-or-error ()
  (or ena:%worksheet% (error "Not in worksheet buffer.")))

(defun ena:worksheet-focus-cell ()
  (ena:aand (ena:worksheet-get-current-cell :noerror t) (ena:cell-goto it)))

(defun ena:worksheet-delete-cell (ws cell &optional focus)
  "Delete a cell.  \(WARNING: no undo!)
This command has no key binding because there is no way to undo
deletion.  Use kill to play on the safe side.

If you really want use this command, you can do something like this
\(but be careful when using it!)::

  \(define-key ena:notebook-mode-map \"\\C-c\\C-d\"
              'ena:worksheet-delete-cell)"
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell)
                     t))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))        ; disable undo recording
    (apply #'ewoc-delete
           (oref ws :ewoc)
           (ena:cell-all-element cell)))
  (oset ws :dirty t)
  (ena:worksheet-empty-undo-maybe)
  (when focus (ena:worksheet-focus-cell)))

(defun ena:worksheet-kill-cell (ws cells &optional focus)
  "Kill (\"cut\") the cell at point or cells in region.
Note that the kill-ring for cells is not shared with the default
kill-ring of Emacs (kill-ring for texts)."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-cells-in-region-or-at-point)
                     t))
  (when cells
    (mapc (lambda (c)
            (ena:cell-save-text c)
            (ena:worksheet-delete-cell ws c)
            (ena:cell-deactivate c))
          cells)
    (ena:kill-new cells)
    (when focus
      (deactivate-mark)
      (ena:worksheet-focus-cell))))

(defun ena:worksheet-copy-cell (cells)
  "Copy the cell at point.  (Put the current cell into the kill-ring.)"
  (interactive
   (list (when (ena:worksheet--get-ws-or-error)
           (prog1 (ena:worksheet-get-cells-in-region-or-at-point)
             (deactivate-mark)))))
  (let ((cells (mapcar
                (lambda (c)
                  (ena:cell-deactivate (ena:cell-copy c))) cells)))
    (ena:log 'info "%s cells are copied." (length  cells))
    (ena:kill-new cells)))

(defun ena:worksheet-insert-clone-below (ws cell pivot)
  (let ((clone (ena:cell-copy cell)))
    ;; Cell can be from another buffer, so reset `ewoc'.
    (oset clone :ewoc (oref ws :ewoc))
    (ena:worksheet-insert-cell-below ws clone pivot)
    clone))

(defun ena:worksheet-yank-cell (ws &optional n)
  "Insert (\"paste\") the latest killed cell.
Prefixes are act same as the normal `yank' command."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (let ((arg current-prefix-arg))
                       (cond ((listp arg) 0)
                             ((eq arg '-) -2)
                             (t (1- arg))))))
  (let* ((cell (ena:worksheet-get-current-cell :noerror t)) ; can be nil
         (killed (ena:current-kill n)))
    (loop for c in killed
          with last = cell
          do (setq last (ena:worksheet-insert-clone-below ws c last))
          finally (ena:cell-goto last))))

(defun ena:worksheet-maybe-new-cell (ws type-or-cell)
  "Return TYPE-OR-CELL as-is if it is a cell, otherwise return a new cell."
  (let ((cell (if (ena:basecell-child-p type-or-cell)
                  type-or-cell
                (ena:worksheet-cell-from-type ws type-or-cell))))
    ;; When newly created or copied, kernel is not attached or not the
    ;; kernel of this worksheet.  So reset it here.
    (when (ena:codecell-p cell)
      (oset cell :kernel (oref ws :kernel)))
    (oset cell :events (oref ws :events))
    cell))

(defun ena:worksheet-insert-cell-below (ws type-or-cell pivot &optional focus)
  "Insert cell below.  Insert markdown cell instead of code cell
when the prefix argument is given.

When used as a lisp function, insert a cell of TYPE-OR-CELL just
after PIVOT and return the new cell."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (if current-prefix-arg 'markdown 'code)
                     (ena:worksheet-get-current-cell :noerror t) ; can be nil
                     t))
  (let ((cell (ena:worksheet-maybe-new-cell ws type-or-cell)))
    (cond
     ((= (ena:worksheet-ncells ws) 0)
      (ena:cell-enter-last cell))
     (pivot
      (ena:cell-insert-below pivot cell))
     (t (error
         "PIVOT is `nil' but ncells != 0.  There is something wrong...")))
    (ena:worksheet-empty-undo-maybe)
    (oset ws :dirty t)
    (when focus (ena:cell-goto cell))
    cell))

(defun ena:worksheet-insert-cell-above (ws type-or-cell pivot &optional focus)
  "Insert cell above.  Insert markdown cell instead of code cell
when the prefix argument is given.
See also: `ena:worksheet-insert-cell-below'."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (if current-prefix-arg 'markdown 'code)
                     (ena:worksheet-get-current-cell :noerror t) ; can be nil
                     t))
  (let ((cell (ena:worksheet-maybe-new-cell ws type-or-cell)))
    (cond
     ((< (ena:worksheet-ncells ws) 2)
      (ena:cell-enter-first cell))
     (pivot
      (let ((prev-cell (ena:cell-prev pivot)))
        (if prev-cell
            (ena:cell-insert-below prev-cell cell)
          (ena:cell-enter-first cell))))
     (t (error
         "PIVOT is `nil' but ncells > 0.  There is something wrong...")))
    (ena:worksheet-empty-undo-maybe)
    (oset ws :dirty t)
    (when focus (ena:cell-goto cell))
    cell))

(defun ena:worksheet-toggle-cell-type (ws cell &optional focus)
  "Toggle the cell type of the cell at point.
Use `ena:worksheet-change-cell-type' to change the cell type
directly."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell)
                     t))
  (let ((type (case (oref ws :nbformat)
                (2 (ena:case-equal (oref cell :cell-type)
                     (("code") "markdown")
                     (("markdown") "code")))
                (3 (ena:case-equal (oref cell :cell-type)
                     (("code") "markdown")
                     (("markdown") "raw")
                     (("raw") "heading")
                     (("heading") "code"))))))
    (let ((relpos (ena:cell-relative-point cell))
          (new (ena:cell-convert-inplace cell type)))
      (when (ena:codecell-p new)
        (oset new :kernel (oref ws :kernel)))
      (ena:worksheet-empty-undo-maybe)
      (when focus (ena:cell-goto new relpos)))))

(defun ena:worksheet-change-cell-type (ws cell type &optional level focus)
  "Change the cell type of the current cell.
Prompt will appear in the minibuffer.

When used in as a Lisp function, TYPE (string) should be chose
from \"code\", \"markdown\", \"raw\" and \"heading\".  LEVEL is
an integer used only when the TYPE is \"heading\"."
  (interactive
   (let* ((ws (ena:worksheet--get-ws-or-error))
          (cell (ena:worksheet-get-current-cell))
          (choices (case (oref ws :nbformat)
                     (2 "cm")
                     (3 "cmr123456")))
          (key (ena:ask-choice-char
                (format "Cell type [%s]: " choices) choices))
          (type (case key
                  (?c "code")
                  (?m "markdown")
                  (?r "raw")
                  (t "heading")))
          (level (when (equal type "heading")
                   (string-to-number (char-to-string key)))))
     (list ws cell type level t)))

  (let ((relpos (ena:cell-relative-point cell))
        (new (ena:cell-convert-inplace cell type)))
    (when (ena:codecell-p new)
      (oset new :kernel (oref ws :kernel)))
    (when level
      (ena:cell-change-level new level))
    (ena:worksheet-empty-undo-maybe)
    (when focus (ena:cell-goto new relpos))))

(defun ena:worksheet-split-cell-at-point (ws cell &optional no-trim focus)
  "Split cell at current position. Newlines at the splitting
point will be removed. This can be omitted by giving a prefix
argument \(C-u)."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell)
                     current-prefix-arg
                     t))
  ;; FIXME: should I inhibit undo?
  (let* ((beg (set-marker (make-marker) (ena:cell-input-pos-min cell)))
         (pos (point-marker))
         (head (buffer-substring beg pos))
         (new (ena:worksheet-insert-cell-above ws
                                               (oref cell :cell-type)
                                               cell)))
    (when (ena:headingcell-p cell)
      (ena:cell-change-level new (oref cell :level)))
    (delete-region beg pos)
    (unless no-trim
      (setq head (ena:trim-right head "\n"))
      (save-excursion
        (goto-char pos)
        (let ((end (set-marker (make-marker) (ena:cell-input-pos-max cell))))
          (while (and (looking-at-p "\n") (< (point) end))
            (delete-char 1)))))
    (ena:cell-set-text new head)
    (ena:worksheet-empty-undo-maybe)
    (when focus (ena:cell-goto cell))))

(defun ena:worksheet-merge-cell (ws cell &optional next focus)
  "Merge previous cell into current cell.
If prefix is given, merge current cell into next cell."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell)
                     current-prefix-arg
                     t))
  (unless next
    (setq cell (ena:cell-prev cell))
    (unless cell (error "No previous cell"))
    (ena:cell-goto cell))
  (let* ((next-cell (ena:cell-next cell))
         (head (ena:cell-get-text cell)))
    (assert next-cell nil "No cell to merge.")
    (ena:worksheet-delete-cell ws cell)
    (save-excursion
      (goto-char (ena:cell-input-pos-min next-cell))
      (insert head "\n"))
    (ena:worksheet-empty-undo-maybe)
    (when focus (ena:cell-goto next-cell))))


;;; Cell selection.

(defun* ena:worksheet-next-input-cell (ewoc-node &optional up (nth 1))
  "Return a cell containing the next input node after EWOC-NODE.
When UP is non-`nil', do the same for the *previous* input node.
When NTH is specified, return NTH cell.  Note that this function is
*not* defined for NTH=0; it returns nil."
  (unless (= nth 0)
    (when (< nth 0)
      (setq nth (* nth -1))
      (setq up (not up)))
    (let ((cell (ena:worksheet-next-input-cell-1 ewoc-node up)))
      (loop repeat (1- nth)
            with next = (if up #'ena:cell-prev #'ena:cell-next)
            if (funcall next cell)
            do (setq cell it)
            else
            return nil)
      cell)))

(defun ena:worksheet-next-input-cell-1 (ewoc-node &optional up)
  (let* ((ewoc-data (ewoc-data ewoc-node))
         (cell (ena:$node-data ewoc-data))
         (path (ena:$node-path ewoc-data))
         (element (nth 1 path)))
    (if (memql element (if up '(output footer) '(prompt)))
        cell
      (funcall (if up #'ena:cell-prev #'ena:cell-next) cell))))

(defun ena:worksheet-goto-input (ewoc-node up)
  (ena:aif (ena:worksheet-next-input-cell ewoc-node up)
      (ena:cell-goto it)
    (error "No %s input!" (if up "previous" "next"))))

(defun ena:worksheet-goto-next-input (ewoc-node)
  (interactive (list (and (ena:worksheet--get-ws-or-error)
                          (ena:worksheet-get-current-ewoc-node))))
  (ena:worksheet-goto-input ewoc-node nil))

(defun ena:worksheet-goto-prev-input (ewoc-node)
  (interactive (list (and (ena:worksheet--get-ws-or-error)
                          (ena:worksheet-get-current-ewoc-node))))
  (ena:worksheet-goto-input ewoc-node t))

(defun ena:worksheet-goto-next-cell-element (&optional nth up relpos prop)
  "Go to NTH next cell element named PROP and shift cursor by RELPOS.
Go to previous cell if UP is t.
Return t when the movement is succeeded."
  (unless prop (setq prop :input))
  (ena:and-let* ((current-node (ena:worksheet-get-current-ewoc-node))
                 (current-cell (ena:cell-from-ewoc-node current-node))
                 (target-cell
                  (if (and (= nth 1)
                           (eq (ena:cell-element-get current-cell :input)
                               current-node)
                           (not (and up
                                     (= (1+ (ewoc-location current-node))
                                        (point)))))
                      current-cell
                    (ena:worksheet-next-input-cell current-node up nth))))
    (ena:cell-goto target-cell relpos prop)
    t))

(defun ena:worksheet-beginning-of-cell-input (&optional arg)
  "Move backward to the beginning of a cell.
This function is for `beginning-of-defun-function', so behaves
similarly with `beginning-of-defun'.
It is set in `ena:notebook-multilang-mode'."
  (ena:worksheet-goto-next-cell-element (or arg 1) t))

(defun ena:worksheet-end-of-cell-input (&optional arg)
  "Move forward to the end of a cell.
This function is for `end-of-defun-function', so behaves
similarly with `end-of-defun'.
It is set in `ena:notebook-multilang-mode'."
  (ena:worksheet-goto-next-cell-element (or arg 1) nil 0 :after-input))


;;; Cell movement

(defun ena:worksheet-move-cell (ws cell up)
  (ena:aif (if up (ena:cell-prev cell) (ena:cell-next cell))
      (let ((inhibit-read-only t)
            (pivot-cell it))
        (ena:cell-save-text cell)
        (ena:worksheet-delete-cell ws cell)
        (funcall (if up
                     #'ena:worksheet-insert-cell-above
                   #'ena:worksheet-insert-cell-below)
                 ws cell pivot-cell)
        (ena:cell-goto cell)
        (oset ws :dirty t))
    (error "No %s cell" (if up "previous" "next"))))

(defun ena:worksheet-move-cell-up (ws cell)
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell)))
  (ena:worksheet-move-cell ws cell t))

(defun ena:worksheet-move-cell-down (ws cell)
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell)))
  (ena:worksheet-move-cell ws cell nil))


;;; Cell collapsing and output clearing

(defun ena:worksheet-toggle-output (ws cell)
  "Toggle the visibility of the output of the cell at point.
This does not alter the actual data stored in the cell."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell
                      :cell-p #'ena:codecell-p)))
  (ena:cell-toggle-output cell)
  (ena:worksheet-empty-undo-maybe)
  (oset ws :dirty t))

(defun ena:worksheet-set-output-visibility-all (ws &optional collapsed)
  "Show all cell output.  When prefix is given, hide all cell output."
  (interactive (list (ena:worksheet--get-ws-or-error) current-prefix-arg))
  (when collapsed (setq collapsed t))   ; force it to be a boolean
  (mapc (lambda (c)
          (when (ena:codecell-p c) (ena:cell-set-collapsed c collapsed)))
        (ena:worksheet-get-cells ws))
  (ena:worksheet-empty-undo-maybe)
  (oset ws :dirty t))

(defun ena:worksheet-clear-output (cell &optional preserve-input-prompt)
  "Clear output from the current cell at point.
Do not clear input prompt when the prefix argument is given."
  (interactive (list (ena:worksheet-get-current-cell
                      :cell-p #'ena:codecell-p)
                     current-prefix-arg))
  (ena:cell-clear-output cell t t t)
  (unless preserve-input-prompt
    (ena:cell-set-input-prompt cell))
  (ena:worksheet-empty-undo-maybe))

(defun ena:worksheet-clear-all-output (ws &optional preserve-input-prompt)
  "Clear output from all cells.
Do not clear input prompts when the prefix argument is given."
  (interactive (list (ena:worksheet--get-ws-or-error) current-prefix-arg))
  (mapc (lambda (c) (ena:worksheet-clear-output c preserve-input-prompt))
        (ena:filter #'ena:codecell-p (ena:worksheet-get-cells ws))))


;;; Kernel related things

(defmethod ena:worksheet-set-kernel ((ws ena:worksheet))
  (mapc (lambda (cell) (oset cell :kernel (oref ws :kernel)))
        (ena:filter #'ena:codecell-p (ena:worksheet-get-cells ws))))

(defun ena:worksheet-execute-cell (ws cell)
  "Execute code type CELL."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell
                      :cell-p #'ena:codecell-p)))
  (ena:kernel-if-ready (oref ws :kernel)
    (ena:cell-execute cell)
    (oset ws :dirty t)
    cell))

(defun ena:worksheet-execute-cell-and-goto-next (ws cell &optional insert)
  "Execute cell at point if it is a code cell and move to the
next cell, or insert if none."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell)))
  (when (ena:codecell-p cell)
    (ena:worksheet-execute-cell ws cell))
  (ena:aif (and (not insert) (ena:cell-next cell))
      (ena:cell-goto it)
    (ena:worksheet-insert-cell-below ws 'code cell t)))

(defun ena:worksheet-execute-cell-and-insert-below (ws cell)
  "Execute cell at point if it is a code cell and insert a
cell bellow."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell)))
  (ena:worksheet-execute-cell-and-goto-next ws cell t))

(defun ena:worksheet-execute-all-cell (ws)
  "Execute all cells in the current worksheet buffer."
  (interactive (list (ena:worksheet--get-ws-or-error)))
  (mapc #'ena:cell-execute
        (ena:filter #'ena:codecell-p (ena:worksheet-get-cells ws))))

(defun ena:worksheet-insert-last-input-history (ws cell index)
  "Insert INDEX-th previous history into CELL in worksheet WS."
  (ena:kernel-history-request
   (oref ws :kernel)
   (list
    :history_reply
    (cons
     (lambda (cell content -metadata-not-used-)
       (destructuring-bind (session line-number input)
           (car (plist-get content :history))
         (if (eq (ena:worksheet-get-current-cell) cell)
             (ena:cell-set-text cell input)
           (ena:log 'warning
             "Cursor moved from the cell after history request."))
         (ena:log 'info "Input history inserted: session:%d line:%d"
                  session line-number)))
     cell))
   :hist-access-type "range"
   :session 0
   :start (- index)
   :stop (- 1 index)))

(defvar ena:worksheet--history-index 1)

(defun ena:worksheet--get-history-index (inc)
  "Increment history index by (possibly negative) INC.
Get history index for `ena:worksheet-previous-input-history' and
`ena:worksheet-next-input-history'.  Raise error if caller tries
to decrement index to less than or equal to 1."
  (if (or (eq last-command 'ena:worksheet-previous-input-history)
          (eq last-command 'ena:worksheet-next-input-history))
      (progn
        (setq ena:worksheet--history-index
              (+ ena:worksheet--history-index inc))
        (when (< ena:worksheet--history-index 1)
          (setq ena:worksheet--history-index 1)
          (error "This is the latest input"))
        ena:worksheet--history-index)
    (setq ena:worksheet--history-index 1)))

(defun ena:worksheet-previous-input-history (ws cell index)
  "Insert the previous input in the execution history.
You can go back further in the history by repeating this command.
Use `ena:worksheet-next-input-history' to go forward in the
history."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell)
                     (ena:worksheet--get-history-index +1)))
  (ena:worksheet-insert-last-input-history ws cell index))

(defun ena:worksheet-next-input-history (ws cell index)
  "Insert next input in the execution history.
You can go forward further in the history by repeating this
command.  Use `ena:worksheet-previous-input-history' to go back
in the history."
  (interactive (list (ena:worksheet--get-ws-or-error)
                     (ena:worksheet-get-current-cell)
                     (ena:worksheet--get-history-index -1)))
  (ena:worksheet-insert-last-input-history ws cell index))


;;; Metadata

(defun ena:worksheet-rename-sheet (ws name)
  "Change worksheet name (*not* notebook name)."
  (interactive (let ((ws (ena:worksheet--get-ws-or-error)))
                 (list ws
                       (read-from-minibuffer
                        "New worksheet name: " (ena:worksheet-name ws)))))
  (unless (equal name (or (ena:worksheet-name ws) ""))
    (ena:worksheet-set-name ws name)
    (ena:worksheet-set-modified-p ws t)
    (ena:worksheet-set-buffer-name ws)))


;;; Generic getter

(defun ena:get-url-or-port--worksheet ()
  (when (ena:worksheet-p ena:%worksheet%)
    (ena:worksheet-url-or-port ena:%worksheet%)))

(defun ena:get-kernel--worksheet ()
  (when (ena:worksheet-p ena:%worksheet%) (oref ena:%worksheet% :kernel)))

(defun ena:get-cell-at-point--worksheet ()
  (ena:worksheet-get-current-cell :noerror t))

(defun ena:get-traceback-data--worksheet ()
  (ena:aand (ena:get-cell-at-point--worksheet) (ena:cell-get-tb-data it)))


;;; Predicate

(defun ena:worksheet-buffer-p ()
  "Return non-`nil' if the current buffer is a worksheet buffer."
  ena:%worksheet%)

(defmethod ena:worksheet-has-buffer-p ((ws ena:worksheet))
  (ena:aand (ena:worksheet-buffer ws) (buffer-live-p it)))

(defmethod ena:worksheet-modified-p ((ws ena:worksheet))
  (let ((buffer (ena:worksheet-buffer ws)))
    (and (buffer-live-p buffer)
         (or (oref ws :dirty)
             (buffer-modified-p buffer)))))


;;; Utility commands

(defun ena:worksheet-dedent-cell-text (cell)
  "Dedent text in CELL."
  (interactive (list (ena:worksheet-get-current-cell)))
  (let* ((beg (ena:cell-input-pos-min cell))
         (end (ena:cell-input-pos-max cell)))
    (indent-rigidly
     beg end (- (ena:find-leftmot-column beg end)))))


;;; Auto-execution

(defun ena:worksheet-toggle-autoexec (cell)
  "Toggle auto-execution flag of the cell at point."
  (interactive (list (ena:worksheet-get-current-cell #'ena:codecell-p)))
  (ena:cell-toggle-autoexec cell))

(defun ena:worksheet-turn-on-autoexec (cells &optional off)
  "Turn on auto-execution flag of the cells in region or cell at point.
When the prefix argument is given, turn off the flag instead.

To use autoexec feature, you need to turn on auto-execution mode
in connected buffers, using the `ena:connect-toggle-autoexec'
command."
  (interactive
   (list (ena:worksheet-get-cells-in-region-or-at-point
          :cell-p #'ena:codecell-p)
         current-prefix-arg))
  (mapc (lambda (c) (ena:cell-set-autoexec c (not off))) cells)
  (ena:log 'info "Turn %s auto-execution flag of %s cells."
           (if off "off" "on")
           (length cells)))

(defun ena:worksheet-execute-autoexec-cells (ws)
  "Execute cells of which auto-execution flag is on.
This function internally sets current buffer to the worksheet
buffer, so you don't need to set current buffer to call this
function."
  (interactive (list (ena:worksheet--get-ws-or-error)))
  (ena:with-live-buffer (ena:worksheet-buffer ws)
    (ena:kernel-if-ready (oref ws :kernel)
      (mapc #'ena:cell-execute
            (ena:filter #'ena:cell-autoexec-p
                        (ena:worksheet-get-cells ws))))))


;;; Imenu

(defun ena:worksheet-imenu-create-index ()
  "`imenu-create-index-function' for notebook buffer."
  ;; As Imenu does not provide the way to represent level *and*
  ;; position, use #'s to do that.
  (loop for cell in (when (ena:worksheet-p ena:%worksheet%)
                      (ena:filter #'ena:headingcell-p
                                  (ena:worksheet-get-cells ena:%worksheet%)))
        for sharps = (loop repeat (oref cell :level) collect "#")
        for text = (ena:cell-get-text cell)
        for name = (ena:join-str "" (append sharps (list " " text)))
        collect (cons name (ena:cell-input-pos-min cell))))

(defun ena:worksheet-imenu-setup ()
  "Called via notebook mode hooks."
  (setq imenu-create-index-function #'ena:worksheet-imenu-create-index))


;;; Workarounds

(defadvice fill-paragraph (around ena:worksheet-fill-paragraph activate)
  "Prevent \"Text is read-only\" error when filling paragraph in
EIN worksheet."
  (if ena:%worksheet%
      (let* ((cell (ena:worksheet-get-current-cell))
             (beg (copy-marker (ena:cell-input-pos-min cell))))
        (save-excursion
          (goto-char beg)
          (insert "\n"))
        (unwind-protect
            ad-do-it
          (save-excursion
            (goto-char beg)
            (delete-char 1))))
    ad-do-it))

(provide 'ena-worksheet)

;;; ena-worksheet.el ends here

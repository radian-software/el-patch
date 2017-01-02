;;; el-patch.el --- Future-proof your Emacs Lisp customizations!

;; Copyright (C) 2016 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/el-patch
;; Keywords: extensions
;; Created: 31 Dec 2016

;;; Commentary:

;; Yet to come.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Libraries

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal variables

(defvar el-patch--patches (make-hash-table :test 'equal)
  "Hash table of patches that have been defined.
The keys are symbols that are function names. The values are
patch definitions, which are lists beginning with `defun',
`defmacro', etc.")

(defvar el-patch--not-present 'key-is-not-present-in-hash-table
  "Value used as a default argument to `gethash'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Resolving patches

(defmacro el-patch--with-puthash (table kvs &rest body)
  "Bind variables in hash TABLE according to KVS then eval BODY.
Each of the KVS is a list whose first element is the key and
whose second element is the value. After BODY is evaluated, the
original state of TABLE is restored. Return value is the result
of evaluating the last form in BODY."
  (declare (indent 2))
  `(let* ((table ,table)
          (kvs ,kvs)
          (original-kvs (mapcar (lambda (kv)
                                  (list (car kv)
                                        (gethash (cadr kv) table
                                                 el-patch--not-present)))
                                kvs)))
     (prog2
         (dolist (kv kvs)
           (puthash (car kv) (cadr kv) table))
         (progn ,@body)
       (dolist (kv original-kvs)
         ;; Note that we can't distinguish between a missing value and
         ;; a value that is coincidentally equal to
         ;; `el-patch--not-present', due to limitations in the Emacs
         ;; Lisp hash table API.
         (if (equal (car kv) el-patch--not-present)
             (remhash (car kv) table)
           (puthash (car kv) (cadr kv) table))))))

(defun el-patch--resolve (form new &optional table)
  "Resolve a patch FORM.
Return a list of forms to be spliced into the surrounding
s-expression. Resolve in favor of the original version if NEW is
nil; otherwise resolve in favor of the new version. TABLE is a
hash table of `el-patch-let' bindings, which maps symbols to
their bindings."
  (let ((table (or table (make-hash-table :test 'equal))))
    (if (listp form)
        (let* ((directive (nth 0 form))
               (this-directive (if new
                                   (pcase directive
                                     ('el-patch-remove 'el-patch-add)
                                     ('el-patch-splice 'el-patch-wrap)
                                     (_ directive))
                                 directive))
               (inverted (not (equal this-directive directive)))
               (this-new (if inverted (not new) new))
               (resolve (lambda (form) (el-patch--resolve form new table))))
          (pcase this-directive
            ((quote el-patch-add)
             (when (<= (length form) 1)
               (error "Not enough arguments (%d) for `%s'"
                      (1- (length form)) directive))
             (when this-new
               (cl-mapcan resolve (cdr form))))
            ((quote el-patch-swap)
             (cond
              ((<= (length form) 2)
               (error "Not enough arguments (%d) for `el-patch-swap'"
                      (1- (length form))))
              ((>= (length form) 4)
               (error "Too many arguments (%d) in for `el-patch-swap'"
                      (1- (length form)))))
             (funcall resolve
                      (if this-new
                          (cl-caddr form)
                        (cadr form))))
            ((quote el-patch-wrap)
             (let ((triml (if (>= (length form) 3)
                              (nth 1 form)
                            0))
                   (trimr (if (>= (length form) 4)
                              (nth 2 form)
                            0))
                   (body (car (last form))))
               (cond
                ((<= (length form) 1)
                 (error "Not enough arguments (%d) for `%s'"
                        (1- (length form)) directive))
                ((>= (length form) 5)
                 (error "Too many arguments (%d) for `%s'"
                        (1- (length form)) directive))
                ((not (listp body))
                 (error "Non-list (%s) as last argument for `%s'"
                        (car (last form)) directive))
                ((and (>= (length form) 3)
                      (not (integerp triml)))
                 (error "Non-integer (%s) as first argument for `%s'"
                        (nth 1 form) directive))
                ((and (>= (length form) 4)
                      (not (integerp trimr)))
                 (error "Non-integer (%s) as second argument for `%s'"
                        (nth 2 form) directive))
                ((< triml 0)
                 (error "Left trim less than zero (%d) for `%s'"
                        triml directive))
                ((< trimr 0)
                 (error "Right trim less than zero (%d) for `%s'"
                        trimr directive))
                ((> (+ triml trimr) (length body))
                 (error "Combined trim (%d + %d) greater than body length (%d) for `%s'"
                        triml trimr (length body) directive)))
               (if new
                   (list (cl-mapcan resolve body))
                 (cl-mapcan resolve (nthcdr triml (butlast body trimr))))))
            ((quote el-patch-let)
             (let ((bindings (nth 1 form))
                   (body (nth 2 form)))
               (cond
                ((<= (length form) 2)
                 (error "Not enough arguments (%d) for `el-patch-let'"
                        (1- (length form))))
                ((>= (length form) 4)
                 (error "Too many arguments (%d) for `el-patch-let'"
                        (1- (length form))))
                ((not (listp bindings))
                 (error "Non-list (%s) as first argument for `el-patch-let'"
                        bindings)))
               (el-patch--with-puthash table
                   (mapcar (lambda (kv)
                             (unless (symbolp (car kv))
                               (error "Non-symbol (%s) as binding for `el-patch-let'"
                                      (car kv)))
                             (list (car kv)
                                   (funcall resolve (cadr kv))))
                           bindings)
                 (funcall resolve body))))
            ((quote el-patch-literal)
             (when (<= (length form) 1)
               (error "Not enough arguments (%d) for `el-patch-literal'"
                      (1- (length form))))
             (cdr form))
            (_ (list (cl-mapcan resolve form)))))
      (or (gethash form table)
          (list form)))))

(defun el-patch--resolve-definition (patch-definition new)
  "Resolve a PATCH-DEFINITION.
PATCH-DEFINITION is a list starting with `defun', `defmacro',
etc. Return a list of the same format. Resolve in favor of the
original version if NEW is nil; otherwise resolve in favor of the
new version."
  (cl-mapcan (lambda (form)
               (el-patch--resolve form new))
             patch-definition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Validating patches

(defun el-patch--find-function (name)
  "Return the Lisp form that defines the function NAME.
Return nil if such a definition cannot be found. (That would
happen if the definition were generated dynamically, or the
function is defined in the C code.)"
  (let* ((buffer-point (ignore-errors
                         ;; Just in case we get an error because the
                         ;; function is defined in the C code, we
                         ;; ignore it and return nil.
                         (save-excursion
                           ;; This horrifying bit of hackery
                           ;; prevents `find-function-noselect' from
                           ;; returning an existing buffer, so that
                           ;; later on when we jump to the
                           ;; definition, we don't temporarily
                           ;; scroll the window if the definition
                           ;; happens to be in the *current* buffer.
                           (prog2
                               (advice-add #'get-file-buffer :override
                                           #'ignore)
                               (find-function-noselect name 'lisp-only)
                             (advice-remove #'get-file-buffer #'ignore)))))
         (defun-buffer (car buffer-point))
         (defun-point (cdr buffer-point)))
    (and defun-buffer
         defun-point
         (with-current-buffer defun-buffer
           (save-excursion
             (goto-char defun-point)
             (read defun-buffer))))))

;;;###autoload
(defun el-patch-validate (patch-definition &optional nomsg)
  "Validate the patch given by PATCH-DEFINITION.
This means el-patch will attempt to find the original definition
for the function, and verify that it is the same as the original
function assumed by the patch. A warning will be signaled if the
original definition for a patched function cannot be found, or if
there is a difference between the actual and expected original
definitions.

PATCH-DEFINITION is a list beginning with `defun', `defmacro',
etc.

Returns nil if the patch is not valid, and otherwise returns t.
If NOMSG is non-nil, does not signal a message when the patch is
valid.

See also `el-patch-validate-all'."
  (interactive (list (el-patch--select-patch)))
  (let* ((name (cadr patch-definition))
         (old-definition (el-patch--resolve-definition
                          patch-definition nil))
         (actual-definition (el-patch--find-function name)))
    (cond
     ((not actual-definition)
      (display-warning
       'el-patch
       (format "Could not find definition of `%S'" name))
      nil)
     ((not (equal old-definition actual-definition))

      (display-warning
       'el-patch
       (format (concat "Definition of `%S' differs from what "
                       "is assumed by its patch")
               name)
       nil))
     (t
      (unless nomsg
        (message "Patch is valid"))
      t))))

;;;###autoload
(defun el-patch-validate-all ()
  "Validate all currently defined patches.
See `el-patch-validate'."
  (interactive)
  (let ((patch-count 0)
        (warning-count 0))
    (maphash (lambda (name patch-definition)
               (setq patch-count (1+ patch-count))
               (unless (el-patch-validate patch-definition 'nomsg)
                 (setq warning-count (1+ warning-count))))
             el-patch--patches)
    (cond
     ((zerop patch-count)
      (user-error "No patches defined"))
     ((zerop warning-count)
      (message "All %d patches are valid" patch-count))
     ((= patch-count warning-count)
      (message "All %d patches are invalid" patch-count))
     (t
      (message "%d patches are valid, %d patches are invalid"
               (- patch-count warning-count) warning-count)))))

(defvar el-patch-validation t
  "Whether or not to perform validation when a patch is defined.
If non-nil, then evaluating an `el-patch-defun' or
`el-patch-defmacro' form will automatically call
`el-patch-validate', and will only install the patch if it
is still valid.

Validation is slow and can generate messages, so it is
recommended that you set this variable to nil during Emacs
startup, and then set it back to t afterwards. This can be done
by calling the function `el-patch-disable-validation-during-init'
in your init-file.")

(defun el-patch--reenable-validation-after-init ()
  "Enable `el-patch-validation'.
Also remove this function from `after-init-hook'."
  (setq el-patch-validation t)
  (remove-hook 'after-init-hook #'el-patch--reenable-validation-after-init))

;;;###autoload
(defun el-patch-disable-validation-during-init ()
  "Disable `el-patch-validation' temporarily during Emacs startup.
Precisely, set it to nil and then set it to t when
`after-init-hook' runs."
  (setq el-patch-validation nil)
  (add-hook 'after-init-hook #'el-patch--reenable-validation-after-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Applying patches

(defun el-patch--advice-name (function-name)
  "Translate a FUNCTION-NAME into an advice name.
Return a symbol naming the `:override' advice el-patch will use
to patch a function called FUNCTION-NAME."
  (intern (format "el-patch--advice--%S" function-name)))

(defun el-patch--function-to-advice (definition)
  "Turn a function DEFINITION into an advice definition.
DEFINITION is a list starting with `defun', `defmacro', etc.
Return a new definition, a list starting with `defun', that can
be used to define an `:override' advice."
  `(defun ,(el-patch--advice-name (cadr definition))
       ,@(cddr definition)))

(defun el-patch--definition (patch-definition)
  "Activate a PATCH-DEFINITION.
PATCH-DEFINITION is a list starting with `defun', `defmacro',
etc. Update `el-patch--patches', create the advice, and activate
it."
  (let* ((function-name (cadr patch-definition))
         (advice-name (el-patch--advice-name function-name)))
    (when (or (not el-patch-validation)
              (el-patch-validate patch-definition 'nomsg))
      (puthash function-name patch-definition el-patch--patches)
      (eval (el-patch--function-to-advice
             (el-patch--resolve-definition patch-definition t)))
      (advice-add function-name :override advice-name))))

;;;###autoload
(defmacro el-patch-defun (&rest args)
  "Patch a function. The ARGS are the same as for `defun'."
  (declare (indent defun))
  `(el-patch--definition ',(cons 'defun args)))

;;;###autoload
(defmacro el-patch-defmacro (&rest args)
  "Patch a macro. The ARGS are the same as for `defmacro'."
  (declare (indent defun))
  `(el-patch--definition ',(cons 'defmacro args)))

;;;###autoload
(defmacro el-patch-defsubst (&rest args)
  "Patch an inline function. The ARGS are the same as for `defsubst'."
  (declare (indent defun))
  `(el-patch--definition ',(cons 'defsubst args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Patch directives

;;;###autoload
(defmacro el-patch-add (&rest args)
  "Patch directive for inserting forms.
In the original definition, the ARGS and their containing form
are removed. In the new definition, the ARGS are spliced into the
containing s-expression."
  (declare (indent 0))
  `(error "Can't use `el-patch-add' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-remove (&rest args)
  "Patch directive for removing forms.
In the original definition, the ARGS are spliced into the
containing s-expression. In the new definition, the ARGS and
their containing form are removed."
  (declare (indent 0))
  `(error "Can't use `el-patch-remove' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-swap (old new)
  "Patch directive for swapping forms.
In the original definition, OLD is spliced into the containing
s-expression. In the new definition, NEW is spliced instead."
  (declare (indent 0))
  `(error "Can't use `el-patch-swap' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-wrap (&optional triml trimr args)
  "Patch directive for wrapping forms.
TRIML and TRIMR are optional arguments. If only one is provided,
it is assumed to be TRIML. ARGS is required, and it must be a
list.

In the original definition, the ARGS are spliced into the
containing s-expression. If TRIML is provided, the first TRIML of
the ARGS are removed first. If TRIMR is provided, the last TRIMR
are also removed. In the new definition, the ARGS and their
containing list are spliced into the containing s-expression."
  (declare (indent defun))
  `(error "Can't use `el-patch-wrap' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-splice (&optional triml trimr args)
  "Patch directive for splicing forms.
TRIML and TRIMR are optional arguments. If only one is provided,
it is assumed to be TRIML. ARGS is required, and it must be a
list.

In the original definition, the ARGS and their containing list
are spliced into the containing s-expression. In the new
definition, the ARGS are spliced into the containing
s-expression. If TRIML is provided, the first TRIML of the ARGS
are removed first. If TRIMR is provided, the last TRIMR are also
removed."
  (declare (indent defun))
  `(error "Can't use `el-patch-splice' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-let (varlist arg)
  "Patch directive for creating local el-patch bindings.
Creates local bindings according to VARLIST, then resolves to ARG
in both the original and new definitions. You may bind symbols
that are also patch directives, but the bindings will not have
effect if the symbols are used at the beginning of a list (they
will act as patch directives)."
  (declare (indent 1))
  `(error "Can't use `el-patch-let' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-literal (arg)
  "Patch directive for treating patch directives literally.
Resolves to ARG, which is not processed further by el-patch."
  (declare (indent 0))
  `(error "Can't use `el-patch-literal' outside of an `el-patch'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Viewing patches

(defun el-patch--select-patch ()
  "Use `completing-read' to select a patched function.
Return the patch definition, a list beginning with `defun',
`defmacro', etc."
  (let ((options nil))
    (maphash (lambda (name patch-definition)
               (push (symbol-name name) options))
             el-patch--patches)
    (unless options
      (user-error "No patches defined"))
    (gethash (intern (completing-read
                      "Which function? "
                      options
                      (lambda (elt) t)
                      'require-match))
             el-patch--patches)))

(defun el-patch--ediff-forms (name1 form1 name2 form2)
  "Ediff two forms.
Obtain empty buffers named NAME1 and NAME2, pretty-print FORM1
and FORM2 into them respectively, and run Ediff on the two
buffers wordwise."
  (let ((min1) (max1) (min2) (max2))
    (with-current-buffer (get-buffer-create name1)
      (erase-buffer)
      (pp form1 (current-buffer))
      (setq min1 (point-min)
            max1 (point-max)))
    (with-current-buffer (get-buffer-create name2)
      (erase-buffer)
      (pp form2 (current-buffer))
      (setq min2 (point-min)
            max2 (point-max)))
    ;; Ugly hack because Ediff is missing an `ediff-buffers-wordwise'
    ;; function.
    (eval-and-compile
      (require 'ediff))
    (ediff-regions-internal
     (get-buffer name1) min1 max1
     (get-buffer name2) min2 max2
     nil 'ediff-regions-wordwise 'word-mode nil)))

;;;###autoload
(defun el-patch-ediff-patch (patch-definition)
  "Show the patch for a function in Ediff.
PATCH-DEFINITION is as returned by `el-patch--select-patch'."
  (interactive (list (el-patch--select-patch)))
  (let ((old-definition (el-patch--resolve-definition
                         patch-definition nil))
        (new-definition (el-patch--resolve-definition
                         patch-definition t)))
    (el-patch--ediff-forms
     "*el-patch original*" old-definition
     "*el-patch patched*" new-definition)
    (when (equal old-definition new-definition)
      (message "No patch"))))

;;;###autoload
(defun el-patch-ediff-conflict (patch-definition)
  "Show a patch conflict in Ediff.
This is a diff between the expected and actual values of a
patch's original function definition. PATCH-DEFINITION is as
returned by `el-patch--select-patch'."
  (interactive (list (el-patch--select-patch)))
  (let* ((name (cadr patch-definition))
         (actual-definition (el-patch--find-function name))
         (expected-definition (el-patch--resolve-definition
                               patch-definition nil)))
    (el-patch--ediff-forms
     "*el-patch actual*" actual-definition
     "*el-patch expected*" expected-definition)
    (when (equal actual-definition expected-definition)
      (message "No conflict"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Removing patches

;;;###autoload
(defun el-patch-unpatch (function-name)
  "Remove the patch for FUNCTION-NAME.
This restores the original functionality of FUNCTION-NAME."
  (interactive (list (cadr (el-patch--select-patch))))
  (let ((advice-name (el-patch--advice-name function-name)))
    (advice-remove function-name advice-name)
    (fmakunbound advice-name)
    (remhash function-name el-patch--patches)))

(provide 'el-patch)

;;; el-patch.el ends here

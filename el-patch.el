;;; el-patch.el --- Future-proof your Emacs Lisp customizations!

;; Copyright (C) 2016 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 31 Dec 2016
;; Homepage: https://github.com/raxod502/el-patch
;; Keywords: extensions
;; Package-Requires: ((emacs "24.4"))
;; Version: 1.0

;;; Commentary:

;; Please see https://github.com/raxod502/el-patch for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x occur with a query of four
;; semicolons followed by a space.

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

(defvar el-patch--feature nil
  "Feature specified by the last feature directive processed.
This is set to the argument of the last `el-patch-feature'
directive processed by `el-patch--resolve'.")

(defvar el-patch--features nil
  "List of features that have been declared to contain patches.
All of these features will be loaded when you call
`el-patch-validate-all', or when you call `el-patch-validate'
with a prefix argument.")

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
their bindings. Set `el-patch--feature' to the last value
specified in a `el-patch-feature' directive."
  (let ((table (or table (make-hash-table :test 'equal))))
    (if (listp form)
        (let* ((directive (nth 0 form))
               (this-directive (pcase directive
                                 ('el-patch-remove 'el-patch-add)
                                 ('el-patch-splice 'el-patch-wrap)
                                 (_ directive)))
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
            ((quote el-patch-feature)
             (cond
              ((<= (length form) 1)
               (error "Not enough arguments (%d) for `el-patch-feature'"
                      (1- (length form))))
              ((>= (length form) 3)
               (error "Too many arguments (%d) for `el-patch-feature'"
                      (1- (length form)))))
             (ignore (setq el-patch--feature (nth 1 form))))
            (_ (list (cl-mapcan resolve form)))))
      (or (gethash form table)
          (list form)))))

(defun el-patch--resolve-definition (patch-definition new)
  "Resolve a PATCH-DEFINITION.
PATCH-DEFINITION is a list starting with `defun', `defmacro',
etc. Return a list of the same format. Resolve in favor of the
original version if NEW is nil; otherwise resolve in favor of the
new version. Set `el-patch--feature' to the last value specified
in a `el-patch-feature' directive."
  (cl-mapcan (lambda (form)
               (el-patch--resolve form new))
             patch-definition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Validating patches

(defvar el-patch-pre-validate-hook nil
  "Hook run before `el-patch-validate-all'.
Also run before `el-patch-validate' if a prefix argument is
provided. This hook should contain functions that make sure all
of your patches are defined (for example, you might need to load
some features if your patches are lazily defined).")

(defvar el-patch-post-validate-hook nil
  "Hook run after `el-patch-validate-all'.
Also run after `el-patch-validate' if a prefix argument is
provided. This hook should contain functions that undo any
patching that might have taken place in
`el-patch-pre-validate-hook', if you do not want the patches to
be defined permanently.")

(defun el-patch--find-function (name)
  "Return the Lisp form that defines the function NAME.
Return nil if such a definition cannot be found. (That would
happen if the definition were generated dynamically, or the
function is defined in the C code, or the function is not
autoloaded and not provided by `el-patch--feature'.) If
`el-patch--feature' is non-nil, `require' that feature first."
  (when el-patch--feature
    (require el-patch--feature))
  (when (fboundp name)
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
               (read defun-buffer)))))))

;;;###autoload
(defun el-patch-validate (patch-definition &optional nomsg run-hooks)
  "Validate the patch given by PATCH-DEFINITION.
This means el-patch will attempt to find the original definition
for the function, and verify that it is the same as the original
function assumed by the patch. A warning will be signaled if the
original definition for a patched function cannot be found, or if
there is a difference between the actual and expected original
definitions.

Interactively, use `completing-read' to select a function to
inspect the patch of.

PATCH-DEFINITION is a list beginning with `defun', `defmacro',
etc.

Returns nil if the patch is not valid, and otherwise returns t.
If NOMSG is non-nil, does not signal a message when the patch is
valid.

If RUN-HOOKS is non-nil, runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'. Interactively, this happens when a
prefix argument is provided.

See also `el-patch-validate-all'."
  (interactive (progn
                 (when current-prefix-arg
                   (run-hooks 'el-patch-pre-validate-hook))
                 (list (el-patch--select-patch) nil current-prefix-arg)))
  (unwind-protect
      (progn
        (setq el-patch--feature nil)
        (let* ((name (cadr patch-definition))
               (expected-definition (el-patch--resolve-definition
                                     patch-definition nil))
               (actual-definition (el-patch--find-function name)))
          (cond
           ((not actual-definition)
            (display-warning
             'el-patch
             (format "Could not find definition of `%S'" name))
            nil)
           ((not (equal expected-definition actual-definition))

            (display-warning
             'el-patch
             (format (concat "Definition of `%S' differs from what "
                             "is assumed by its patch")
                     name))
            nil)
           (t
            (unless nomsg
              (message "Patch is valid"))
            t))))
    (when run-hooks
      (run-hooks 'el-patch-post-validate-hook))))

;;;###autoload
(defun el-patch-validate-all ()
  "Validate all currently defined patches.
Runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'.

See `el-patch-validate'."
  (interactive)
  (run-hooks 'el-patch-pre-validate-hook)
  (unwind-protect
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
                   (- patch-count warning-count) warning-count))))
    (run-hooks 'el-patch-post-validate-hook)))

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

(defun el-patch--autoload-function (function-name feature)
  (let ((recursive-autoload
         (intern (format "el-patch--recursive-autoload--%S"
                         function-name))))
    `(defun ,function-name (&rest args)
       ,(format "This function is a stub generated by el-patch. The patch for
`%S' has been loaded (see the `:override'
advice on this function), but the original definition of
`%S', which is provided by feature
`%S', has not yet been loaded.

If you remove the `:override' advice and then call
this function, it will load feature `%S' and
then invoke the actual definition of
`%S', just like an autoload."
                function-name function-name feature feature function-name)
       (if (boundp ',recursive-autoload)
           (error "Autoload of `%S' failed to define `%S'"
                  feature function-name)
         (require ',feature)
         (apply ',function-name args)))))

(defun el-patch--stealthy-defun (function-name definition)
  ;; FIXME should prevent `load-history' from being updated
  (eval definition))

(defun el-patch--definition (patch-definition)
  "Activate a PATCH-DEFINITION.
PATCH-DEFINITION is a list starting with `defun', `defmacro',
etc. Update `el-patch--patches', create the advice, and activate
it."
  (let* ((function-name (cadr patch-definition))
         (advice-name (el-patch--advice-name function-name)))
    (puthash function-name patch-definition el-patch--patches)
    (setq el-patch--feature nil)
    (eval (el-patch--function-to-advice
           (el-patch--resolve-definition patch-definition t)))
    ;; FIXME should not require `el-patch-feature' directive if the
    ;; function is autoloaded
    (unless (and (fboundp function-name)
                 (not (autoloadp (symbol-function function-name))))
      (unless el-patch--feature
        (error "You must specify an `el-patch-feature' directive for `%S'"
               function-name))
      (el-patch--stealthy-defun
       function-name
       (el-patch--autoload-function function-name el-patch--feature)))
    (advice-add function-name :override advice-name)))

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

;;;###autoload
(defmacro el-patch-feature (feature)
  "Patch directive for declaring which FEATURE loads a function.
If you patch a function that is not autoloaded, you need to
include this directive somewhere in the body of the patch
definition. It resolves to nothing in both the original and new
definitions."
  (declare (indent 0)))

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
  (setq el-patch--feature nil)
  (let* ((name (cadr patch-definition))
         (expected-definition (el-patch--resolve-definition
                               patch-definition nil))
         (actual-definition (el-patch--find-function name)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Closing remarks

(provide 'el-patch)

;;; el-patch.el ends here

;;; el-patch.el --- Future-proof your Emacs Lisp customizations

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
  "Hash table of patches that have been defined. The keys are
symbols that are function names. The values are patch
definitions, which are lists beginning with `defun', `defmacro',
etc.")

(defvar el-patch--not-present 'key-is-not-present-in-hash-table
  "Value used as a default argument to `gethash'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Resolving patches

(defmacro el-patch--with-puthash (table kvs &rest body)
  "Binds each of the KVS (lists whose first element is the key
and whose second element is the value) in the hash TABLE,
evaluates BODY, then restores the original state of TABLE. Return
value is the result of evaluating the last form in BODY."
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
  "Resolves a patch FORM, returning a list of forms to be spliced
into the surrounding s-expression. Resolves in favor of the
original version if NEW is nil; otherwise resolves in favor of
the new version. TABLE is a hash table of `el-patch-let'
bindings, which maps symbols to their bindings."
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
               (error "not enough arguments (%d) for `%s'"
                      (1- (length form)) directive))
             (when this-new
               (cl-mapcan resolve (cdr form))))
            ((quote el-patch-swap)
             (cond
              ((<= (length form) 2)
               (error "not enough arguments (%d) for `el-patch-swap'"
                      (1- (length form))))
              ((>= (length form) 4)
               (error "too many arguments (%d) in for `el-patch-swap'"
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
                 (error "not enough arguments (%d) for `%s'"
                        (1- (length form)) directive))
                ((>= (length form) 5)
                 (error "too many arguments (%d) for `%s'"
                        (1- (length form)) directive))
                ((not (listp body))
                 (error "non-list (%s) as last argument for `%s'"
                        (car (last form)) directive))
                ((and (>= (length form) 3)
                      (not (integerp triml)))
                 (error "non-integer (%s) as first argument for `%s'"
                        (nth 1 form) directive))
                ((and (>= (length form) 4)
                      (not (integerp trimr)))
                 (error "non-integer (%s) as second argument for `%s'"
                        (nth 2 form) directive))
                ((< triml 0)
                 (error "left trim less than zero (%d) for `%s'"
                        triml directive))
                ((< trimr 0)
                 (error "right trim less than zero (%d) for `%s'"
                        trimr directive))
                ((> (+ triml trimr) (length body))
                 (error "combined trim (%d + %d) greater than body length (%d) for `%s'"
                        triml trimr (length body) directive)))
               (if new
                   (list (cl-mapcan resolve body))
                 (cl-mapcan resolve (nthcdr triml (butlast body trimr))))))
            ((quote el-patch-let)
             (let ((bindings (nth 1 form))
                   (body (nth 2 form)))
               (cond
                ((<= (length form) 2)
                 (error "not enough arguments (%d) for `el-patch-let'"
                        (1- (length form))))
                ((>= (length form) 4)
                 (error "too many arguments (%d) for `el-patch-let'"
                        (1- (length form))))
                ((not (listp bindings))
                 (error "non-list (%s) as first argument for `el-patch-let'"
                        bindings)))
               (el-patch--with-puthash table
                   (mapcar (lambda (kv)
                             (unless (symbolp (car kv))
                               (error "non-symbol (%s) as binding for `el-patch-let'"
                                      (car kv)))
                             (list (car kv)
                                   (funcall resolve (cadr kv))))
                           bindings)
                 (funcall resolve body))))
            ((quote el-patch-literal)
             (when (<= (length form) 1)
               (error "not enough arguments (%d) for `el-patch-literal'"
                      (1- (length form))))
             (cdr form))
            (_ (list (cl-mapcan resolve form)))))
      (or (gethash form table)
          (list form)))))

(defun el-patch--resolve-definition (patch-definition new)
  "Resolves a patch DEFINITION, a list starting with `defun',
`defmacro', etc., returning a list of the same format. Resolves
in favor of the original version if NEW is nil; otherwise
resolves in favor of the new version."
  (cl-mapcan (lambda (form)
               (el-patch--resolve form new))
             patch-definition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Applying patches

(defun el-patch--definition (patch-definition)
  "Activates a PATCH-DEFINITION, a list starting with `defun',
`defmacro', etc., installing the new function definition and
updating `el-patch--patches'."
  (let ((name (cadr patch-definition)))
    (puthash name patch-definition el-patch--patches)
    (eval (el-patch--resolve-definition patch-definition t))))

;;;###autoload
(defmacro el-patch-defun (&rest args)
  "Patch a function. The ARGS are the same as for `defun'."
  (declare (indent defun))
  `(el-patch--definition ,(cons 'defun args)))

;;;###autoload
(defmacro el-patch-defmacro (&rest args)
  "Patch a macro called NAME. The rest of the ARGS are the same
as in `defmacro'."
  (declare (indent defun))
  `(el-patch--definition ,(cons 'defmacro args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Patch directives

;;;###autoload
(defmacro el-patch-add (&rest args)
  "Patch directive. In the original definition, the ARGS and
their containing form are removed. In the new definition, the
ARGS are spliced into the containing s-expression."
  (declare (indent 0))
  (error "Can't use `el-patch-add' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-remove (&rest args)
  "Patch directive. In the original definition, the ARGS are
spliced into the containing s-expression. In the new definition,
the ARGS and their containing form are removed."
  (declare (indent 0))
  (error "Can't use `el-patch-remove' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-swap (old new)
  "Patch directive. In the original definition, OLD is spliced
into the containing s-expression. In the new definition, NEW is
spliced instead."
  (declare (indent 0))
  (error "Can't use `el-patch-swap' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-wrap (&optional triml &optional trimr args)
  "Patch directive. In the original definition, the ARGS are
spliced into the containing s-expression. If TRIML is an integer,
the first TRIML of the ARGS are removed first. If TRIMR is an
integer, the last TRIMR are also removed. If TRIML or TRIMR are
not integers, they are interpreted as part of ARGS. In the new
definition, the ARGS and their containing form are spliced into
the containing s-expression (but the symbol `el-patch-wrap' is
removed)."
  (declare (indent defun))
  (error "Can't use `el-patch-wrap' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-splice (&optional triml &optional trimr args)
  "Patch directive. In the original definition, the ARGS and
their containing form are spliced into the containing
s-expression (but the symbol `el-patch-splice' is removed). In
the new definition, the ARGS are spliced into the containing
s-expression. If TRIML is an integer, the first TRIML of the ARGS
are removed first. If TRIMR is an integer, the last TRIMR are
also removed. If TRIML or TRIMR are not integers, they are
interpreted as part of ARGS."
  (declare (indent defun))
  (error "Can't use `el-patch-splice' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-let (varlist arg)
  "Patch directive. Creates local bindings according to VARLIST,
then resolves to ARG in both the original and new definitions.
You may bind symbols that are also patch directives, but the
bindings will not have effect if the symbols are used at the
beginning of a list (they will act as patch directives)."
  (declare (indent 1))
  (error "Can't use `el-patch-let' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-literal (arg)
  "Patch directive. Resolves to ARG, which is not processed
further by el-patch."
  (declare (indent 0))
  (error "Can't use `el-patch-literal' outside of an `el-patch'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Validating patches

(defun el-patch--find-function (name)
  "Returns the Lisp form that defines the function NAME, or nil
if such a definition cannot be found. (That would happen if the
definition were generated dynamically, or the function is defined
in the C code.)"
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
                           (advice-add #'get-file-buffer :override
                                       #'ignore)
                           (find-function-noselect name 'lisp-only)
                           (advice-remove #'get-file-buffer #'ignore))))
         (defun-buffer (car buffer-point))
         (defun-point (cdr buffer-point)))
    (and defun-buffer
         defun-point
         (with-current-buffer defun-buffer
           (save-excursion
             (goto-char defun-point)
             (read defun-buffer))))))

;;;###autoload
(defun validate-patches ()
  "Validate all the patches that have been defined in the current
Emacs session. This means el-patch will attempt to find the
original definition for each patched function, and verify that it
is the same as the original function assumed by the patch. A
warning will be signaled if the original definition for a patched
function cannot be found, or if there is a different between the
actual and expected original definitions."
  (interactive)
  (let ((any-patches nil))
    (maphash (lambda (name patch-definition)
               (setq any-patches t)
               (let ((old-definition (el-patch--resolve-definition
                                      patch-definition nil))
                     (actual-definition (el-patch--find-function name)))
                 (cond
                  ((not actual-definition)
                   (display-warning
                    'el-patch
                    "Could not find definition of `%S'" name))
                  ((not (equal old-definition actual-definition))
                   (display-warning
                    'el-patch
                    (concat "Definition of `%S' differs from what "
                            "is assumed by its patch"))))))
             el-patch--patches)
    (unless any-patches
      (user-error "No patches defined"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Viewing patches

(defun el-patch--select-patch ()
  "Use `completing-read' to select a function that has been
patched using el-patch in the current Emacs session. Returns the
patch definition, a list beginning with `defun', `defmacro',
etc."
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
  "Obtains empty buffers named NAME1 and NAME2, pretty-prints
FORM1 and FORM2 into them respectively, and run Ediff on the two
buffers."
  (with-current-buffer (get-buffer-create name1)
    (erase-buffer)
    (pp form1 (current-buffer)))
  (with-current-buffer (get-buffer-create name2)
    (erase-buffer)
    (pp form2 (current-buffer)))
  (ediff-buffers (get-buffer name1)
                 (get-buffer name2)))

;;;###autoload
(defun el-patch-ediff-patch (patch-definition)
  "Show the patch for a function in Ediff. PATCH-DEFINITION is as
returned by `el-patch--select-patch'."
  (interactive (el-patch--select-patch))
  (let ((old-definition (el-patch--resolve-definition
                         patch-definition nil))
        (new-definition (el-patch--resolve-definition
                         patch-definition t)))
    (el-patch--ediff-forms
     "*el-patch original*" old-definition
     "*el-patch patched*" new-definition)))

;;;###autoload
(defun el-patch-ediff-conflict (patch-definition)
  "Show the conflict between the expected and actual values of a
patch's original function definition in Ediff. PATCH-DEFINITION
is as returned by `el-patch--select-patch'."
  (interactive (el-patch--select-patch))
  (let* ((name (cadr patch-definition))
         (actual-definition (el-patch--find-function name))
         (expected-definition (el-patch--resolve-definition
                               patch-definition nil)))
    (when (equal actual-definition expected-definition)
      (message "No conflict"))
    (el-patch--ediff-forms
     "*el-patch actual*" actual-definition
     "*el-patch expected*" expected-definition)))

(provide 'el-patch)

;;; el-patch.el ends here

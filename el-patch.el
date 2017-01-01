;;; el-patch.el --- Future-proof your Emacs Lisp customizations

;; Copyright (C) 2016 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/el-patch
;; Keywords: extensions
;; Created: 31 Dec 2016

;;; Commentary:

;; FIXME: `el-patch-validate' needs to be smarter, so el-patch can be
;; properly idempotent in all situations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar el-patch-validate t
  "If this variable is non-nil, then el-patch will validate your
patches and issue a warning if a patched function has changed
since it was originally patched. Otherwise, validation is skipped
and patches act as simple overrides.

It is recommended that you set this variable to nil in your
init-file before loading el-patch-validate. This will improve
performance and suppress unnecessary messages. However, whenever
you upgrade Emacs or your packages, you should set this variable
to a non-nil value and reload your init-file, to see if any of
your patches might need to be updated.")

(defvar el-patches (make-hash-table :test 'equal)
  "Hash table of patches attempted. Primarily for internal use;
end users will likely prefer the interactive functions
`el-patch-inspect' and `el-patch-forget'.

The hash table maps symbols (function names) to lists of patches.
Each patch is represented as a list (patch definition, actual
function definition, expected old function definition, new
function definition).")

(defmacro el-patch--with-puthash (table kvs &rest body)
  "Binds each of the KVS (lists whose car is the key and whose
cadr is the value) in the hash TABLE, evaluates BODY, then
restores the original state of TABLE. Return value is the result
of evaluating the last form in BODY."
  (declare (indent 2))
  `(let* ((table ,table)
          (kvs ,kvs)
          (original-kvs (mapcar (lambda (kv)
                                  (list (car kv) (gethash (cadr kv) table)))
                                kvs)))
     (prog2
         (dolist (kv kvs)
           (puthash (car kv) (cadr kv) table))
         (progn ,@body)
       (dolist (kv original-kvs)
         (puthash (car kv) (cadr kv) table)))))

(defun el-patch-resolve (form new &optional table)
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
               (resolve (lambda (form) (el-patch-resolve form new table))))
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
                 (error "too many arguments (%d) for `el-patch-let'"))
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

(defun el-patch-resolve-definition (type body new)
  "Resolves a definition. TYPE is `defun', `defmacro', etc. BODY
is a list of the forms coming after TYPE in the definition.
Resolves in favor of the original version if NEW is nil;
otherwise resolves in favor of the new version."
  (cons type (cl-mapcan (lambda (form)
                          (el-patch-resolve form new))
                        body)))

(defun el-patch-symbol-function (name)
  "Like `symbol-function' but returns a Lisp form, or nil. If the
function has been byte-compiled, returns nil."
  (let ((form (symbol-function name)))
    (cond
     ((not (listp form))
      nil)
     ((equal (car form) 'macro)
      (cons 'defmacro (cons name (cddr form))))
     (t
      (cons 'defun (cons name (cdr form)))))))

(defun el-patch-find-function (name)
  "Like `find-function-noselect' but returns a Lisp form, or nil.
If the function definition cannot be found, returns nil."
  (let* ((buffer-point (condition-case nil
                           (save-excursion
                             ;; This horrifying bit of hackery
                             ;; prevents `find-function-noselect' from
                             ;; temporarily moving point when the
                             ;; definition of the function happens to
                             ;; be in a different part of the current
                             ;; buffer.
                             (advice-add #'get-file-buffer :override
                                         #'ignore)
                             (find-function-noselect name 'lisp-only)
                             (advice-remove #'get-file-buffer #'ignore))
                         ((error nil))))
         (defun-buffer (car buffer-point))
         (defun-point (cdr buffer-point)))
    (and defun-buffer
         defun-point
         (with-current-buffer defun-buffer
           (save-excursion
             (goto-char defun-point)
             (read defun-buffer))))))

(defun el-patch-definition (el-patch-type type name args)
  "Applies a patch to a definition. EL-PATCH-TYPE is
`el-patch-defun', `el-patch-defmacro', etc. TYPE is `defun',
`defmacro', etc. NAME is the name of the function being defined,
and ARGS is the remainder of the forms coming after NAME in the
body of the definition. If `el-patch-validate' is nil, validation
is skipped."
  (let ((patch (append (list el-patch-type name) args))
        (already-patched nil))
    (maphash (lambda (patch-name patches)
               (dolist (patch-data patches)
                 (when (equal patch (nth 0 patch-data))
                   (setq already-patched t))))
             el-patches)
    (unless el-patch-validate
      (let ((old-definition (el-patch-resolve-definition
                             type (cons name args) nil))
            (new-definition (el-patch-resolve-definition
                             type (cons name args) t))
            (actual-definition nil))
        (unless (when el-patch-validate
                  (setq actual-definition (or (el-patch-symbol-function name)
                                              (el-patch-find-function name)))
                  (or (unless actual-definition
                        (display-warning
                         'el-patch
                         (format (concat "could not find definition "
                                         "of `%S', not patching")
                                 name))
                        t)
                      (unless (equal actual-definition
                                     old-definition)
                        (display-warning
                         'el-patch
                         (format (concat "definition of `%S' has changed, not"
                                         " patching (see `el-patch-inspect')")
                                 name))
                        t)))
          (eval new-definition))
        (puthash name
                 ;; Add element to end of list without using
                 ;; `append-to-list'.
                 (append (gethash name el-patches)
                         (list
                          (list patch
                                actual-definition
                                old-definition
                                new-definition)))
                 el-patches)))))

(defmacro el-patch-defun (name &rest args)
  "Patch a function called NAME. The rest of the ARGS are the
same as in `defun'."
  (declare (indent defun))
  `(el-patch-definition 'el-patch-defun 'defun ',name ',args))

(defmacro el-patch-defmacro (name &rest args)
  "Patch a macro called NAME. The rest of the ARGS are the same
as in `defmacro'."
  (declare (indent defun))
  `(el-patch-definition 'el-patch-defmacro 'defmacro ',name ',args))

(defmacro el-patch-add (&rest args)
  "Patch directive. In the original definition, the ARGS and
their containing form are removed. In the new definition, the
ARGS are spliced into the containing s-expression."
  (declare (indent 0))
  (error "Can't use `el-patch-add' outside of an `el-patch'"))

(defmacro el-patch-remove (&rest args)
  "Patch directive. In the original definition, the ARGS are
spliced into the containing s-expression. In the new definition,
the ARGS and their containing form are removed."
  (declare (indent 0))
  (error "Can't use `el-patch-remove' outside of an `el-patch'"))

(defmacro el-patch-swap (old new)
  "Patch directive. In the original definition, OLD is spliced
into the containing s-expression. In the new definition, NEW is
spliced instead."
  (declare (indent 0))
  (error "Can't use `el-patch-swap' outside of an `el-patch'"))

(defmacro el-patch-wrap (&optional triml &optional trimr &rest args)
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

(defmacro el-patch-splice (&optional triml &optional trimr &rest args)
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

(defmacro el-patch-let (varlist arg)
  "Patch directive. Creates local bindings according to VARLIST,
then resolves to ARG in both the original and new definitions.
You may bind symbols that are also patch directives, but the
bindings will not have effect if the symbols are used at the
beginning of a list (they will act as patch directives)."
  (declare (indent 1))
  (error "Can't use `el-patch-let' outside of an `el-patch'"))

(defmacro el-patch-literal (arg)
  "Patch directive. Resolves to ARG, which is not processed
further by el-patch."
  (declare (indent 0))
  (error "Can't use `el-patch-literal' outside of an `el-patch'"))

(defun el-patch-select-patch ()
  "Utility function to select a patch using `completing-read'.
Returns a list (function name, patch index), which can be used as
the arguments to `el-patch-inspect' or `el-patch-forget'."
  (let ((options nil))
    (maphash (lambda (name patches)
               (if (= (length patches) 1)
                   (push (cons (symbol-name name)
                               (list name 1))
                         options)
                 (dolist (x (number-sequence 1 (length patches)))
                   (push (cons (format "%S<%d>" name x)
                               (list name x))
                         options))))
             el-patches)
    (unless options
      (user-error "No patches"))
    (setq options (sort options
                        (lambda (x y)
                          (string< (car x)
                                   (car y)))))
    (let* ((match (completing-read
                   "Which patch? "
                   (mapcar 'car options)
                   (lambda (elt) t) 'require-match)))
      (or (cdr (cl-find match options :key 'car :test 'equal))
          (error "Unexpected internal error in `el-patch-inspect'")))))

(defun el-patch-inspect (name index)
  "Ediff the actual function definition for a patch against the
definition expected by the patch. NAME and INDEX are as returned
by `el-patch-select-patch'."
  (interactive (el-patch-select-patch))
  (let ((patch-data (nth (1- index) (gethash name el-patches))))
    (if (not patch-data)
        (user-error "No el-patch for `%S'" name)
      (with-current-buffer (get-buffer-create "*el-patch actual*")
        (erase-buffer)
        (pp (nth 1 patch-data) (current-buffer)))
      (with-current-buffer (get-buffer-create "*el-patch expected*")
        (erase-buffer)
        (pp (nth 2 patch-data) (current-buffer)))
      (ediff-buffers (get-buffer "*el-patch actual*")
                     (get-buffer "*el-patch expected*")))))

(defun el-patch-forget (name index)
  "Forget about a patch, so that it will be applied again the
next time the patch definition is evaluated. NAME and INDEX are
as returned by `el-patch-select-patch'."
  (interactive (el-patch-select-patch))
  (puthash name
           (cl-delete-if (lambda (_) t) (gethash name el-patches)
                         :start (1- index) :count 1)
           el-patches))

;;; el-patch.el ends here

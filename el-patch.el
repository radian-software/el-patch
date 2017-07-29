;;; el-patch.el --- Future-proof your Emacs Lisp customizations!

;; Copyright (C) 2016 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 31 Dec 2016
;; Homepage: https://github.com/raxod502/el-patch
;; Keywords: extensions
;; Package-Requires: ((emacs "25"))
;; Version: 1.2

;;; Commentary:

;; el-patch allows you to override Emacs Lisp functions in a
;; future-proof way. Specifically, you can override a function by
;; providing an s-expression-based "patch", from which the "original"
;; and "modified" definitions can both be computed -- just like a Git
;; patch.

;; The "modified" definition is what is actually evaluated in your
;; init-file, but at any time you can ask el-patch to look up the
;; actual definition of the function and compare it to the patch's
;; "original" definition. If there is a difference -- meaning that the
;; original function definition was updated since you created the
;; patch -- el-patch will show you with Ediff. This means you know
;; when you might need to update your customizations (this is the
;; future-proof part).

;; el-patch also provides a powerful mechanism to help you lazy-load
;; packages. If you want to use a function from a package without
;; triggering its autoload (for instance, activating a minor mode or
;; defining keybindings), you can just copy its definition to your
;; init-file and declare it as a patch. Then you can freely use the
;; function, but you will still be notified of updates to the original
;; definition by el-patch so you will know when to update your copy of
;; the definition.

;; Please see https://github.com/raxod502/el-patch for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x occur with a query of four
;; semicolons followed by a space.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Libraries

(require 'subr-x)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-facing variables

(defgroup el-patch nil
  "Future-proof your Emacs Lisp customizations!"
  :prefix "el-patch-"
  :group 'lisp
  :link '(url-link :tag "GitHub" "https://github.com/raxod502/el-patch")
  :link '(emacs-commentary-link :tag "Commentary" "el-patch"))

(defcustom el-patch-use-aggressive-defvar nil
  "When patching `defvar' or similar, override existing values.
This means that `el-patch-defvar', `el-patch-defconst', and
`el-patch-defcustom' will unbind the old variable definition
before evaluating the new one."
  :type 'boolean
  :group 'el-patch)

(defcustom el-patch-require-function #'require
  "Function to `require' a feature in `el-patch-pre-validate-hook'.
This is passed all of the arguments of `el-patch-feature' as
quoted literals, and it should load the feature. This function
might be useful if, for example, some of your features are
provided by lazy-installed packages, and those packages need to
be installed before the features can be loaded."
  :type 'function
  :group 'el-patch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal variables

;;;###autoload
(defvar el-patch--patches (make-hash-table :test 'equal)
  "Hash table of patches that have been defined.
The keys are symbols naming the objects that have been patched.
The values are hash tables mapping definition types (symbols
`defun', `defmacro', etc.) to patch definitions, which are lists
beginning with `defun', `defmacro', etc.

Note that the symbols are from the versions of patches that have
been resolved in favor of the modified version, when a patch
renames a symbol.")

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
               (if this-new
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
  (cons (car patch-definition)
        (cl-mapcan (lambda (form)
                     (el-patch--resolve form new))
                   (cdr patch-definition))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Validating patches

(defcustom el-patch-pre-validate-hook nil
  "Hook run before `el-patch-validate-all'.
Also run before `el-patch-validate' if a prefix argument is
provided. This hook should contain functions that make sure all
of your patches are defined (for example, you might need to load
some features if your patches are lazily defined)."
  :type 'hook
  :group 'el-patch)

(defcustom el-patch-post-validate-hook nil
  "Hook run after `el-patch-validate-all'.
Also run after `el-patch-validate' if a prefix argument is
provided. This hook should contain functions that undo any
patching that might have taken place in
`el-patch-pre-validate-hook', if you do not want the patches to
be defined permanently."
  :type 'hook
  :group 'el-patch)

(defun el-patch--classify-definition-type (type)
  "Classifies a definition TYPE as a `function' or `variable'.
TYPE is a symbol `defun', `defmacro', etc."
  (pcase type
    ((or 'defun 'defmacro 'defsubst 'define-minor-mode)
     'function)
    ((or 'defvar 'defconst 'defcustom)
     'variable)
    (_ (error "Unexpected definition type %S" type))))

(defun el-patch--find-symbol (name type)
  "Return the Lisp form that defines the symbol NAME.
Return nil if such a definition cannot be found. (That would
happen if the definition were generated dynamically.) TYPE is a
symbol `defun', `defmacro', etc. which is used to determine
whether the symbol is a function or variable."
  (let ((classification (el-patch--classify-definition-type type)))
    (when (pcase classification
            ('function (fboundp name))
            ('variable (boundp name)))
      (let* (;; Since Emacs actually opens the source file in a (hidden)
             ;; buffer, it can try to apply local variables, which might
             ;; result in an annoying interactive prompt. Let's disable
             ;; that.
             (enable-local-variables nil)
             (enable-dir-local-variables nil)
             ;; This is supposed to be noninteractive so we also
             ;; suppress all the messages. This has the side effect of
             ;; masking all debugging messages (you can use `insert'
             ;; instead, or temporarily remove these bindings), but
             ;; there are just so many different messages that can
             ;; happen for various reasons and I haven't found any other
             ;; standard way to suppress them.
             (inhibit-message t)
             (message-log-max nil)
             ;; Now we actually do the find-function operation.
             (buffer-point (save-excursion
                             ;; This horrifying bit of hackery on
                             ;; `get-file-buffer' prevents
                             ;; `find-function-noselect' from
                             ;; returning an existing buffer, so that
                             ;; later on when we jump to the
                             ;; definition, we don't temporarily
                             ;; scroll the window if the definition
                             ;; happens to be in the *current* buffer.
                             (cl-letf (((symbol-function #'get-file-buffer)
                                        (symbol-function #'ignore)))
                               (pcase classification
                                 ('function
                                  (find-function-noselect name 'lisp-only))
                                 ('variable
                                  (find-variable-noselect name))))))
             (defun-buffer (car buffer-point))
             (defun-point (cdr buffer-point)))
        (and defun-buffer
             defun-point
             (with-current-buffer defun-buffer
               (save-excursion
                 (goto-char defun-point)
                 (read defun-buffer))))))))

;;;###autoload
(defun el-patch-validate (name type &optional nomsg run-hooks)
  "Validate the patch with given NAME and TYPE.
This means el-patch will attempt to find the original definition
for the function, and verify that it is the same as the original
function assumed by the patch. A warning will be signaled if the
original definition for a patched function cannot be found, or if
there is a difference between the actual and expected original
definitions.

Interactively, use `completing-read' to select a function to
inspect the patch of.

NAME is a symbol naming the object being patched; TYPE is a
symbol `defun', `defmacro', etc.

Returns nil if the patch is not valid, and otherwise returns t.
If NOMSG is non-nil, does not signal a message when the patch is
valid.

If RUN-HOOKS is non-nil, runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'. Interactively, this happens unless
a prefix argument is provided.

See also `el-patch-validate-all'."
  (interactive (progn
                 (unless current-prefix-arg
                   (run-hooks 'el-patch-pre-validate-hook))
                 (append (el-patch--select-patch)
                         (list nil (unless current-prefix-arg
                                     'post-only)))))
  (unless (member run-hooks '(nil post-only))
    (run-hooks 'el-patch-pre-validate-hook))
  (unwind-protect
      (progn
        (let* ((patch-definition (el-patch-get name type))
               (type (car patch-definition))
               (expected-definition (el-patch--resolve-definition
                                     patch-definition nil))
               (name (cadr expected-definition))
               (actual-definition (el-patch--find-symbol name type)))
          (cond
           ((not actual-definition)
            (display-warning
             'el-patch
             (format "Could not find definition of %S `%S'" type name))
            nil)
           ((not (equal expected-definition actual-definition))

            (display-warning
             'el-patch
             (format (concat "Definition of %S `%S' differs from what "
                             "is assumed by its patch")
                     type name))
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
        (dolist (name (hash-table-keys el-patch--patches))
          (let ((patch-hash (gethash name el-patch--patches)))
            (dolist (type (hash-table-keys patch-hash))
              (setq patch-count (1+ patch-count))
              (unless (el-patch-validate name type 'nomsg)
                (setq warning-count (1+ warning-count))))))
        (cond
         ((zerop patch-count)
          (user-error "No patches defined"))
         ((zerop warning-count)
          (if (= patch-count 1)
              (message "Patch is valid (only one defined)")
            (message "All %d patches are valid" patch-count)))
         ((= patch-count warning-count)
          (if (= patch-count 1)
              (message "Patch is invalid (only one defined)")
            (message "All %d patches are invalid" patch-count)))
         (t
          (message "%s valid, %s invalid"
                   (if (= warning-count (1- patch-count))
                       "1 patch is"
                     (format "%d patches are" (- patch-count warning-count)))
                   (if (= warning-count 1)
                       "1 patch is"
                     (format "%d patches are" warning-count))))))
    (run-hooks 'el-patch-post-validate-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Applying patches

(defun el-patch--compute-load-history-items (definition)
  "Determine the items that DEFINITION will add to the `load-history'.
Return a list of those items. Beware, uses heuristics."
  (cl-destructuring-bind (type name . body) definition
    (pcase type
      ((or 'defun 'defmacro 'defsubst)
       (list (cons 'defun name)))
      ((or 'defvar 'defconst 'defcustom)
       (list name))
      ((quote define-minor-mode)
       (list (cons 'defun name)
             (or (when-let ((rest (member :variable body)))
                   (cadr rest))
                 name)))
      (_ (error "Unexpected definition type %S" type)))))

(defmacro el-patch--stealthy-eval (definition)
  "Evaluate DEFINITION without updating `load-history'.
DEFINITION should be a list beginning with `defun', `defmacro',
`define-minor-mode', etc."
  (let ((items (cl-remove-if (lambda (item)
                               (member item current-load-list))
                             (el-patch--compute-load-history-items
                              definition))))
    `(progn
       ,@(when (and el-patch-use-aggressive-defvar
                    (eq (el-patch--classify-definition-type
                         (car definition))
                        'variable))
           ;; Note that this won't necessarily handle `define-minor-mode'
           ;; correctly if a custom `:variable' is specified. However, I'm
           ;; not going to handle that edge case until somebody else
           ;; complains about it.
           (list `(makunbound ,(cadr definition))))
       ,definition
       ,@(cl-loop for item in items collect
                  `(setq current-load-list
                         (remove ',item current-load-list))))))

(defmacro el-patch--definition (patch-definition)
  "Activate a PATCH-DEFINITION and update `el-patch--patches'.
PATCH-DEFINITION is a list starting with `defun', `defmacro',
etc., which may contain patch directives."
  ;; First we resolve the patch definition in favor of the modified
  ;; version, because that is the version we need to activate (no
  ;; validation happens here).
  (let ((definition (el-patch--resolve-definition patch-definition t)))
    ;; Then we parse out the definition type and symbol name.
    (cl-destructuring-bind (type name . body) definition
      `(progn
         ;; Register the patch in our hash. We want to do this right away
         ;; so that if there is an error then at least the user can undo
         ;; the patch (as long as it is not too terribly wrong).
         (unless (gethash ',name el-patch--patches)
           (puthash ',name (make-hash-table :test #'equal) el-patch--patches))
         (puthash ',type ',patch-definition (gethash ',name el-patch--patches))
         ;; Now we actually overwrite the current definition.
         (el-patch--stealthy-eval ,definition)))))

;; Function-like objects.

;;;###autoload
(defmacro el-patch-defun (&rest args)
  "Patch a function. The ARGS are the same as for `defun'."
  (declare (doc-string 3)
           (indent defun))
  `(el-patch--definition ,(cons #'defun args)))

;;;###autoload
(defmacro el-patch-defmacro (&rest args)
  "Patch a macro. The ARGS are the same as for `defmacro'."
  (declare (doc-string 3)
           (indent defun))
  `(el-patch--definition ,(cons #'defmacro args)))

;;;###autoload
(defmacro el-patch-defsubst (&rest args)
  "Patch an inline function. The ARGS are the same as for `defsubst'."
  (declare (doc-string 3)
           (indent defun))
  `(el-patch--definition ,(cons #'defsubst args)))

;; Variable-like objects.

;;;###autoload
(defmacro el-patch-defvar (&rest args)
  "Patch a variable. The ARGS are the same as for `defvar'."
  (declare (indent defun))
  `(el-patch--definition ,(cons #'defvar args)))

;;;###autoload
(defmacro el-patch-defconst (&rest args)
  "Patch a constant. The ARGS are the same as for `defconst'."
  (declare (indent defun))
  `(el-patch--definition ,(cons #'defconst args)))

;;;###autoload
(defmacro el-patch-defcustom (&rest args)
  "Patch a customizable variable. The ARGS are the same as for `defcustom'."
  (declare (indent defun))
  `(el-patch--definition ,(cons #'defcustom args)))

;; Other objects.

;;;###autoload
(defmacro el-patch-define-minor-mode (&rest args)
  "Patch a minor mode. The ARGS are the same as for `define-minor-mode'."
  (declare (doc-string 2)
           (indent defun))
  `(el-patch--definition ,(cons #'define-minor-mode args)))

;; For convenience.

;;;###autoload
(defmacro el-patch-feature (feature &rest args)
  "Declare that some patches are only defined after FEATURE is loaded.
This is a convenience macro that creates a function for invoking
`require' on that feature, and then adds it to
`el-patch-pre-validate-hook' so that your patches are loaded and
`el-patch' can properly validate them.

FEATURE should be an unquoted symbol. ARGS, if given, are passed
as quoted literals along with FEATURE to
`el-patch-require-function' when `el-patch-validate-all' is
called."
  (let ((defun-name (intern (format "el-patch-require-%S" feature))))
    `(progn
       (defun ,defun-name ()
         (apply el-patch-require-function ',feature ',args))
       (add-hook 'el-patch-pre-validate-hook #',defun-name))))

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

;;;###autoload
(defun el-patch-get (name type)
  "Return the patch for object NAME of the given TYPE.
NAME is a symbol for the name of the definition that was patched,
and TYPE is a symbol `defun', `defmacro', etc. If the patch could
not be found, return nil."
  (condition-case nil
      (gethash type (gethash name el-patch--patches))
    (error nil)))

(defun el-patch--select-patch ()
  "Use `completing-read' to select a patched function.
Return a list of two elements, the name (a symbol) of the object
being patched and the type (a symbol `defun', `defmacro', etc.)
of the definition."
  (let ((options (mapcar #'symbol-name (hash-table-keys el-patch--patches))))
    (unless options
      (user-error "No patches defined"))
    (let* ((name (intern (completing-read
                          "Which patch? "
                          options
                          (lambda (elt) t)
                          'require-match)))
           (patch-hash (gethash name el-patch--patches))
           (options (mapcar #'symbol-name
                            (hash-table-keys patch-hash))))
      (list name
            (intern (pcase (length options)
                      (0 (error "Internal `el-patch' error"))
                      (1 (car options))
                      (_ (completing-read
                          "Which version? "
                          options
                          (lambda (elt) t)
                          'require-match))))))))

(defun el-patch--ediff-forms (name1 form1 name2 form2)
  "Ediff two forms.
Obtain and empty buffer named NAME1 and pretty-print FORM1 into
it. Do the same for NAME2 and FORM2, and then run Ediff on the
two buffers wordwise."
  (let (min1 max1 min2 max2)
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
(defun el-patch-ediff-patch (name type)
  "Show the patch for an object in Ediff.
NAME and TYPE are as returned by `el-patch-get'."
  (interactive (el-patch--select-patch))
  (if-let ((patch-definition (el-patch-get name type)))
      (let* ((old-definition (el-patch--resolve-definition
                              patch-definition nil))
             (new-definition (el-patch--resolve-definition
                              patch-definition t)))
        (el-patch--ediff-forms
         "*el-patch original*" old-definition
         "*el-patch patched*" new-definition)
        (when (equal old-definition new-definition)
          (message "No patch")))
    (error "There is no patch for %S %S" type name)))

;;;###autoload
(defun el-patch-ediff-conflict (name type)
  "Show a patch conflict in Ediff.
This is a diff between the expected and actual values of a
patch's original definition. NAME and TYPE are as returned by
`el-patch-get'."
  (interactive (el-patch--select-patch))
  (if-let ((patch-definition (el-patch-get name type)))
      (let* ((expected-definition (el-patch--resolve-definition
                                   patch-definition nil))
             (name (cadr expected-definition))
             (type (car expected-definition))
             (actual-definition (el-patch--find-symbol name type)))
        (el-patch--ediff-forms
         "*el-patch actual*" actual-definition
         "*el-patch expected*" expected-definition)
        (when (equal actual-definition expected-definition)
          (message "No conflict")))
    (error "There is no patch for %S %S" type name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Removing patches

;;;###autoload
(defun el-patch-unpatch (name type)
  "Remove the patch given by the PATCH-DEFINITION.
This restores the original functionality of the object being
patched. NAME and TYPE are as returned by `el-patch-get'."
  (interactive (el-patch--select-patch))
  (if-let ((patch-definition (el-patch-get name type)))
      (eval `(el-patch--stealthy-eval ,(el-patch--resolve-definition
                                        patch-definition nil)))
    (error "There is no patch for %S %S" type name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Closing remarks

(provide 'el-patch)

;;; el-patch.el ends here

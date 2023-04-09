;;; el-patch.el --- Future-proof your Elisp -*- lexical-binding: t -*-

;; Copyright (C) 2016-2022 Radian LLC and contributors

;; Author: Radian LLC <contact+el-patch@radian.codes>
;; Created: 31 Dec 2016
;; Homepage: https://github.com/radian-software/el-patch
;; Keywords: extensions
;; Package-Requires: ((emacs "26"))
;; SPDX-License-Identifier: MIT
;; Version: 3.0

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

;; Please see https://github.com/radian-software/el-patch for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(defvar use-package-keywords)

(declare-function use-package-normalize-forms "use-package")
(declare-function use-package-process-keywords "use-package")

;;;; User-facing variables

;;;###autoload
(defgroup el-patch nil
  "Future-proof your Emacs Lisp customizations!"
  :prefix "el-patch-"
  :group 'lisp
  :link '(url-link :tag "GitHub" "https://github.com/radian-software/el-patch")
  :link '(emacs-commentary-link :tag "Commentary" "el-patch"))

(defcustom el-patch-use-aggressive-defvar nil
  "When patching `defvar' or similar, override existing values.
This means that `el-patch-defvar', `el-patch-defconst', and
`el-patch-defcustom' will unbind the old variable definition
before evaluating the new one."
  :type 'boolean)

(defun el-patch-default-require-function (feature &rest _args)
  "Invoke `require' on FEATURE, printing warning if it is unavailable.
This is the default value for `el-patch-require-function'."
  (condition-case-unless-debug e
      (require feature)
    (error (display-warning
            'el-patch
            (format "On el-patch-pre-validate-hook: %s"
                    (error-message-string e))
            :error))))

(defcustom el-patch-require-function #'el-patch-default-require-function
  "Function to `require' a feature in `el-patch-pre-validate-hook'.
This is passed all of the arguments of `el-patch-feature' as
quoted literals, and it should load the feature. This function
might be useful if, for example, some of your features are
provided by lazy-installed packages, and those packages need to
be installed before the features can be loaded."
  :type 'function)

(defcustom el-patch-deftype-alist nil
  "Alist of types of definitions that can be patched with `el-patch'.
The keys are definition types, like `defun', `define-minor-mode',
etc. The values are plists; the following keywords are accepted:

`:classify' - a function which may be called with a full
definition form (a list starting with e.g. `defun') and which
returns an alist detailing what it defines. In this alist, the
keys are symbols; only the values `function' and `variable' are
allowed. The values are names of functions and variables,
respectively, that are defined by the definition form. This
argument is mandatory.

`:locate' - a function which may be called with a full definition
form (a list starting with e.g. `defun') and which returns the
actual source code of the definition, as a list. If the patch
correct and up to date, then this will actually be the same as
the definition which was passed in. This argument is optional,
but required if you want patch validation to work.

`:declare' - a list to be put in a `declare' form of the
resulting `el-patch' macro, like:

    ((doc-string 2) (indent defun))

This argument is optional.

`:macro-name' - normally the name of the macro generated for
patching a `defun' is called `el-patch-defun', but you can
override that by providing this argument. This argument is
optional.

`:font-lock' - a function that can be called to set up font-lock
keywords (e.g., by calling `font-lock-add-keywords' with some
appropriate arguments). The function is called with one argument,
the macro name (e.g. `el-patch-defun')."
  :type '(alist
          :key-type symbol
          :value-type (plist
                       :key-type (choice
                                  (const :classify)
                                  (const :locate)
                                  (const :declare)
                                  (const :macro-name))
                       :value-type sexp)))

(defcustom el-patch-enable-use-package-integration t
  "Non-nil means to automatically enable `use-package' integration.
This variable has an effect only when the `el-patch' library is
loaded. You can toggle the `use-package' integration later using
\\[el-patch-use-package-mode]."
  :type 'boolean)

(defcustom el-patch-validate-during-compile nil
  "Non-nil means to validate patches when byte-compiling."
  :type 'boolean)

(defcustom el-patch-use-advice nil
  "Non-nil causes el-patch to use Emacs' advice system for patching functions.
This can be set globally or bound dynamically around a patch.

An advice is used if the patched function has the same name and
the same number of arguments as the original.

An advice takes precedence over subsequent non-advice patches.
You may need to un-advice or un-patch a function to apply a new
patch."
  :type 'boolean)

;;;; Internal variables

(defvar el-patch-variant nil
  "Advanced variable for defining patch variants.
This variable may be used to define multiple different patches
for the same object, and have them all be validated by
`el-patch'. Usage: dynamically bind this variable around an
invocation of `el-patch-defun', etc. As long as each patch
definition for the same object uses a different value for this
variable (including its default, nil), the patches will be
distinguishable (and hence can be validated) by `el-patch', even
though only the most recently defined one will actually take
effect.

Common use case: defining a separate patch for the same function
before and after a library is loaded.")

(defvar el-patch--patches (make-hash-table :test 'equal)
  "Hash table of patches that have been defined.
This is a three-level hash table. The first-level keys are
symbols naming the objects that have been patched. The
second-level keys are definition types (symbols `defun',
`defmacro', etc.). The third-level keys are arbitrary symbols
\(always nil, unless `el-patch-variant' has been used). The values
are patch definitions, which are lists beginning with `defun',
`defmacro', etc.

Note that the object name symbols are from the versions of
patches that have been resolved in favor of the modified version,
when a patch renames a symbol.")

(defvar el-patch--not-present (make-symbol "el-patch--not-present")
  "Value used as a default argument to `gethash'.")

(defvar el-patch--concat-function #'concat
  "Function to concatenate strings when resolving patches.")

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
         (if (eq (car kv) el-patch--not-present)
             (remhash (car kv) table)
           (puthash (car kv) (cadr kv) table))))))

(defun el-patch--copy-semitree (tree)
  "Copy the list TREE, and return the copy. The list may be improper.
This function lives halfway between `copy-sequence' and
`copy-tree', since it only recurses into cdrs."
  (if (consp tree)
      (cons (car tree) (el-patch--copy-semitree (cdr tree)))
    tree))

(defun el-patch--advice-name (name variant-name)
  "Return advice name for a given NAME and VARIANT-NAME."
  (intern
   (format "%S@%s@el-patch--advice"
           name
           (if variant-name (format "%S" el-patch-variant) ""))))

(defun el-patch--resolve (form new &optional table)
  "Resolve a patch FORM.
Return a list of forms to be spliced into the surrounding
s-expression. Resolve in favor of the original version if NEW is
nil; otherwise resolve in favor of the new version. TABLE is a
hash table of `el-patch-let' bindings, which maps symbols to
their bindings."
  (let ((table (or table (make-hash-table :test 'equal))))
    (cond
     ((consp form)
      (let* ((directive (car form))
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
               (error (concat "Combined trim (%d + %d) greater "
                              "than body length (%d) for `%s'")
                      triml trimr (length body) directive)))
             (if this-new
                 (list (cl-mapcan resolve body))
               (cl-mapcan resolve (nthcdr triml (butlast body trimr))))))
          ((quote el-patch-let)
           (let ((bindings (nth 1 form))
                 (body (nthcdr 2 form)))
             (cond
              ((<= (length form) 2)
               (error "Not enough arguments (%d) for `el-patch-let'"
                      (1- (length form))))
              ((not (listp bindings))
               (error "Non-list (%s) as first argument for `el-patch-let'"
                      bindings)))
             (el-patch--with-puthash table
                 (mapcar
                  (lambda (kv)
                    (unless (symbolp (car kv))
                      (error "Non-symbol (%s) as binding for `el-patch-let'"
                             (car kv)))
                    (list (car kv)
                          (funcall resolve (cadr kv))))
                  bindings)
               (cl-mapcan resolve body))))
          ((quote el-patch-literal)
           (when (<= (length form) 1)
             (error "Not enough arguments (%d) for `el-patch-literal'"
                    (1- (length form))))
           (cdr form))
          ((quote el-patch-concat)
           (when (<= (length form) 1)
             (error "Not enough arguments (%d) for `el-patch-concat'"
                    (1- (length form))))
           (list (apply el-patch--concat-function
                        (cl-mapcan resolve (cdr form)))))
          (_
           (let ((car-forms (funcall resolve (car form)))
                 (cdr-forms (funcall resolve (cdr form))))
             (cond
              ((null car-forms) cdr-forms)
              ((null cdr-forms) car-forms)
              (t
               (let ((forms (nconc car-forms (butlast cdr-forms))))
                 (setf (nthcdr (length forms) forms)
                       (car (last cdr-forms)))
                 (list forms)))))))))
     ((vectorp form)
      (list
       (seq-mapcat
        (lambda (subform)
          (el-patch--resolve subform new table))
        form
        'vector)))
     (t
      (or
       ;; Copy since otherwise we may end up with the same list object
       ;; returned multiple times, which is not okay since lists
       ;; returned by this function may be modified destructively.
       (el-patch--copy-semitree (gethash form table))
       (list form))))))

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

;;;; Patch directives

;;;###autoload
(defmacro el-patch-add (&rest args)
  "Patch directive for inserting forms.
In the original definition, the ARGS and their containing form
are removed. In the new definition, the ARGS are spliced into the
containing s-expression."
  (declare (indent 0))
  (ignore args)
  `(error "Can't use `el-patch-add' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-remove (&rest args)
  "Patch directive for removing forms.
In the original definition, the ARGS are spliced into the
containing s-expression. In the new definition, the ARGS and
their containing form are removed."
  (declare (indent 0))
  (ignore args)
  `(error "Can't use `el-patch-remove' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-swap (old new)
  "Patch directive for swapping forms.
In the original definition, OLD is spliced into the containing
s-expression. In the new definition, NEW is spliced instead."
  (declare (indent 0))
  (ignore old new)
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
  (ignore triml trimr args)
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
  (ignore triml trimr args)
  `(error "Can't use `el-patch-splice' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-let (varlist &rest args)
  "Patch directive for creating local el-patch bindings.
Creates local bindings according to VARLIST, then splices ARGS
into both the original and new definitions. You may bind symbols
that are also patch directives, but the bindings will not have
effect if the symbols are used at the beginning of a list (they
will act as patch directives)."
  (declare (indent 1))
  (ignore varlist args)
  `(error "Can't use `el-patch-let' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-literal (&rest args)
  "Patch directive for treating patch directives literally.
ARGS are spliced into the containing s-expression, but are not
processed further by el-patch."
  (declare (indent 0))
  (ignore args)
  `(error "Can't use `el-patch-literal' outside of an `el-patch'"))

;;;###autoload
(defmacro el-patch-concat (&rest args)
  "Patch directive for modifying string literals.
ARGS should resolve to strings; those strings are passed to
`concat' and spliced into the containing s-expression in both the
original and new definitions."
  (declare (indent 0))
  (ignore args)
  `(error "Can't use `el-patch-concat' outside of an `el-patch'"))

;;;; Applying patches

(defmacro el-patch--stealthy-eval (definition &optional docstring-note)
  "Evaluate DEFINITION without updating `load-history'.
DEFINITION should be an unquoted list beginning with `defun',
`defmacro', `define-minor-mode', etc. DOCSTRING-NOTE, if given,
is a sentence to put in brackets at the end of the docstring."
  (let* ((type (nth 0 definition))
         (props (alist-get type el-patch-deftype-alist)))
    (unless props
      (error "Unregistered definition type `%S'" type))
    (let* ((classify (plist-get props :classify))
           (docstring-idx
            (nth 1 (assq 'doc-string (plist-get props :declare)))))
      (unless classify
        (error
         "Definition type `%S' has no `:classify' in `el-patch-deftype-alist'"
         type))
      (when (and docstring-note docstring-idx)
        (let ((old-docstring (nth docstring-idx definition)))
          (if (stringp old-docstring)
              (let ((new-docstring
                     (concat
                      old-docstring
                      (format "\n\n[%s]" docstring-note))))
                (setq definition (cl-copy-list definition))
                (setf (nth docstring-idx definition)
                      new-docstring))
            (setq definition (append
                              (butlast definition
                                       (- (length definition) docstring-idx))
                              (cons (format "[%s]" docstring-note)
                                    (nthcdr docstring-idx definition)))))))
      (let* ((classification
              (funcall classify definition))
             (items
              (cl-remove-if
               (lambda (item)
                 (member item current-load-list))
               (mapcar
                (lambda (entry)
                  (pcase (car entry)
                    (`function (cons 'defun (cdr entry)))
                    (`variable (cdr entry))
                    (_ (error
                        "Unexpected classification type `%S'" (car entry)))))
                classification))))
        `(prog2
             ;; Using a `progn' here so that the `prog2' above will
             ;; correctly cause the evaluated definition to be
             ;; returned, even if `el-patch-use-aggressive-defvar' is
             ;; nil.
             (progn
               ,@(when el-patch-use-aggressive-defvar
                   (cl-mapcan
                    (lambda (entry)
                      (when (eq (car entry) 'variable)
                        `((makunbound ',(cdr entry)))))
                    classification)))
             ,definition
           ,@(mapcar (lambda (item)
                       `(setq current-load-list
                              (remove ',item current-load-list)))
                     items))))))

;;;###autoload
(defmacro el-patch--definition (patch-definition)
  "Activate a PATCH-DEFINITION and update `el-patch--patches'.
PATCH-DEFINITION is an unquoted list starting with `defun',
`defmacro', etc., which may contain patch directives."
  ;; First we resolve the patch definition in favor of the modified
  ;; version, because that is the version we need to activate (no
  ;; validation happens here).
  (let ((definition (el-patch--resolve-definition patch-definition t)))
    ;; Then we parse out the definition type and symbol name.
    (cl-destructuring-bind (type name . body) definition
      (let* ((advise (and el-patch-use-advice
                          ;; Only advice functions
                          (let* ((props (alist-get type
                                                   el-patch-deftype-alist))
                                 (classifier (plist-get props :classify)))
                            (and classifier
                                 (equal
                                  (caar (funcall classifier definition))
                                  'function)))
                          ;; Patches must have the same name and
                          ;; same number of arguments
                          (let ((orig-def (el-patch--resolve-definition
                                           (cl-subseq patch-definition 0 3)
                                           nil)))
                            ;; Same name and same argument count
                            (and (equal name (nth 1 orig-def))
                                 (equal (length (nth 2 definition))
                                        (length (nth 2 orig-def)))))
                          'advice))
             (register-patch
              `(let ((table (or (bound-and-true-p el-patch--patches)
                                (make-hash-table :test #'eq))))
                 (setq el-patch--patches table)
                 (setq table
                       (puthash ',name
                                (gethash
                                 ',name table
                                 (make-hash-table :test #'eq))
                                table))
                 (setq table
                       (puthash ',type
                                (gethash
                                 ',type table
                                 (make-hash-table :test #'equal))
                                table))
                 (puthash (cons ,(when advise `(quote ,advise))
                                el-patch-variant)
                          ',patch-definition table))))
        ;; If we need to validate the patch, then we also need to
        ;; register it at compile-time, not just at runtime.
        (when (and el-patch-validate-during-compile byte-compile-current-file)
          (eval register-patch t)
          (el-patch-validate name type 'nomsg nil
                             (cons advise el-patch-variant)))
        ;; Check that `el-patch-variant' is not a cons or a string
        (when (or (consp el-patch-variant)
                  (stringp el-patch-variant))
          (error "`el-patch-variant' cannot be a string or a cons"))
        `(progn
           ;; Register the patch in our hash. We want to do this right
           ;; away so that if there is an error then at least the user
           ;; can undo the patch (as long as it is not too terribly
           ;; wrong).
           ,register-patch
           ;; Now we actually overwrite the current definition.
           ,(if advise
                ;; Use advice system
                (let ((advice-name (el-patch--advice-name name
                                                          el-patch-variant)))
                  `(progn
                     (el-patch--stealthy-eval
                      ,(append
                        (list (car definition) ;; Same type
                              advice-name)     ;; Different name
                        ;; Rest is the same
                        (cddr definition))
                      ,(format
                        ;; The new line before the name is to avoid
                        ;; long doc strings
                        "This advice was defined by `el-patch' for\n`%S'."
                        name))
                     (advice-add (quote ,name)
                                 :override (quote ,advice-name))))
              `(el-patch--stealthy-eval
                ,definition
                "This definition was patched by `el-patch'.")))))))

;;;;; Removing patches

;;;###autoload
(defun el-patch-unpatch (name type variant)
  "Remove the patch given by the PATCH-DEFINITION.
This restores the original functionality of the object being
patched. NAME, TYPE, and VARIANT are as returned by
`el-patch-get'."
  (interactive (el-patch--select-patch))
  (if-let ((patch-definition (el-patch-get name type variant)))
      (if (car variant)
          ;; an advice, remove it
          (advice-remove name
                         (el-patch--advice-name name (cdr variant)))
        ;; Otherwise just re-evaluate original definition
        (eval
         `(el-patch--stealthy-eval
           ,(el-patch--resolve-definition
             patch-definition nil)
           "This function was patched and then unpatched by `el-patch'.")))
    (error "There is no patch for %S %S" type name)))

;;;; Defining patch types

;;;###autoload
(cl-defmacro el-patch-deftype
    (type &rest kwargs &key classify locate declare macro-name font-lock)
  "Allow `el-patch' to patch definitions of the given TYPE.
TYPE is a symbol like `defun', `define-minor-mode', etc. This
updates `el-patch-deftype-alist' (which see for explanations of
CLASSIFY, LOCATE, DECLARE, MACRO-NAME, and FONT-LOCK) with the
provided KWARGS and defines a macro named like `el-patch-defun',
`el-patch-define-minor-mode', etc. (which can be overridden by
MACRO-NAME)."
  (declare (indent defun))
  (ignore locate)
  (unless classify
    (error "You must specify `:classify' in calls to `el-patch-deftype'"))
  (let ((macro-name (or macro-name (intern (format "el-patch-%S" type)))))
    `(progn
       (unless (bound-and-true-p el-patch-deftype-alist)
         (setq el-patch-deftype-alist nil))
       (setf (alist-get ',type el-patch-deftype-alist)
             ;; Make sure we don't accidentally create self-modifying
             ;; code if somebody decides to mutate
             ;; `el-patch-deftype-alist'.
             (copy-tree ',kwargs))
       ,@(when font-lock
           `((,font-lock ',macro-name)))
       (defmacro ,macro-name
           (name &rest args)
         ,(format "Use `el-patch' to override a `%S' form.
The ARGS are the same as for `%S'."
                  type type)
         ,@(when declare
             `((declare ,@declare)))
         (list #'el-patch--definition (cl-list* ',type name args))))))

;;;;; Classification functions

(defun el-patch-classify-variable (definition)
  "Classify the items defined by a variable DEFINITION.
DEFINITION is a list starting with `defvar' or similar."
  (list (cons 'variable (nth 1 definition))))

(defun el-patch-classify-function (definition)
  "Classify the items defined by a function DEFINITION.
DEFINITION is a list starting with `defun' or similar."
  (list (cons 'function (nth 1 definition))))

(defun el-patch-classify-define-minor-mode (definition)
  "Classify the items defined by a minor mode DEFINITION.
DEFINITION is a list starting with `define-minor-mode' or
similar."
  (let* ((function-name (nth 1 definition))
         (variable-name (nth 1 definition))
         (kw-args (nthcdr 3 definition)))
    (dotimes (_ 3)
      (unless (keywordp (car kw-args))
        (setq kw-args (cdr kw-args))))
    (while (keywordp (car kw-args))
      (when (eq (car kw-args) :variable)
        (setq variable-name (car kw-args)))
      (setq kw-args (nthcdr 2 kw-args)))
    (list (cons 'function function-name)
          (cons 'variable variable-name))))

;;;;; Font-lock functions

;;;###autoload
(defun el-patch-fontify-as-defun (name)
  "Fontify `el-patch' macro with given NAME as function definition."
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(concat
        (format "(\\(%S\\)\\>[[:blank:]]+\\(" name)
        lisp-mode-symbol-regexp
        "\\)[[:blank:]]")
      (2 font-lock-function-name-face)))))

;;;###autoload
(defun el-patch-fontify-as-variable (name)
  "Fontify `el-patch' macro with given NAME as variable definition."
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(concat
        (format "(\\(%S\\)\\>[[:blank:]]+\\(" name)
        lisp-mode-symbol-regexp
        "\\)[[:blank:]]")
      (2 font-lock-variable-name-face)))))

;;;;; Location functions

(defmacro el-patch-wrap-locator (&rest body)
  "Wrap the operation of `find-function-noselect' or similar.
This disables local variables and messaging, saves the current
buffer and point, etc. BODY is executed within this context. It
is assumed that BODY finds the appropriate file in a buffer using
`get-file-buffer', and then returns a cons cell with the buffer
and point for the beginning of some Lisp form. The return value
is the Lisp form, read from the buffer at point."
  (declare (indent 0))
  `(let* (;; Since Emacs actually opens the source file in a (hidden)
          ;; buffer, it can try to apply local variables, which might
          ;; result in an annoying interactive prompt. Let's disable
          ;; that.
          (enable-local-variables nil)
          (enable-dir-local-variables nil)
          ;; This is supposed to be noninteractive so we also suppress
          ;; all the messages. This has the side effect of masking all
          ;; debugging messages (you can use `insert' instead, or
          ;; temporarily remove these bindings), but there are just so
          ;; many different messages that can happen for various
          ;; reasons and I haven't found any other standard way to
          ;; suppress them.
          (inhibit-message t)
          (message-log-max nil)
          ;; Now we actually execute BODY to move point to the right
          ;; file and location.
          (buffer-point (save-excursion
                          ;; This horrifying bit of hackery on
                          ;; `get-file-buffer' prevents
                          ;; `find-function-noselect' from returning
                          ;; an existing buffer, so that later on when
                          ;; we jump to the definition, we don't
                          ;; temporarily scroll the window if the
                          ;; definition happens to be in the *current*
                          ;; buffer.
                          (cl-letf (((symbol-function #'get-file-buffer)
                                     #'ignore))
                            ;; Because we get an error if the function
                            ;; doesn't have a definition anywhere.
                            (ignore-errors
                              ,@body))))
          (defun-buffer (car buffer-point))
          (defun-point (cdr buffer-point)))
     (prog1 (and defun-buffer
                 defun-point
                 (with-current-buffer defun-buffer
                   (save-excursion
                     (goto-char defun-point)
                     (read (current-buffer)))))
       (when defun-buffer
         (kill-buffer defun-buffer)))))

(defun el-patch-locate-variable (definition)
  "Return the source code of DEFINITION.
DEFINITION is a list starting with `defvar' or similar."
  (el-patch-wrap-locator
    (find-variable-noselect (nth 1 definition))))

(defun el-patch-locate-function (definition)
  "Return the source code of DEFINITION.
DEFINITION is a list starting with `defun' or similar."
  (el-patch-wrap-locator
    (find-function-noselect (nth 1 definition) 'lisp-only)))

;;;;; Predefined patch types

;;;###autoload(require 'el-patch-stub)
;;;###autoload(el-patch--deftype-stub-setup)

;; These are alphabetized.

;;;###autoload
(el-patch-deftype cl-defun
  :classify el-patch-classify-function
  :locate el-patch-locate-function
  :font-lock el-patch-fontify-as-defun
  :declare ((doc-string 3)
            (indent defun)))

;;;###autoload
(el-patch-deftype defconst
  :classify el-patch-classify-variable
  :locate el-patch-locate-variable
  :font-lock el-patch-fontify-as-variable
  :declare ((doc-string 3)
            (indent defun)))

;;;###autoload
(el-patch-deftype defcustom
  :classify el-patch-classify-variable
  :locate el-patch-locate-variable
  :font-lock el-patch-fontify-as-variable
  :declare ((doc-string 3)
            (indent defun)))

;;;###autoload
(el-patch-deftype define-minor-mode
  :classify el-patch-classify-define-minor-mode
  :locate el-patch-locate-function
  :font-lock el-patch-fontify-as-defun
  :declare ((doc-string 2)
            (indent defun)))

;;;###autoload
(el-patch-deftype defmacro
  :classify el-patch-classify-function
  :locate el-patch-locate-function
  :font-lock el-patch-fontify-as-defun
  :declare ((doc-string 3)
            (indent defun)))

;;;###autoload
(el-patch-deftype defsubst
  :classify el-patch-classify-function
  :locate el-patch-locate-function
  :font-lock el-patch-fontify-as-defun
  :declare ((doc-string 3)
            (indent defun)))

;;;###autoload
(el-patch-deftype defun
  :classify el-patch-classify-function
  :locate el-patch-locate-function
  :font-lock el-patch-fontify-as-defun
  :declare ((doc-string 3)
            (indent defun)))

;;;###autoload
(el-patch-deftype defvar
  :classify el-patch-classify-variable
  :locate el-patch-locate-variable
  :font-lock el-patch-fontify-as-variable
  :declare ((doc-string 3)
            (indent defun)))

;;;; Validating patches

(defcustom el-patch-pre-validate-hook nil
  "Hook run before `el-patch-validate-all'.
Also run before `el-patch-validate' if a prefix argument is
provided. This hook should contain functions that make sure all
of your patches are defined (for example, you might need to load
some features if your patches are lazily defined)."
  :type 'hook)

(defcustom el-patch-post-validate-hook nil
  "Hook run after `el-patch-validate-all'.
Also run after `el-patch-validate' if a prefix argument is
provided. This hook should contain functions that undo any
patching that might have taken place in
`el-patch-pre-validate-hook', if you do not want the patches to
be defined permanently."
  :type 'hook)

(defun el-patch--locate (definition)
  "Return the Lisp form corresponding to the given DEFINITION.
Return nil if such a definition cannot be found. (That would
happen if the definition were generated dynamically.) TYPE is a
symbol `defun', `defmacro', etc. which is used to determine
whether the symbol is a function or variable."
  (let* ((type (nth 0 definition))
         (props (alist-get type el-patch-deftype-alist))
         (locator (plist-get props :locate)))
    (unless locator
      (error
       "Definition type `%S' has no `:locate' in `el-patch-deftype-alist'"
       type))
    (funcall locator definition)))

;;;###autoload
(defun el-patch-validate (name type &optional nomsg run-hooks variant)
  "Validate the patch with given NAME and TYPE.
This means el-patch will attempt to find the original definition
for the function, and verify that it is the same as the original
function assumed by the patch. A warning will be signaled if the
original definition for a patched function cannot be found, or if
there is a difference between the actual and expected original
definitions.

If multiple variants exist for the same patch, then select the
one specified by VARIANT (defaults to nil, like
`el-patch-variant'). For advanced usage only.

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
                 (cl-destructuring-bind (name type variant)
                     (el-patch--select-patch)
                   (list
                    name type nil
                    (unless current-prefix-arg
                      'post-only)
                    variant))))
  (unless (member run-hooks '(nil post-only))
    (run-hooks 'el-patch-pre-validate-hook))
  (unwind-protect
      (progn
        (let* ((patch-definition (el-patch-get name type variant))
               (type (car patch-definition))
               (expected-definition (el-patch--resolve-definition
                                     patch-definition nil))
               (name (cadr expected-definition))
               (actual-definition (el-patch--locate expected-definition)))
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
              (let ((type-hash (gethash type patch-hash)))
                (dolist (variant (hash-table-keys type-hash))
                  (setq patch-count (1+ patch-count))
                  (unless (el-patch-validate name type 'nomsg nil
                                             variant)
                    (setq warning-count (1+ warning-count))))))))
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

;;;;; el-patch-feature

;;;###autoload
(defmacro el-patch-feature (feature &rest args)
  "Declare that some patches are only defined after FEATURE is loaded.
This is a convenience macro that creates a function for invoking
`require' on that feature, and then adds it to
`el-patch-pre-validate-hook' so that your patches are loaded and
`el-patch' can properly validate them.

FEATURE should be an unquoted symbol. ARGS, if given, are passed
unchanged along with FEATURE to `el-patch-require-function' when
`el-patch-validate-all' is called."
  (let ((defun-name (intern (format "el-patch-require-%S" feature))))
    `(progn
       (defun ,defun-name ()
         (funcall el-patch-require-function ',feature ,@args))
       (add-hook 'el-patch-pre-validate-hook #',defun-name))))

;;;; Viewing patches

;;;###autoload
(defun el-patch-get (name type &optional variant)
  "Return the patch for object NAME of the given TYPE.
NAME is a symbol for the name of the definition that was patched,
and TYPE is a symbol `defun', `defmacro', etc. If the patch could
not be found, return nil.

If VARIANT is provided, select that variant of the patch. This is
useful only if patches were defined using `el-patch-variant'."
  (condition-case nil
      (gethash
       (if (consp variant)
           variant
         (cons nil variant))
       (gethash type (gethash name el-patch--patches)))
    (error nil)))

(defun el-patch--select-patch ()
  "Use `completing-read' to select a patched function.
Return a list of three elements, the name (a symbol) of the
object being patched, the type (a symbol `defun', `defmacro',
etc.) of the definition, and the patch variant (a symbol, usually
nil; see `el-patch-variant')."
  (let* ((options (mapcar #'symbol-name (hash-table-keys el-patch--patches)))
         (name (intern
                (pcase (length options)
                  (0 (user-error "No patches defined"))
                  (_ (completing-read
                      "Which patch? "
                      options
                      nil
                      'require-match)))))
         (patch-hash (gethash name el-patch--patches))
         (options (mapcar #'symbol-name
                          (hash-table-keys patch-hash)))
         (type (intern
                (pcase (length options)
                  (0 (error "Internal `el-patch' error"))
                  (1 (car options))
                  (_ (completing-read
                      "Which type? "
                      options
                      nil
                      'require-match)))))
         (type-hash (gethash type patch-hash))
         (options (hash-table-keys type-hash))
         (variant (pcase (length options)
                    (0 (error "Internal `el-patch' error"))
                    (1 (car options))
                    (_ (let ((completing-options
                              (mapcar (lambda (x)
                                        (cons (format "%s%S"
                                                      (or (and (car x)
                                                               "Advice: ")
                                                          "")
                                                      (cdr x))
                                              x))
                                      (hash-table-keys type-hash))))
                         (alist-get
                          (completing-read
                           "Which variant? "
                           completing-options
                           nil
                           'require-match)
                          completing-options
                          nil nil 'equal))))))
    (list name type variant)))

(defun el-patch--ediff-forms (name1 form1 name2 form2)
  "Ediff two forms.
Obtain and empty buffer named NAME1 and pretty-print FORM1 into
it. Do the same for NAME2 and FORM2, and then run Ediff on the
two buffers wordwise."
  (let (min1 max1 min2 max2)
    (with-current-buffer (get-buffer-create name1)
      (erase-buffer)
      (emacs-lisp-mode)
      (pp form1 (current-buffer))
      (setq min1 (point-min)
            max1 (point-max)))
    (with-current-buffer (get-buffer-create name2)
      (erase-buffer)
      (emacs-lisp-mode)
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
(defun el-patch-ediff-patch (name type &optional variant)
  "Show the patch for an object in Ediff.
NAME, TYPE, and VARIANT are as returned by `el-patch-get'."
  (interactive (el-patch--select-patch))
  (if-let ((patch-definition (el-patch-get name type variant)))
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
(defun el-patch-ediff-conflict (name type &optional variant)
  "Show a patch conflict in Ediff.
This is a diff between the expected and actual values of a
patch's original definition. NAME, TYPE, and VARIANT are as
returned by `el-patch-get'."
  (interactive (el-patch--select-patch))
  (if-let ((patch-definition (el-patch-get name type variant)))
      (let* ((expected-definition (el-patch--resolve-definition
                                   patch-definition nil))
             (actual-definition (el-patch--locate expected-definition)))
        (el-patch--ediff-forms
         "*el-patch actual*" actual-definition
         "*el-patch expected*" expected-definition)
        (when (equal actual-definition expected-definition)
          (message "No conflict")))
    (error "There is no patch for %S %S" type name)))

;;;; use-package integration

(defun el-patch--use-package-handler
    (base-keyword name _keyword args rest state)
  "When applied partially, return a `use-package' handler.
BASE-KEYWORD is either `:init' or `:config'. The remaining
arguments NAME, KEYWORD, ARGS, REST, and STATE are explained by
the `use-package' documentation."
  (setq rest
        (plist-put
         rest base-keyword
         (append
          (mapcar
           (lambda (arg)
             (if (and (consp arg)
                      (assq (car arg) el-patch-deftype-alist))
                 (cons (or
                        (plist-get
                         (alist-get (car arg) el-patch-deftype-alist)
                         :macro-name)
                        (intern (format "el-patch-%S" (car arg))))
                       (cdr arg))
               arg))
           args)
          (plist-get rest base-keyword))))
  (setq rest
        (plist-put
         rest :init
         (cons `(el-patch-feature ,name)
               (plist-get rest :init))))
  (use-package-process-keywords name rest state))

(define-minor-mode el-patch-use-package-mode
  "Minor mode to enable `use-package' integration for `el-patch'.
This mode is enabled or disabled automatically when the
`el-patch' library is loaded, according to the value of
`el-patch-enable-use-package-integration'."
  :global t
  :group 'el-patch
  (if el-patch-use-package-mode
      (with-eval-after-load 'use-package-core
        (dolist (kw '(:init/el-patch :config/el-patch))
          (cl-pushnew kw use-package-keywords))
        (dolist (fun '(use-package-normalize/:init/el-patch
                       use-package-normalize/:config/el-patch))
          (defalias fun #'use-package-normalize-forms))
        (defalias 'use-package-handler/:init/el-patch
          (apply-partially #'el-patch--use-package-handler :init))
        (defalias 'use-package-handler/:config/el-patch
          (apply-partially #'el-patch--use-package-handler :config)))
    (with-eval-after-load 'use-package-core
      (dolist (kw '(:init/el-patch :config/el-patch))
        (setq use-package-keywords (delq kw use-package-keywords)))
      (dolist (fun '(use-package-normalize/:init/el-patch
                     use-package-normalize/:config/el-patch
                     use-package-handler/:init/el-patch
                     use-package-handler/:config/el-patch))
        (fmakunbound fun)))))

(if el-patch-enable-use-package-integration
    (el-patch-use-package-mode +1)
  (el-patch-use-package-mode -1))

;;;; Closing remarks

(provide 'el-patch)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; el-patch.el ends here

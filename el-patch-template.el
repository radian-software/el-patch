;;; el-patch-template.el --- Even easier patching -*- lexical-binding: t -*-

;; Copyright (C) 2021 Al Haji-Ali

;; Author: Al Haji-Ali <abdo.haji.ali@gmail.com>
;; Created: 1 March 2021
;; Homepage: https://github.com/radian-software/el-patch
;; Keywords: extensions
;; Package-Requires: ((emacs "26"))
;; SPDX-License-Identifier: MIT
;; Version: 3.0

;;; Commentary:

;; `el-patch-template' is an extension of `el-patch' that allows one
;; to specifiy a patch without providing the complete source code of
;; the patched form.
;;
;; Example usage:

;; (el-patch-define-template
;;   (defun (el-patch-swap restart-emacs radian-new-emacs))
;;   (el-patch-concat
;;     (el-patch-swap
;;       "Restart Emacs."
;;       "Start a new Emacs session without killing the current one.")
;;     ...
;;     (el-patch-swap "restarted" "started")
;;     ...
;;     (el-patch-swap "restarted" "started")
;;     ...
;;     (el-patch-swap "restarted" "started")
;;     ...)
;;   (el-patch-swap
;;     (save-buffers-kill-emacs)
;;     (restart-emacs--launch-other-emacs restart-args)))
;;
;; Please see https://github.com/radian-software/el-patch for more
;; information.

;;; Code:

(require 'cl-lib)
(require 'el-patch)

;;;;;; Internal functions and variables:
;;;###autoload
(defvar el-patch--templates (make-hash-table :test 'equal)
  "Hash table of templates that have been defined.
The keys are symbols naming the objects that have been patched.
The values are hash tables mapping definition types (symbols
`defun', `defmacro', etc.) to patch definitions, which are lists
beginning with `defun', `defmacro', etc.")


(defun el-patch--process-el-patch-template
    (form template &optional match next-step-fn table)
  "Match FORM to an `el-patch-*' directive and return the resolution.
Assume that TEMPLATE is a list whose first element is an el-patch
directive and throw `not-el-patch' otherwise. Upon successful
matching, process the forms, append them to MATCH and call
NEXT-STEP-FN with the result and the remaining unmatched forms.
TABLE is a hashtable containing the bindings of `el-patch-let'"
  (let ((directive (car template)))
    (pcase directive
      ('el-patch-swap
        (let ((swap-next-step
               (lambda (new-match remainder-form)
                 (when (cdr new-match)
                   ;; el-patch-swap swaps a single form
                   ;; with another
                   (throw 'no-match nil))
                 (funcall next-step-fn
                          (append match
                                  (list
                                   (list directive
                                         ;; First argument is
                                         ;; replaced by match
                                         (car new-match)
                                         ;; Second argument as is
                                         ;; from template
                                         (cl-caddr template))))
                          remainder-form))))
          (el-patch--process-template form
                                      ;; We match the first argument
                                      ;; only
                                      (list (cadr template))
                                      nil swap-next-step
                                      table nil)))
      ((or 'el-patch-wrap 'el-patch-splice)
       (let* ((triml (if (>= (length template) 3)
                         (nth 1 template)
                       0))
              (trimr (if (>= (length template) 4)
                         (nth 2 template)
                       0))
              (is-splice (equal directive 'el-patch-splice))
              (body (car (last template)))
              (wrap-next-step
               (lambda (new-match remainder-form)
                 (funcall next-step-fn
                          (append match
                                  ;; The directive with arguments
                                  (list (append
                                         (cl-subseq template 0
                                                    (1- (length template)))
                                         (if (equal directive
                                                    'el-patch-splice)
                                             new-match
                                           (list (append
                                                  (cl-subseq body 0 triml)
                                                  new-match
                                                  (last body trimr)))))))
                          remainder-form))))
         (el-patch--process-template form
                                     (if is-splice
                                         (list body)
                                       ;; Should not match the trimmings
                                       (nthcdr triml (butlast body trimr)))
                                     nil
                                     wrap-next-step
                                     table
                                     nil)))
      ((quote el-patch-let)
       (let* ((bindings (nth 1 template))
              (body (nthcdr 2 template))
              (let-next-step (lambda (new-match remainder-form)
                               ;; Build list of new bindings
                               ;; based on the their resolution
                               (let ((new-bindings
                                      (mapcar
                                       (lambda (kv)
                                         (let ((x (gethash (car kv)
                                                           table)))
                                           (list (car kv)
                                                 (or (cdr x) (car x)))))
                                       bindings)))
                                 (funcall next-step-fn
                                          (append match
                                                  (list
                                                   (append
                                                    (list
                                                     directive
                                                     new-bindings)
                                                    new-match)))
                                          remainder-form)))))
         (el-patch--with-puthash table
             (mapcar
              (lambda (kv)
                (unless (symbolp (car kv))
                  (user-error "Non-symbol (%s) as binding for `el-patch-let'"
                              (car kv)))
                (list (car kv)
                      (cons (cadr kv)
                            ;; The cdr is the resolution, nil for
                            ;; now, and will be filled in
                            ;; el-patch--process-template
                            nil)))
              bindings)
           (el-patch--process-template form body
                                       nil
                                       let-next-step
                                       table))))
      ('el-patch-concat
        (when (or (not (consp form))
                  (not (stringp (car form))))
          ;; el-patch-concat can only match a string
          (throw 'no-match nil))
        (let* ((resolved (car (el-patch--resolve (cdr template) nil)))
               (regex
                (apply 'concat (mapcar (lambda (x)
                                         (if (equal x '...)
                                             ;;"[\0-\377[:nonascii:]]*"
                                             ;; match any
                                             ;; character
                                             "\\(\\(?:.\\|\n\\)+\\)"
                                           (regexp-quote x)))
                                       resolved)))
               (match-no 1) split-form)
          (save-match-data
            (unless (string-match (concat "^" regex "$") (car form))
              (throw 'no-match nil))
            ;; Exchange form by the resolved template splicing in
            ;; the matched strings
            (setq split-form
                  (mapcar (lambda (x)
                            (if (equal x '...)
                                (prog1
                                    (match-string match-no
                                                  (car form))
                                  (setq match-no
                                        (1+ match-no)))
                              x))
                          resolved)))
          (el-patch--process-template split-form
                                      (cdr template)
                                      nil
                                      (lambda (new-match
                                               remainder-form)
                                        (when remainder-form
                                          ;; Must be a complete
                                          ;; match
                                          (throw 'no-match nil))
                                        (funcall next-step-fn
                                                 (append match
                                                         (list (cons
                                                                directive
                                                                new-match)))
                                                 (cdr form)))
                                      table)))
      ((or 'el-patch-literal 'el-patch-remove)
       (el-patch--process-template form (cdr template)
                                   nil
                                   (lambda (new-match remainder-form)
                                     (funcall next-step-fn
                                              (append match
                                                      (list (cons
                                                             directive
                                                             new-match)))
                                              remainder-form))
                                   table
                                   (equal directive 'el-patch-literal)))
      ('el-patch-add ;; Matches nothing
        (funcall next-step-fn
                 ;; simply add the template to the match
                 (append match (list template))
                 form))
      (_
       (throw 'not-el-patch nil)))))

(defun el-patch--process-template (form template &optional match
                                        next-step-fn
                                        table literal)
  "Match FORM to TEMPLATE and return the resolution.
TEMPLATE may contain `...' which greedily matches any number of
forms in FORM. TEMPLATE may also contain `el-patch-*' directives
which are resolved before matching. Upon successful matching,
process the forms, append them to MATCH and call NEXT-STEP-FN
with the result and the remaining unmatched forms. TABLE is a
hashtable containing the bindings of `el-patch-let'. When LITERAL
is non-nil, do not process el-patch-* directives.

If NEXT-STEP-FN is nil, return a cons whose car is concatenation
of MATCH and the processed forms from FROM, including
`el-patch-*' directives, which match TEMPLATE when the
`el-patch-*' directives are resolved, and the cdr are the
remaining unmatched forms."
  (let ((table (or table (make-hash-table :test 'equal))))
    (cond
     ((and (not literal)
           (consp template)
           (consp (car template))
           (member (caar template) '(el-patch-swap el-patch-wrap
                                                   el-patch-splice
                                                   el-patch-remove
                                                   el-patch-add
                                                   el-patch-concat
                                                   el-patch-let)))
      (el-patch--process-el-patch-template form (car template)
                                           match
                                           ;; The next step is to
                                           ;; match cdr template
                                           (lambda (new-match remainder-form)
                                             (el-patch--process-template
                                              remainder-form
                                              (cdr template)
                                              new-match
                                              next-step-fn
                                              table literal))
                                           table))
     ((and (consp template) (consp form))
      (if (member (car template) '(...))
          (let* ((dots-next-step
                  (when next-step-fn
                    ;; If next-step-fn was provided, then we need to
                    ;; cascade the steps. Otherwise, there's no need
                    ;; and we can simply return the cons
                    (lambda (new-match remainder-form)
                      (funcall next-step-fn
                               (append match
                                       (cons (car form)
                                             new-match))
                               remainder-form))))
                 (ret-val (or (catch 'no-match
                                (el-patch--process-template
                                 (cdr form)
                                 ;; Try not consuming `...'
                                 template nil
                                 dots-next-step table
                                 literal))
                              (el-patch--process-template
                               (cdr form)
                               ;; If we are here, we failed
                               ;; the previous match so try
                               ;; consuming `...'
                               (cdr template) nil
                               dots-next-step table
                               literal))))
            (if dots-next-step
                ret-val ;; Next step already processed
              (cons (append match
                            (cons (car form)
                                  (car ret-val)))
                    (cdr ret-val))))
        ;; NOTE: If we want to match zero or more (rather than one or
        ;; more) then we need to catch the exception from the previous
        ;; line and try matching after consuming `...' from TEMPLATE
        ;; but not consuming any from FORM
        (let* ((ret-val (el-patch--process-template (car form) (car template)
                                                    nil nil table
                                                    literal))
               (new-match (car ret-val))
               (remainder-form (cdr ret-val)))
          (el-patch--process-template
           (if remainder-form
               (cons remainder-form
                     (cdr form))
             (cdr form))
           (cdr template)
           (append match ;; start with previous match
                   (list new-match))
           next-step-fn
           table literal))))
     ((and (vectorp template) (vectorp form))
      (let* ((ret-val (el-patch--process-template
                       (append form nil) ;; convert to list
                       (append template nil)
                       nil
                       nil
                       table
                       literal))
             (new-match (car ret-val))
             (remainder-form (cdr ret-val)))
        (when remainder-form
          ;; Must be complete match
          (throw 'no-match nil))
        (funcall (or next-step-fn 'cons)
                 (if match
                     (append match
                             (list (apply
                                    'vector
                                    new-match)))
                   (apply 'vector new-match))
                 remainder-form)))
     ((null template) ;; nothing else to match
      (funcall (or next-step-fn 'cons) match form))
     ((or (member template '(...)) (equal template form))
      ;; A Complete match.
      (funcall (or next-step-fn 'cons) (append match form) nil))
     (t
      (or (when-let ((symbol (gethash template table)))
            (let* ((ret-val (el-patch--process-template
                             form
                             (or (cdr symbol)  ;; The previous resolution
                                 (car symbol)) ;; The template-value
                             nil
                             nil
                             table literal))
                   (new-match (car ret-val))
                   (remainder-form (cdr ret-val))
                   (old-entry (gethash template table)))
              ;; Save the symbol resolution
              (puthash template
                       (cons
                        (car symbol)
                        new-match)
                       table)
              ;; Then process the next step, adding the
              ;; template to the match
              (condition-case _
                  (funcall (or next-step-fn 'cons)
                           (if match
                               (cons match template)
                             template)
                           remainder-form)
                (no-match
                 ;; Ultimately, the matching did not
                 ;; work, so undo the symbol resolution
                 (puthash template old-entry table)
                 ;; and rethrow
                 (throw 'no-match nil)))))
          (throw 'no-match nil))))))

(defun el-patch--match-template-p (form
                                   template
                                   &optional full-match)
  "Check if the forms in FORM match TEMPLATE.
TEMPLATE may contain `...' which greedily matches any number of
forms in FORM. Match is successful if a partial list of FORM,
starting from the beginning, matches TEMPLATE. The return value
is the number of forms in FORM which match TEMPLATE or nil if a
match is not possible. If FULL-MATCH then perform a full match
and return t if all forms in FORM match TEMPLATE."
  (cond
   ((and (consp template) (consp form))
    (when-let ((matched-count
                (if (member (car template) '(...))
                    (or
                     (el-patch--match-template-p (cdr form)
                                                 template
                                                 full-match)
                     ;; If we are here, we failed so try consuming
                     ;; `...'
                     (el-patch--match-template-p (cdr form)
                                                 (cdr template)
                                                 full-match))
                  (and
                   (el-patch--match-template-p (car form)
                                               (car template) t) ;; Full match
                   (el-patch--match-template-p (cdr form)
                                               (cdr template) full-match)))))
      (if full-match
          matched-count
        (1+ matched-count))))
   ((and (vectorp template) (vectorp form))
    (el-patch--match-template-p (append form nil);; covert to list
                                (append template nil) t))
   ((and (consp template)
         (equal (car template) 'el-patch-template--concat)
         (stringp form))
    (string-match-p
     (apply 'concat (mapcar (lambda (x)
                              (if (and (equal x '...))
                                  ;; match any character
                                  ;;"[\0-\377[:nonascii:]]*"
                                  "\\(.\\|\n\\)+"
                                (regexp-quote x)))
                            (cdr template)))
     form))
   (t (or (and (null template)
               (if full-match
                   (null form)
                 0))
          (and (or (member template '(...))
                   (equal template form))
               (if (consp form)
                   (length form)
                 1))))))

(defun el-patch--any-template-p (definition ptemplates &optional up-to)
  "Return t if any form in DEFINITION matches a template in PTEMPLATES.
Otherwise return nil. See `el-patch--apply-template' for a
description of PTEMPLATES. The forms in DEFINITION are checked
against the `:old' resolutions in PTEMPLATES. The optional
argument UP-TO specifies the number of forms in DEFINITION to
check.

A match is successful if `el-patch--match-template-p' returns
non-nil."
  (and (or (null up-to) (> up-to 0))
       (or (cl-some
            (lambda (x) (el-patch--match-template-p definition
                                                    (plist-get x :old)))
            ptemplates)
           (and
            (consp definition)
            (or (el-patch--any-template-p (car definition)
                                          ptemplates)
                (el-patch--any-template-p (cdr definition)
                                          ptemplates
                                          (when up-to
                                            (1- up-to))))))))

(defun el-patch--apply-template (definition ptemplates)
  "Return DEFINITION after applying the templates in PTEMPLATES.

PTEMPLATE is a list of property lists which contain `:template'
where the actual template resides, `:old' is the template's old
resolution and `:matched' which is set to t if the template is
matched to a form in DEFINITION."
  (let (matched-forms-count matched-ptemplate)
    (cl-dolist (ptemplate ptemplates)
      (let ((matched (el-patch--match-template-p definition
                                                 (plist-get ptemplate :old))))
        (when matched
          (when matched-ptemplate
            (user-error "A form matches multiple templates"))
          (setq matched-forms-count matched
                matched-ptemplate ptemplate))))
    (cond
     ((null matched-ptemplate)
      (if (consp definition)
          (cons (el-patch--apply-template (car definition)
                                          ptemplates)
                (el-patch--apply-template (cdr definition)
                                          ptemplates))
        definition))
     ((plist-get matched-ptemplate :matched)
      (user-error "A template matches multiple forms"))
     ((and (consp definition)
           (or
            (el-patch--any-template-p (car definition)
                                      ptemplates)
            (and
             (cdr definition)
             (el-patch--any-template-p (cdr definition)
                                       ptemplates
                                       (1- matched-forms-count)))))
      (user-error "A form matching a template has subforms matching\
 other templates"))
     (t
      ;; The old resolution of the template uniquely matches the definition
      ;; Here we first mark the template as being matched then
      ;; do the actual resolution
      (plist-put matched-ptemplate :matched t)
      (let* ((temp-def (seq-take definition matched-forms-count))
             (remainder-def (seq-drop definition matched-forms-count))
             (resolution
              (el-patch--process-template temp-def
                                          (list
                                           (plist-get matched-ptemplate
                                                      :template)))))
        (when (cdr resolution)
          (error "Internal `el-patch' error"))
        (cons (caar resolution)
              (el-patch--apply-template remainder-def
                                        ptemplates)))))))

(defun el-patch--partial-old-resolve (forms)
  "Resolve `el-patch-*' directives in FORMS to old form.

Similar to `el-patch--resolve' with a special treatment for
`el-patch-concat'. Specifically, if the arguments of
`el-patch-concat' have `...' in them, it is not resolved but
changed to `el-patch-template--concat'."
  (let ((el-patch--concat-function
         (lambda (args)
           (if (cl-some (lambda (x) (equal x '...)) args)
               (cons 'el-patch-template--concat args)
             (apply #'concat args)))))
    (el-patch--resolve forms nil)))

;; Stolen from el-patch--select
(defun el-patch--select-template ()
  "Use `completing-read' to select a template.
Return a list of two elements, the name (a symbol) of the object
being patched and the type (a symbol `defun', `defmacro', etc.)
of the definition."
  (let ((options (mapcar #'symbol-name
                         (hash-table-keys el-patch--templates))))
    (unless options
      (user-error "No templates defined"))
    (let* ((name (intern (completing-read
                          "Which template? "
                          options
                          nil
                          'require-match)))
           (patch-hash (gethash name el-patch--templates))
           (options (mapcar #'symbol-name
                            (hash-table-keys patch-hash))))
      (list name
            (intern (pcase (length options)
                      (0 (error "Internal `el-patch' error"))
                      (1 (car options))
                      (_ (completing-read
                          "Which version? "
                          options
                          nil
                          'require-match))))))))

(defun el-patch--resolve-template (name type)
  "Resolve a template and returns the complete `el-patch-*' definition.

Template should have been defined using
`el-patch-define-template'. NAME is a symbol naming the object
being patched; TYPE is a symbol `defun', `defmacro', etc."
  (let* ((template-def (gethash type (gethash name
                                              el-patch--templates)))
         (unresolved-name (car template-def))
         (templates (cdr template-def))
         (old-name (car (el-patch--resolve unresolved-name nil))))
    (unless template-def
      (user-error "Resolving `%s' template failed -- \
 Cannot find template definition" name))
    (let* ((definition (or (el-patch--locate (list type old-name))
                           (user-error "Resolving `%s' template failed --\
 Cannot find definition" name)))
           (ptemplates (mapcar
                        (lambda (template)
                          (list :template template
                                :old (el-patch--partial-old-resolve template)
                                :matched nil))
                        templates))
           (patch (prog1
                      (condition-case err-handle
                          (el-patch--apply-template definition ptemplates)
                        (error
                         (user-error "Resolving `%s' template failed -- %s"
                                     name (cdr err-handle))))
                    (cl-dolist (ptemplate ptemplates)
                      (unless (plist-get ptemplate :matched)
                        (user-error
                         "Resolving `%s' template failed -- at least \
one template did not match any form in" name)))))
           (props (alist-get type el-patch-deftype-alist)))
      (cons (intern
             (or (plist-get props :macro-name)
                 ;; otherwise should be an el-patch-*
                 (format "el-patch-%S" (car patch))))
            (append (list unresolved-name)
                    (cddr patch))))))

(defun el-patch--define-template (type-name templates)
  "Define an el-patch template.
Return the new-resolved name of the object.

The meaning of TYPE-NAME and TEMPLATES is the same as in
`el-patch-define-template' (which see), but here they need to be
quoted since they are passed as regular function arguments."
  (unless (and (listp type-name) (eq (length type-name) 2))
    (user-error "TYPE-NAME is expected to be a list with two \
elements"))
  (let* ((type (car type-name))
         (unresolved-name (cadr type-name))
         (resolved-name (car (el-patch--resolve unresolved-name t))))
    (puthash type
             (cons unresolved-name templates)
             (or (gethash resolved-name el-patch--templates)
                 (puthash resolved-name
                          (make-hash-table :test #'equal)
                          el-patch--templates)))
    resolved-name))

;;;;;; User options, functions and macros
(defcustom el-patch-warn-on-eval-template t
  "When non-nil, print a warning when a template is evaluated in runtime.
The message is printed when
`el-patch-define-compiletime-template' is called in runtime
rather than in compile time."
  :type 'boolean
  :group 'el-patch)

;;;###autoload
(defun el-patch-insert-template (name type)
  "Resolve a template to an el-patch definition and insert it at point.

Template should have been defined using
`el-patch-define-template'. NAME is a symbol naming the object
being patched; TYPE is a symbol `defun', `defmacro', etc."
  (interactive (el-patch--select-template))
  (insert (format "%S"
                  (el-patch--resolve-template name type))))

;;;###autoload
(defun el-patch-eval-template (name type)
  "Resolve a template to an el-patch definition and evaluate it.

Template should have been defined using
`el-patch-define-template'. NAME is a symbol naming the object
being patched; TYPE is a symbol `defun', `defmacro', etc."
  (interactive (el-patch--select-template))
  (eval (el-patch--resolve-template name type)))

;;;###autoload
(defmacro el-patch-define-template (type-name &rest templates)
  "Define an el-patch template.
TYPE-NAME is a list whose first element is a type which can be
any type from `el-patch-deftype-alist', e.g., `defun',
`defmacro', etc, and the second element is the name of the elisp
object to be patched or an `el-patch-*' form that resolves to
that name. Return the new-resolved name of the object.

A template in TEMPLATES can contain `...', which greedily matches
one or more forms, and `el-patch-*' directives which are resolved
before being matched. A template must match exactly one form in
the definition of the elisp object, and should not match a
subform in another template. The checks along with the actual
matching are done when the functions `el-patch-eval-template' or
`el-patch-insert-template' are called."
  `(el-patch--define-template (quote ,type-name)
                              (quote ,templates)))

;;;###autoload
(defmacro el-patch-define-and-eval-template (type-name &rest templates)
  "Define and evaluate an el-patch template.

The meaning of TYPE-NAME and TEMPLATES are the same as
`el-patch-define-template'. If called in compile-time,
macro-expand the resolved template after defining the template.
If called in runtime, evaluate the resolved template instead and,
if `el-patch-warn-on-eval-template' is non-nil, print a warning."
  (if (bound-and-true-p byte-compile-current-file)
      (let* ((resolved-name
              (el-patch--define-template type-name templates))
             (resolved-template (el-patch--resolve-template
                                 resolved-name (car type-name))))
        `(progn
           ;; In case el-patch-template is not loaded then simply
           ;; set `el-patch--templates' to an empty hash
           (setq el-patch--templates
                 (or (bound-and-true-p el-patch--templates)
                     (make-hash-table :test 'equal)))
           ;; Add template definition to `el-patch--templates'
           (puthash (quote ,(car type-name))
                    (cons (quote ,(cadr type-name))
                          (quote ,templates))
                    (or (gethash (quote ,resolved-name) el-patch--templates)
                        (puthash (quote ,resolved-name)
                                 (make-hash-table :test #'equal)
                                 el-patch--templates)))
           ,resolved-template))
    `(let* ((qtype-name (quote ,type-name))
            (resolved-name (el-patch--define-template
                            qtype-name (quote ,templates))))
       (when el-patch-warn-on-eval-template
         (display-warning 'el-patch "Runtime evaluation of el-patch templates \
can be slow, consider byte-compiling."))
       (condition-case-unless-debug err
           (el-patch-eval-template resolved-name
                                   (car qtype-name))
         (error (display-warning 'el-patch (error-message-string err)))))))


;; Stolen from `el-patch-validate'
;;;###autoload
(defun el-patch-validate-template (name type &optional nomsg run-hooks)
  "Validate the template with given NAME and TYPE.
This means el-patch will verify that the template is applicable
to the original function assumed by the patch. A warning will be
signaled if the original definition for a patched function cannot
be found, or if the template is not applicable.

Interactively, use `completing-read' to select a function to
inspect the template of.

NAME is a symbol naming the object being patched; TYPE is a
symbol `defun', `defmacro', etc.

Returns nil if the template is not valid, and otherwise returns t.
If NOMSG is non-nil, does not signal a message when the patch is
valid.

If RUN-HOOKS is non-nil, runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'. Interactively, this happens unless
a prefix argument is provided.

See also `el-patch-validate-all'."
  (interactive (progn
                 (unless current-prefix-arg
                   (run-hooks 'el-patch-pre-validate-hook))
                 (append (el-patch--select-template)
                         (list nil (unless current-prefix-arg
                                     'post-only)))))
  (message "Checking %S" name)
  (unless (member run-hooks '(nil post-only))
    (run-hooks 'el-patch-pre-validate-hook))
  (prog1 (condition-case err-handle
             (progn
               (el-patch--resolve-template name type)
               (unless nomsg
                 (message "Template is valid"))
               t)
           (error
            (progn
              (display-warning 'el-patch
                               (format "`%s' failed -- %s" name
                                       (error-message-string err-handle)))
              nil)))
    (when run-hooks
      (run-hooks 'el-patch-post-validate-hook))))

;; Stolen from `el-patch-validate-all'
;;;###autoload
(defun el-patch-validate-all-templates ()
  "Validate all currently defined patches.
Runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'.

See `el-patch-validate-template'."
  (interactive)
  (run-hooks 'el-patch-pre-validate-hook)
  (unwind-protect
      (let ((template-count 0)
            (warning-count 0))
        (dolist (name (hash-table-keys el-patch--templates))
          (let ((template-hash (gethash name el-patch--templates)))
            (dolist (type (hash-table-keys template-hash))
              (setq template-count (1+ template-count))
              (unless (el-patch-validate-template name type 'nomsg)
                (setq warning-count (1+ warning-count))))))
        (cond
         ((zerop template-count)
          (user-error "No templates defined"))
         ((zerop warning-count)
          (if (= template-count 1)
              (message "Template is valid (only one defined)")
            (message "All %d templates are valid" template-count)))
         ((= template-count warning-count)
          (if (= template-count 1)
              (message "Template is invalid (only one defined)")
            (message "All %d templates are invalid" template-count)))
         (t
          (message "%s valid, %s invalid"
                   (if (= warning-count (1- template-count))
                       "1 template is"
                     (format "%d templates are"
                             (- template-count warning-count)))
                   (if (= warning-count 1)
                       "1 template is"
                     (format "%d templates are" warning-count))))))
    (run-hooks 'el-patch-post-validate-hook)))


(provide 'el-patch-template)

;; Local Variables:
;; indent-tabs-mode: nil
;; checkdoc-verb-check-experimental-flag: nil
;; End:

;;; el-patch-template.el ends here

;;; el-patch-template.el --- -*- lexical-binding: t -*-


;; Author: Al Haji-Ali <abdo.haji.ali@gmail.com>
;; Created: 1 March 2021
;; Homepage: https://github.com/raxod502/el-patch
;; Keywords: extensions
;; Package-Requires: ((emacs "25"))
;; SPDX-License-Identifier: MIT
;; Version: 2.3.1

;;; Commentary:
;; `el-patch-template' is an extension of `el-patch' that allows one
;; to specifiy a patch without providing the complete source code of
;; the patched form.
;;
;; Example usage:
;;
;; (el-patch-template (defun TeX-update-style)
;;   (el-patch-concat
;;     "Run style specific hooks"
;;     (el-patch-add
;;       ", silently,")
;;     " for the current document.
;;
;; Only do this if it has not been done before, or if optional argument
;; FORCE is not nil.")
;;   (el-patch-remove
;;     (... "Applying style hooks..."))
;;   (el-patch-remove
;;     (... "Applying style hooks...done")))

(defun el-patch-template--process-el-patch
    (form template &optional match next-step-fn table)
  "Process an el-patch statement. Arguments are the same as
`el-patch-template--match'. Assumes that TEMPLATE is a cons whose
car is an el-patch directive and throws `not-el-patch' otherwise.
Upon succesful matching calls `next-step-fn' with MATCH after
appending it with the matching forms from FORM."
  (when (consp template)
    (let* ((directive (car template)))
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
                                           (caddr template))))
                            remainder-form))))
            (el-patch-template--process form
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
           (el-patch-template--process form
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
                    (error "Non-symbol (%s) as binding for `el-patch-let'"
                           (car kv)))
                  (list (car kv)
                        (cons (cadr kv)
                              ;; The cdr is the resolution, nil for
                              ;; now, and will be filled in
                              ;; el-patch-template--match
                              nil)))
                bindings)
             (el-patch-template--process form body
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
                                               "\\(\\(?:.\\|\n\\)*\\)"
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
            (el-patch-template--process split-form
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
         (el-patch-template--process form (cdr template)
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
         (throw 'not-el-patch nil))))))

(defun el-patch-template--process (form template &optional match
                                        next-step-fn
                                        table literal)
  "Matches TEMPLATE to FORM. TEMPLATE may contain `...' which
greedily match any number of forms. It may also contain
`el-patch-*' directives which are resolved before matching. Match
is succesfful if FORM matches TEMPLATE. Return value is a cons
where the car is the forms from FROM which match TEMPLATE the cdr
are the are the remaining unmatched forms. If TEMPLATE is nil,
calls NEXT-STEP-FN with MATCH and FORM. When LITERAL is non-nil,
do not process el-patch-* directives. TABLE is a hash-table which
contains bindings used by `el-patch-let'"
  (let ((next-step-fn (or next-step-fn
                          (lambda (match remainder-form)
                            (cons match remainder-form))))
        (table (or table (make-hash-table :test 'equal))))
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
      (el-patch-template--process-el-patch form (car template)
                                           match
                                           ;; The next step is to
                                           ;; match cdr template
                                           (lambda (new-match remainder-form)
                                             (el-patch-template--process
                                              remainder-form
                                              (cdr template)
                                              new-match
                                              next-step-fn
                                              table literal))
                                           table))
     ((and (consp template) (consp form))
      (if (member (car template) '(...))
          (progn
            (let ((dots-next-step
                   (lambda (new-match remainder-form)
                     (funcall next-step-fn
                              (append match
                                      (cons (car form)
                                            new-match))
                              remainder-form))))
              (or
               (catch 'no-match
                 (el-patch-template--process (cdr form)
                                             ;; Try not consuming `...'
                                             template nil
                                             dots-next-step table
                                             literal))
               (el-patch-template--process (cdr form)
                                           ;; If we are here, we failed
                                           ;; the previous match so try
                                           ;; consuming `...'
                                           (cdr template) nil
                                           dots-next-step table
                                           literal))))
        ;; NOTE: If we want to match zero or more (rather than one or
        ;; more) then we need to catch the exception from the previous
        ;; line and try matching after consuming `...' from TEMPLATE
        ;; but not consuming any from FORM
        (let ((consp-next-step (lambda (new-match remainder-form)
                                 ;; If we are in a string and the
                                 ;; remainder is a string then we can
                                 ;; still match it
                                 (el-patch-template--process
                                  (if remainder-form
                                      (cons remainder-form
                                            (cdr form))
                                    (cdr form))
                                  (cdr template)
                                  (append match ;; start with previous match
                                          (list new-match))
                                  next-step-fn
                                  table literal))))
          (el-patch-template--process (car form) (car template)
                                      nil consp-next-step table
                                      literal))))
     ((and (vectorp template) (vectorp form))
      (el-patch-template--process (append form nil) ;; convert to list
                                  (append template nil)
                                  nil
                                  (lambda (new-match remainder-form)
                                    (when remainder-form
                                      ;; Must be complete match
                                      (throw 'no-match nil))
                                    (funcall next-step-fn
                                             (if match
                                                 (append match
                                                         (list (apply
                                                                'vector
                                                                new-match)))
                                               (apply 'vector new-match))
                                             remainder-form))
                                  table
                                  literal))
     ((null template) ;; nothing else to match
      (funcall next-step-fn match form))
     ((or (member template '(...)) (equal template form))
      ;; A Complete match.
      (funcall next-step-fn (append match form) nil))
     (t
      (or (when-let ((symbol (gethash template table))
                     (symbol-next-step
                      (lambda (new-match remainder-form)
                        ;; First put the match in the table.
                        (let ((old-entry (gethash template table)))
                          (puthash template
                                   (cons
                                    (car symbol)
                                    new-match)
                                   table)
                          ;; Then process the next step, adding the
                          ;; template to the match
                          (condition-case _
                              (funcall next-step-fn
                                       (if match
                                           (cons match template)
                                         template)
                                       remainder-form)
                            ('no-match
                             ;; Ultimately, the matching did not
                             ;; work, so undo the symbol resolution
                             (puthash template old-entry table)
                             ;; and rethrow
                             (throw 'no-match nil)))))))
            (el-patch-template--process form
                                        (or
                                         ;; The previous resolution
                                         (cdr symbol)
                                         ;; The template-value
                                         (car symbol))
                                        nil
                                        symbol-next-step
                                        table literal))
          (throw 'no-match nil))))))

(defun el-patch-template--match-p (form template)
  "Matches TEMPLATE to FORM. TEMPLATE may contain `...' which
greedily match any number of forms. Match is succesful if a
partial list of FORM, starting from the beginning matches
TEMPLATE. The return value is the number of forms in FORM which
match template or nil if a match is not possible."
  (cond
   ((and (consp template) (consp form))
    (when-let ((matched-count
                (if (member (car template) '(...))
                    (or
                     (el-patch-template--match-p (cdr form)
                                                 template)
                     ;; If we are here, we failed so try consuming
                     ;; `...'
                     (el-patch-template--match-p (cdr form)
                                                 (cdr template)))
                  (and
                   (el-patch-template--match-p (car form)
                                               (car template))
                   (el-patch-template--match-p (cdr form)
                                               (cdr template))))))
      (1+ matched-count)))
   ((and (vectorp template) (vectorp form))
    (el-patch-template--match-p (append form nil);; covert to list
                                (append template nil)))
   ((and (consp template)
         (equal (car template) 'el-patch-template--concat)
         (stringp form))
    (string-match-p
     (apply 'concat (mapcar (lambda (x)
                              (if (and (equal x '...))
                                  ;; match any character
                                  ;;"[\0-\377[:nonascii:]]*"
                                  "\\(.\\|\n\\)*"
                                (regexp-quote x)))
                            (cdr template)))
     form))
   (t (or (and (null template) 0)
          (and (or (member template '(...))
                   (equal template form))
               (if (consp form)
                   (length form)
                 1))))))

(defun el-patch-template--any-p (definition ptemplates &optional up-to)
  "Returns t if any templates in PTEMPLATE match any form in
definition, otherwise returns nil."
  (and (or (null up-to) (> up-to 0))
       (or (cl-some
            (lambda (x) (el-patch-template--match-p definition
                                                    (plist-get x :old)))
            ptemplates)
           (and
            (consp definition)
            (or (el-patch-template--any-p (car definition)
                                          ptemplates)
                (el-patch-template--any-p (cdr definition)
                                          ptemplates
                                          (when up-to
                                            (1- up-to))))))))

(defun el-patch-template--apply (definition ptemplates)
  "Apply the templates in PTEMPLATES to definition.
PTEMPLATE is a list of plist templates which contain `:template'
where the actual template resides, `:old' is the template's old
resolution and `:matched' which is set to t if the template is
matched in DEFINITION."
  (let (matched-forms-count matched-ptemplate)
    (cl-dolist (ptemplate ptemplates)
      (let ((matched (el-patch-template--match-p definition
                                                 (plist-get ptemplate :old))))
        (when matched
          (when matched-ptemplate
            (error "A form matches multiple templates"))
          (setq matched-forms-count matched
                matched-ptemplate ptemplate))))
    (cond
     ((null matched-ptemplate)
      (if (consp definition)
          (cons (el-patch-template--apply (car definition)
                                          ptemplates)
                (el-patch-template--apply (cdr definition)
                                          ptemplates))
        definition))
     ((plist-get matched-ptemplate :matched)
      (error "A template matches multiple forms"))
     ((and (consp definition)
           (or
            (el-patch-template--any-p (car definition)
                                      ptemplates)
            (and
             (cdr definition)
             (el-patch-template--any-p (cdr definition)
                                       ptemplates
                                       (1- matched-forms-count)))))
      (error "A form matching a template has subforms matching\
 other templates"))
     (t
      ;; The old resolution of the template uniquely matches the definition
      ;; Here we first mark the template as being matched then
      ;; do the actual resolution
      (plist-put matched-ptemplate :matched t)
      (let ((resolution
             (el-patch-template--process definition
                                         (list
                                          (plist-get matched-ptemplate
                                                     :template)))))
        (cons (caar resolution)
              (el-patch-template--apply (cdr resolution)
                                        ptemplates)))))))

(defun el-patch-template--resolve (forms)
  "Calls `el-patch--resolve' with a special treatment for
`el-patch-concat'. Specifically, if the arguments of
`el-patch-concat' have `...' in them, it is not resolved but
changed to `el-patch-template--concat'."
  (cl-letf* ((old-concat (symbol-function 'concat))
             ((symbol-function 'concat)
              (lambda (&rest args)
                (if (cl-some (lambda (x) (equal x '...)) args)
                    (cons 'el-patch-template--concat args)
                  (apply old-concat args)))))
    (el-patch--resolve forms nil)))

(defun el-patch-template--impl (keyword-name templates)
  "The actual implementation of `el-patch-template', accepts the
same arguments but quoted."
  (let* ((definition (or (el-patch--locate
                          (car (el-patch--resolve keyword-name nil)))
                         (error "Cannot find definition for `%s'"
                                (cadr keyword-name))))
         (ptemplates (mapcar
                      (lambda (template)
                        (list :template template
                              :old (el-patch-template--resolve template)
                              :matched nil))
                      templates))
         (patch (prog1 (el-patch-template--apply definition ptemplates)
                  (cl-dolist (ptemplate ptemplates)
                    (unless (plist-get ptemplate :matched)
                      (error
                       "At least one template did not match any form"))))))
    ;; NOTE: Unfortunately `el-patch-deftype-alist' doesn't save the
    ;; macro name so we have to assume that it is `el-patch-*'
    (cons (intern (format "el-patch-%S"
                          (car patch))) ;; should be an el-patch-*
          (append (cdr keyword-name)
                  (cddr patch)))))

(defmacro el-patch-template (keyword-name &rest templates)
  "KEYWORD-NAME is cons whose car is a type which can be any type
from `el-patch-deftype-alist' and the cdr is the name of the form
to be patched. Looks for all forms which match a template in
TEMPLATES and processes them by matching all `...' forms against
the source code of the form, while keeping the `el-patch-*'
directives. Returns an `el-patch-*' definition.

A template must match exactly one form in the definition. and
should not match a subform in another template."
  `(el-patch-template--impl (quote ,keyword-name)
                            (quote ,templates)))

(provide 'el-patch-template)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; el-patch-template.el ends here

;; -*- lexical-binding: t -*-

;; `el-patch-unit-tests' - unit tests using Buttercup.
;;
;; Setup originally stolen from Apheleia and modified.

(require 'el-patch)
(require 'el-patch-template)
(require 'buttercup)

(require 'cl-lib)

(describe "el-patch--resolve"
  (cl-macrolet ((testcases
                 (description &rest specs)
                 `(it ,description
                    ,@(mapcan
                       (lambda (spec)
                         (cl-destructuring-bind (input old new) spec
                           `((expect (el-patch--resolve ',input nil)
                                     :to-equal '(,old))
                             (expect (el-patch--resolve ',input t)
                                     :to-equal '(,new)))))
                       specs))))
    (testcases
     "does no-ops when no patch directives used"

     ((foo bar baz)
      (foo bar baz)
      (foo bar baz))

     ([foo bar baz]
      [foo bar baz]
      [foo bar baz])

     ((oh my how . improper)
      (oh my how . improper)
      (oh my how . improper))

     )
    (testcases
     "handles el-patch-add and el-patch-remove"

     ((foo (el-patch-add bar) baz)
      (foo baz)
      (foo bar baz))

     ([foo (el-patch-add bar) baz]
      [foo baz]
      [foo bar baz])

     ((foo (el-patch-add bar baz) quux)
      (foo quux)
      (foo bar baz quux))

     (((el-patch-add foo) bar baz)
      (bar baz)
      (foo bar baz))

     ((foo bar (el-patch-add baz))
      (foo bar)
      (foo bar baz))

     (((el-patch-add foo) bar (el-patch-add baz))
      (bar)
      (foo bar baz))

     (((el-patch-add foo))
      ()
      (foo))

     (((el-patch-add foo) bar (el-patch-remove baz))
      (bar baz)
      (foo bar))

     )
    (testcases
     "handles el-patch-concat"

     ((el-patch-concat "foo" "bar")
      "foobar"
      "foobar")

     ((el-patch-concat "foo" (el-patch-add "bar") "baz")
      "foobaz"
      "foobarbaz")

     ((foo (el-patch-concat "test" (el-patch-swap "1" "2")) bar)
      (foo "test1" bar)
      (foo "test2" bar))

     )))

(describe "el-patch--process-template"
  (cl-flet ((apply-templates
             (form templates)
             (el-patch--apply-template
              form
              (mapcar
               (lambda (template)
                 (list :template template
                       :old (el-patch--partial-old-resolve template)
                       :matched nil))
               templates))))
    (cl-macrolet ((testcases
                   (description &rest specs)
                   `(it ,description
                      ,@(mapcar
                         (lambda (spec)
                           (cl-destructuring-bind (form templates expected) spec
                             `(expect (apply-templates ',form ',templates)
                                      :to-equal ',expected)))
                         specs))))

      (testcases
       "provides basic functionality"

       ((foo (1 2 3 unwanted 4 5 6) quux)
        ((... 3 (el-patch-remove unwanted) 4 ...))
        (foo (1 2 3 (el-patch-remove unwanted) 4 5 6) quux))

       )
      (testcases
       "works with examples from the magit-file-icons package"

       ((defun magit-diff-insert-file-section
            (file orig status modes rename header binary long-status)
          (magit-insert-section
              ( file file
                (or (equal status "deleted") (derived-mode-p 'magit-status-mode))
                :source (and (not (equal orig file)) orig)
                :header header
                :binary binary)
            (insert (propertize (format "%-10s %s" status
                                        (if (or (not orig) (equal orig file))
                                            file
                                          (format "%s -> %s" orig file)))
                                'font-lock-face 'magit-diff-file-heading))
            (cond ((and binary long-status)
                   (insert (format " (%s, binary)" long-status)))
                  ((or binary long-status)
                   (insert (format " (%s)" (if binary "binary" long-status)))))
            (magit-insert-heading)
            (when modes
              (magit-insert-section (hunk '(chmod))
                (insert modes)
                (magit-insert-heading)))
            (when rename
              (magit-insert-section (hunk '(rename))
                (insert rename)
                (magit-insert-heading)))
            (magit-wash-sequence #'magit-diff-wash-hunk)))

        ((defun magit-diff-insert-file-section)
         (format (el-patch-swap "%-10s %s" "%-10s %s %s") status
                 (el-patch-add (nerd-icons-icon-for-file (or orig file)))
                 (if (or (not orig) (equal orig file))
                     file
                   (format (el-patch-swap "%s -> %s" "%s -> %s %s") orig
                           (el-patch-add (nerd-icons-icon-for-file file)) file))))

        (defun magit-diff-insert-file-section
            (file orig status modes rename header binary long-status)
          (magit-insert-section
              ( file file
                (or (equal status "deleted") (derived-mode-p 'magit-status-mode))
                :source (and (not (equal orig file)) orig)
                :header header
                :binary binary)
            (insert (propertize (format (el-patch-swap "%-10s %s" "%-10s %s %s") status
                                        (el-patch-add (nerd-icons-icon-for-file (or orig file)))
                                        (if (or (not orig) (equal orig file))
                                            file
                                          (format (el-patch-swap "%s -> %s" "%s -> %s %s") orig
                                                  (el-patch-add (nerd-icons-icon-for-file file)) file)))
                                'font-lock-face 'magit-diff-file-heading))
            (cond ((and binary long-status)
                   (insert (format " (%s, binary)" long-status)))
                  ((or binary long-status)
                   (insert (format " (%s)" (if binary "binary" long-status)))))
            (magit-insert-heading)
            (when modes
              (magit-insert-section (hunk '(chmod))
                (insert modes)
                (magit-insert-heading)))
            (when rename
              (magit-insert-section (hunk '(rename))
                (insert rename)
                (magit-insert-heading)))
            (magit-wash-sequence #'magit-diff-wash-hunk))))
       )
      (testcases
       "works with the example from the readme"

       ((defun restart-emacs (&optional args)
          "Restart Emacs.

When called interactively ARGS is interpreted as follows

- with a single `universal-argument' (`C-u') Emacs is restarted
  with `--debug-init' flag
- with two `universal-argument' (`C-u') Emacs is restarted with
  `-Q' flag
- with three `universal-argument' (`C-u') the user prompted for
  the arguments

When called non-interactively ARGS should be a list of arguments
with which Emacs should be restarted."
          (interactive "P")
          (restart-emacs--ensure-can-restart)
          (let* ((default-directory (restart-emacs--guess-startup-directory))
                 (translated-args (if (called-interactively-p 'any)
                                      (restart-emacs--translate-prefix-to-args args)
                                    args))
                 (restart-args (append translated-args
                                       (unless (member "-Q" translated-args)
                                         (restart-emacs--frame-restore-args))))
                 (kill-emacs-hook (append kill-emacs-hook
                                          (unless restart-emacs--inhibit-kill-p
                                            (list (apply-partially #'restart-emacs--launch-other-emacs
                                                                   restart-args))))))
            (if restart-emacs--inhibit-kill-p
                (restart-emacs--launch-other-emacs restart-args)
              (save-buffers-kill-emacs))))

        ((defun (el-patch-swap restart-emacs radian-new-emacs))
         (el-patch-concat
           (el-patch-swap
             "Restart Emacs."
             "Start a new Emacs session without killing the current one.")
           ...
           (el-patch-swap "restarted" "started")
           ...
           (el-patch-swap "restarted" "started")
           ...
           (el-patch-swap "restarted" "started")
           ...)
         (el-patch-remove (kill-emacs-hook ...))
         (el-patch-swap
           (save-buffers-kill-emacs)
           (restart-emacs--launch-other-emacs restart-args)))

        (defun restart-emacs (&optional args)
          (el-patch-concat
            (el-patch-swap
              "Restart Emacs."
              "Start a new Emacs session without killing the current one.")
            "
When called interactively ARGS is interpreted as follows

- with a single `universal-argument' (`C-u') Emacs is "
            (el-patch-swap "restarted" "started")
            "
  with `--debug-init' flag
- with two `universal-argument' (`C-u') Emacs is "
            (el-patch-swap "restarted" "started")
            " with
  `-Q' flag
- with three `universal-argument' (`C-u') the user prompted for
  the arguments

When called non-interactively ARGS should be a list of arguments
with which Emacs should be "
            (el-patch-swap "restarted" "started")
            ".")
          (interactive "P")
          (restart-emacs--ensure-can-restart)
          (let* ((default-directory (restart-emacs--guess-startup-directory))
                 (translated-args (if (called-interactively-p 'any)
                                      (restart-emacs--translate-prefix-to-args args)
                                    args))
                 (restart-args (append translated-args
                                       (unless (member "-Q" translated-args)
                                         (restart-emacs--frame-restore-args))))
                 (el-patch-remove
                   (kill-emacs-hook (append kill-emacs-hook
                                            (unless restart-emacs--inhibit-kill-p
                                              (list (apply-partially #'restart-emacs--launch-other-emacs
                                                                     restart-args)))))))
            (if restart-emacs--inhibit-kill-p
                (restart-emacs--launch-other-emacs restart-args)
              (el-patch-swap
                (save-buffers-kill-emacs)
                (restart-emacs--launch-other-emacs restart-args))))))

       )
      ;; (testcases
      ;;  "avoids issue #72"

      ;;  ((foo "here is a very long string example" bar)
      ;;   ))

      )))

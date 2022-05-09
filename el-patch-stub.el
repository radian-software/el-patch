;;; el-patch-stub.el --- Functions loaded separately -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Radian LLC and contributors

;; Author: Radian LLC <contact+el-patch@radian.codes>
;; Created: 21 Dec 2021
;; Homepage: https://github.com/radian-software/el-patch
;; Keywords: extensions
;; Package-Requires: ((emacs "26"))
;; SPDX-License-Identifier: MIT
;; Version: 2.3.1

;;; Commentary:

;; `el-patch-stub' has some utility functions that are used to define
;; stubs that are used only in the middle of the autoloading process.
;; Basically, they allow us to conveniently set up syntax highlighting
;; and indentation correctly for a number of different functions all
;; at once, without a bunch of code duplication.

;; Please see https://github.com/radian-software/el-patch for more
;; information.

;;; Code:

(defun el-patch--deftype-stub-setup ()
  "Define `el-patch-deftype' as a minimal version suitable for autoload time.
This temporary replacement for the real functionality just takes
care of the `declare' forms, and leaves everything else for
later."
  (unless (fboundp 'el-patch-deftype)
    (defmacro el-patch-deftype (type &rest kwargs)
      (let ((name (intern (format "el-patch-%S" type)))
            (props (plist-get kwargs :declare)))
        `(progn
           (autoload ',name "el-patch" nil nil t)
           (put ',name 'doc-string-elt ',(alist-get 'doc-string props))
           (put ',name 'lisp-indent-function ',(alist-get 'indent props)))))))

(provide 'el-patch-stub)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; el-patch-stub.el ends here

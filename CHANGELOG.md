# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## 3.1.1 (released 2025-08-15)
### Bugs fixed
* Using `el-patch-literal` could result in incorrect patch generation
  in some cases. This is fixed.

## 3.1 (released 2023-11-23)
### Features
* New user option `el-patch-use-advice` which can be customized to
  have `el-patch` install patches using the built-in advice system
  instead of overwriting function definitions directly.

### Enhancements
* Some minor improvements to the way errors are reported from
  `el-patch-template`.

## 3.0 (released 2022-04-17)
### Breaking changes
* The arguments to `el-patch-feature` after the feature name are no
  longer quoted by `el-patch-feature` before being passed to
  `el-patch-require-function`. This means you can write them as normal
  function arguments instead as unquoted symbols.
* The default value of `el-patch-require-function` has changed from
  `require` to `el-patch-default-require-function`, which ignores all
  arguments to `el-patch-feature` aside from the feature name, and
  converts `require` errors to warnings ([#47]).
* Emacs 25 is no longer supported.

### Features
* You can define patch variants using the new variable
  `el-patch-variant`. For advanced usage. See [#29] for discussion.
* You can validate patches automatically during byte-compilation by
  setting `el-patch-validate-during-compile` to non-nil ([#48]).

### Enhancements
* All `el-patch` forms are now fontified the same way as their
  built-in counterparts, e.g. the function name in an `el-patch-defun`
  is fontified the same as the function name in a `defun` would be.
  See [#35].

### Internal changes
* The autoloading mechanism used by `el-patch` has changed, reducing
  the amount of work that is done at startup and simplifying the
  implementation ([#56]). The user-facing impact is as follows:
    * `el-patch--patches` and `el-patch-deftype-alist` are no longer
      autoloaded. If you use a compiled init-file, you may need to
      recompile it with the new version of `el-patch`; the code
      compiled with the old version of `el-patch` will not work at
      runtime with the new version of `el-patch`. However, evaluating
      patches in a compiled init-file, even one that uses
      `el-patch-deftype`, still does not load `el-patch`.
    * `el-patch-defun` and analogous functions are now autoloaded,
      rather than fully defined at init time. This should not matter
      since a compiled init-file would have macroexpanded these into
      smaller components that do not have runtime dependencies on
      `el-patch`.
    * There is a new file `el-patch-stub.el` that needs to be on the
      `load-path` for autoloads to work. This should be taken care of
      automatically by any of the popular Emacs package managers.

[#29]: https://github.com/radian-software/el-patch/issues/29
[#35]: https://github.com/radian-software/el-patch/issues/35
[#47]: https://github.com/radian-software/el-patch/issues/47
[#48]: https://github.com/radian-software/el-patch/issues/48
[#56]: https://github.com/radian-software/el-patch/pull/56

## 2.4 (released 2021-12-27)
### New features
* New way to patch elisp objects based on a partial template and the
  original definition the object ([#50]). See the documentation on
  `el-patch-template` in the README.

### Bugs fixed
* Patch forms were not processed when they appeared inside a vector.
  This has been fixed ([#51]).

[#50]: https://github.com/radian-software/el-patch/issues/50
[#51]: https://github.com/radian-software/el-patch/issues/51

## 2.3.1 (released 2020-07-16)
### Bugs fixed
* If a function is not defined, then `el-patch-validate-all` will
  proceed to check other patches before reporting the error, rather
  than crashing immediately ([#46]).

[#46]: https://github.com/radian-software/el-patch/issues/46

## 2.3 (released 2020-04-02)
### Added
* New patch type `el-patch-cl-defun` ([#39]).

### Enhancements
* When using `el-patch-ediff-patch` and `el-patch-ediff-conflict` the
  compared buffers are put into `emacs-lisp-mode` automatically to
  enable syntax highlighting and Lisp navigation.

### Bugs fixed
* Under some circumstances, users received the error message

      Unregistered definition type ‘defalias’

  when evaluating an `el-patch` form. The problem could be worked
  around by explicitly `require`-ing `el-patch` before the form
  underwent macroexpansion. However, it should now be impossible to
  get that error regardless of whether `el-patch` was loaded
  previously ([#30]).

[#30]: https://github.com/radian-software/el-patch/issues/30
[#39]: https://github.com/radian-software/el-patch/issues/39

## 2.2.3 (released 2019-04-10)
### Enhancements
* The user option `el-patch-use-package-mode` is now correctly
  declared in the Custom group `el-patch`, rather than the separate
  group `el-patch-use-package`.

## 2.2.2 (released 2018-12-14)
### Enhancements
* When a definition is patched, `el-patch` automatically adds a note
  to the docstring so that you know a patch is in effect. Previously,
  this behavior was suppressed if the modified definition did not
  contain a docstring at all; now, the note is included regardless,
  adding a docstring if one was missing ([#31]).

[#31]: https://github.com/radian-software/el-patch/pull/31

## 2.2.1 (released 2018-09-04)

This release includes only a minor documentation enhancement.

## 2.2 (released 2018-08-14)
### New features
* The new patch directive `el-patch-concat` may be used to modify
  string literals in a patch without repeating their contents twice
  ([#14]).
* You may now define functions analogous to `el-patch-defun` for your
  own definition types using the new `el-patch-deftype` macro, which
  operates on the new user option `el-patch-deftype-alist` ([#24]).
* `el-patch` now provides integration with `use-package`, controlled
  by the variable `el-patch-enable-use-package-integration` and the
  minor mode `el-patch-use-package-mode`. Two new keywords,
  `:init/el-patch` and `:config/el-patch`, are defined ([#25]).

### Enhancements
* When a definition is patched, `el-patch` now automatically appends a
  note to the end of the docstring indicating that a patch was made
  ([#14]).

[#14]: https://github.com/radian-software/el-patch/issues/14
[#24]: https://github.com/radian-software/el-patch/issues/24
[#25]: https://github.com/radian-software/el-patch/issues/25

## 2.1 (released 2018-07-12)
### New features
* `el-patch-let` now allows more than one body form; all body forms
  are spliced into the containing s-expression. This is fully
  backwards compatible.
* `el-patch-literal` always allowed more than one body form, but this
  fact is now documented.

### Bugfixes
* Using `el-patch-let` could previously cause a circular list error.
  For example:

      (el-patch-let ((x y)) (x x x))

  That has now been fixed.

## 2.0.1 (released 2018-06-21)
### Internal changes
* `el-patch` now uses lexical binding.
* `el-patch` is now tested on all supported Emacs versions.
* `el-patch` no longer uses the deprecated [`elint`][elint] testing
  framework.

## 2.0 (released 2018-05-10)
### Changed
* `el-patch` can now be used in your init-file without being loaded at
  runtime. In other words, the patch definition macros expand, at
  compile time, to code that does not reference `el-patch` ([#11]).
* `el-patch` now uses [`elint`][elint] to run its tests and stylistic
  checks. Lines longer than 80 characters now fail the build.
* `el-patch` forms now have a well-defined return value, namely the
  same as would have been returned by the corresponding non-`el-patch`
  form ([#15]).

### Fixed
* `el-patch` previously failed with an error if you attempted to patch
  an object which used a literal cons cell or improper list in its
  definition. This has been fixed ([#13]).

## 1.2 (released 2017-07-23)
### Added
* `el-patch` now has a Makefile for convenient byte-compilation,
  linting, and table-of-contents updating.
* `el-patch` now checks for byte-compilation and checkdoc warnings on
  Travis CI ([#7]).
* New macro `el-patch-feature` (unrelated to the old obsolete
  `el-patch-feature` macro that was removed) which is a convenient way
  to add feature-requiring functions to `el-patch-pre-validate-hook`
  ([#10]).
* New user option `el-patch-require-function` for advanced users only.
  It allows you to integrate `el-patch-feature` with your package
  manager, and other things like that.

### Removed
* Deprecated patch directive `el-patch-feature` has been removed.

### Fixed
* Messages emitted by `el-patch-validate-all` now have correct
  pluralization ([#9]).
* The documentation on `el-patch-validate` was not updated to reflect
  its new signature in version 1.1.1. This has now been fixed.

## 1.1.2 (released 2017-06-05)
### Added
* Patches can now be defined for more types of objects:
  * `el-patch-defconst`
* Custom group for `el-patch`.
* New user option `el-patch-aggressive-defvar` to allow overriding the
  existing value of a variable.

### Changed
* Variables `el-patch-pre-validate-hook` and
  `el-patch-post-validate-hook` are now Custom variables.

### Fixed
* Since version 1.1, the "But how does it work?" section of the README
  contained an erroneous explanation implying that `set` and `fset`
  were used to establish patched definitions. This has been replaced
  with a correct explanation.

## 1.1.1 (released 2017-02-27)
### Changed
* The function `el-patch-validate` now takes a patch name and type
  rather than a patch definition. The remaining arguments `NOMSG` and
  `RUN-HOOKS` have not changed.
* The meaning of the prefix argument has been inverted for
  `el-patch-validate`. Now it runs `el-patch-pre-validate-hook` and
  `el-patch-post-validate-hook` by default, and only omits doing so if
  a prefix argument is provided.

### Fixed
* Since version 1.1, `el-patch-validate` failed with an error in
  `el-patch--classify-definition-type`. This has been fixed
  ([#8]).
* Since version 1.1, if a patch was defined in the same file as the
  function it patched, the patch could not be validated. This has been
  fixed.
* Previously, when `el-patch-validate` was called noninteractively
  with a non-nil value for `RUN-HOOKS`, it failed to run
  `el-patch-pre-validate-hook`. This has been fixed.

## 1.1 (released 2017-02-11)
### Added
* Patches can now be defined for more types of objects:
  * `el-patch-defvar`
  * `el-patch-defcustom`
  * `el-patch-define-minor-mode`
* New function `el-patch-get` to get a patch definition by name.
* Objects can now be renamed in a patch using `el-patch-swap` on the
  name ([#4]).
* Actual description in the Commentary section of `el-patch.el`
  ([#5]).
* MIT license ([melpa/melpa#4512]).
* Arbitrary functions can be run (to load patches) by adding things to
  the new hook `el-patch-pre-validate-hook`. Cleanup can then be done
  in `el-patch-post-validate-hook`.

### Changed
* It is no longer necessary to specify `el-patch-feature` due to
  improvements in the `el-patch` patching mechanism.
* The functions `el-patch-ediff-patch`, `el-patch-ediff-conflict`, and
  `el-patch-unpatch` now take an object name and definition type
  rather than a patch definition (or function name in the case of
  `el-patch-unpatch`). Interactive usage has not changed.

### Deprecated
* As a result of the aforementioned improvements, `el-patch-feature`
  is now a deprecated no-op.

### Fixed
* `el-patch-splice` previously acted the same as `el-patch-wrap`. Now
  it functions correctly.
* The patch definition macros now have their docstrings colored
  correctly by the syntax highlighter (by specifying `(declare
  (doc-string N))`).
* `.dir-locals.el` is now ignored when validating patches ([#2]).

## 1.0 (released 2017-01-21)
### Added
* Public API functions:
  * Functions for defining patches:
    * `el-patch-defun`
    * `el-patch-defmacro`
    * `el-patch-defsubst`
  * Patch directives:
    * `el-patch-add`
    * `el-patch-remove`
    * `el-patch-swap`
    * `el-patch-wrap`
    * `el-patch-splice`
    * `el-patch-let`
    * `el-patch-literal`
    * `el-patch-feature`
  * Functions for validating patches:
    * `el-patch-validate`
    * `el-patch-validate-all`
  * Functions for viewing patches:
    * `el-patch-ediff-patch`
    * `el-patch-ediff-conflict`
  * Functions for removing patches:
    * `el-patch-unpatch`

[#2]: https://github.com/radian-software/el-patch/issues/2
[#4]: https://github.com/radian-software/el-patch/issues/4
[#5]: https://github.com/radian-software/el-patch/issues/5
[#7]: https://github.com/radian-software/el-patch/issues/7
[#8]: https://github.com/radian-software/el-patch/issues/8
[#9]: https://github.com/radian-software/el-patch/issues/9
[#10]: https://github.com/radian-software/el-patch/issues/10
[#11]: https://github.com/radian-software/el-patch/pull/11
[#13]: https://github.com/radian-software/el-patch/issues/13
[#15]: https://github.com/radian-software/el-patch/issues/15

[melpa/melpa#4512]: https://github.com/melpa/melpa/pull/4512#issuecomment-274682089

[elint]: https://github.com/raxod502/elint
[keep a changelog]: https://keepachangelog.com/en/1.0.0/

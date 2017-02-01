# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

[Keep a Changelog]: http://keepachangelog.com/

## Unreleased
### Added
* Patches can now be defined for more types of objects:
  * `el-patch-defvar`
  * `el-patch-defgroup`
  * `el-patch-defcustom`
  * `el-patch-define-minor-mode`
* New function `el-patch-get` to get a patch definition by name.
* Objects can now be renamed in a patch using `el-patch-swap` on the
  name.
* MIT license.

### Changed
* It is no longer necessary to specify `el-patch-feature` due to
  improvements in the `el-patch` patching mechanism.
* The functions `el-patch-ediff-patch` `el-patch-ediff-conflict`, and
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

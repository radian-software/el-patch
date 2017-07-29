# el-patch

> Future-proof your Emacs Lisp customizations!

## Table of contents

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->


- [el-patch](#el-patch)
    - [Table of contents](#table-of-contents)
    - [TL;DR](#tldr)
        - [How do I get it](#how-do-i-get-it)
        - [What is it](#what-is-it)
    - [Installation](#installation)
    - [Why does it exist](#why-does-it-exist)
    - [Basic usage](#basic-usage)
    - [Patch directives](#patch-directives)
    - [Defining patches](#defining-patches)
    - [Inspecting patches](#inspecting-patches)
    - [Validating patches](#validating-patches)
    - [Removing patches](#removing-patches)
    - [Lazy-loading packages](#lazy-loading-packages)
    - [Validating patches that are not loaded yet](#validating-patches-that-are-not-loaded-yet)
    - [But how does it work?](#but-how-does-it-work)
    - [But how does it actually work?](#but-how-does-it-actually-work)
    - [But does it actually work?](#but-does-it-actually-work)

<!-- markdown-toc end -->

## TL;DR

### How do I get it

From [MELPA][melpa], using your package manager of choice.
See [Installation][installation]. Emacs 25 and later is supported,
please submit an issue if you want `el-patch` to support Emacs 24.

### What is it

Like the [advice][advice] system, `el-patch` provides a way to
customize the behavior of Emacs Lisp functions that do not provide
enough variables and hooks to let you make them do what you want. The
advantage of using `el-patch` is that you will be notified if the
definition of a function you are customizing changes, so that you are
aware your customizations might need to be updated.

Using the same mechanism, `el-patch` also
provides [a way][lazy-loading] to make lazy-loading packages much more
easy, powerful, and robust.

## Installation

`el-patch` is available on [MELPA][melpa]. It is easiest to install it
using [`straight.el`][straight.el]:

    (straight-use-package 'el-patch)

However, you may install using any other package manager if you
prefer.

## Why does it exist

Emacs provides a comprehensive set of customizable variables and hooks
as well as a powerful [advice][advice] system. Sometimes, however,
these are not enough and you must override the entire function in
order to change a detail of its implementation.

Such a situation is not ideal, since the original definition of the
function might change when you update Emacs or one of its packages,
and your overridden version would then be outdated. This could prevent
you from benefitting from bugfixes made to the original function, or
even introduce new bugs into your configuration. Even worse, there is
no way to tell when the original definition has changed! The
correctness of your configuration is basically based on faith.

`el-patch` introduces another way to override Emacs Lisp functions.
You can provide a *patch* which simutaneously specifies both the
original and modified definitions of the function. When Emacs starts
up, your patches act just like you had overridden the functions they
are modifying. However, you can later ask `el-patch` to *validate*
your patches—that is, to make sure that the original function
definitions have not changed since you created the patches. If they
have, `el-patch` will show you the difference using Ediff.

## Basic usage

Consider the following function defined in
the [`company-statistics`][company-statistics] package:

    (defun company-statistics--load ()
      "Restore statistics."
      (load company-statistics-file 'noerror nil 'nosuffix))

Suppose we want to change the third argument from `nil` to
`'nomessage`, to suppress the message that is logged when
`company-statistics` loads its statistics file. We can do that by
placing the following code in our `init.el`:

    (el-patch-defun company-statistics--load ()
      "Restore statistics."
      (load company-statistics-file 'noerror
            (el-patch-swap nil 'nomessage)
            'nosuffix))

Simply calling `el-patch-defun` instead of `defun` defines a no-op
patch: that is, it has no effect (well, not
quite—see [later][lazy-loading]). However, by including *patch
directives*, you can make the modified version of the function
different from the original.

In this case, we use the `el-patch-swap` directive. The
`el-patch-swap` form is replaced with `nil` in the original definition
(that is, the version that is compared against the "official"
definition in `company-statistics.el`), and with `'nomessage` in the
modified definition (that is, the version that is actually evaluated
in your init-file).

## Patch directives

* `(el-patch-add ARGS...)`

  Insert forms. In the original definition, the entire form is
  removed, and in the modified definition, each of the `ARGS` is
  spliced into the surrounding form. For example, the following patch:

      (foo (el-patch-add bar baz) quux)

  resolves to this in the modified definition:

      (foo bar baz quux)

* `(el-patch-remove ARGS...)`

  Remove forms. This is just like `el-patch-add`, except that the
  roles of the original and modified definitions are exchanged.

* `(el-patch-swap OLD NEW)`

  Replace one form with another. In the original definition, the
  entire form is replaced with `OLD`, and in the modified definition,
  the entire form is replaced with `NEW`.

* `(el-patch-wrap [TRIML [TRIMR]] ARGS)`

  Wrap forms in a list, optionally prepending or postpending
  additional forms. This is the most complicated directive, so an
  example will probably be helpful. The following patch:

      (el-patch-wrap 1 1
        (or
         (eq (get-text-property (point) 'face) 'font-lock-doc-face)
         (eq (get-text-property (point) 'face) 'font-lock-string-face)))

  resolves to this in the original definition:

      (eq (get-text-property (point) 'face) 'font-lock-doc-face)

  and this in the modified definition:

      (or
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)
       (eq (get-text-property (point) 'face) 'font-lock-string-face)))

  That is, the original `eq` call has been wrapped in an additional
  list, and also it has had forms inserted before and after it. The
  first `1` in the call to `el-patch-wrap` is the number of forms to
  insert before it, and the second `1` is the number of forms to
  insert after it.

  What you provide to `el-patch-wrap` for `ARGS` is the fully wrapped
  form, so you can think of `TRIML` and `TRIMR` as the number of forms
  to trim from each end of the `ARGS` before removing the surrounding
  parentheses.

  You can omit both `TRIML` and `TRIMR`; each defaults to zero. Notice
  that `ARGS` is always a list, so the number of arguments is either
  one, two, or three—thus eliminating any ambiguity about which
  argument is which.

* `(el-patch-splice [TRIML [TRIMR]] ARGS)`

  Splice forms into their containing form, optionally removing some
  from the beginning and end first. This is just like `el-patch-wrap`,
  except that the roles of the original and modified definitions are
  exchanged.

* `(el-patch-let VARLIST ARG)`

  Sometimes you need to restructure a form in an inconvenient way. For
  example, suppose you need to turn the following form:

      (if $cond
          $then
        $else)

  into the following form:

      (cond
       ($cond $then)
       ($new-cond $new-then)
       (t $else))

  where `$cond`, `$then`, `$new-cond`, `$new-then`, and `$else` are
  all long forms with many sub-expressions. You could do it in the
  following way:

      (el-patch-swap
        (if $cond
            $then
          $else)
        (cond
         ($cond $then)
         ($new-cond $new-then)
         (t $else)))

  However, this is not ideal because you have repeated the forms and
  violated [DRY][dry].

  You could achieve the patch without any repetition by using the
  basic patch directives, but that would be hard to read. Wouldn't it
  be great if you could just do the following?

      (el-patch-let (($cond (... long form ...))
                     ($then (... another form ...))
                     ($else (... more code ...))
                     ($new-cond (... even more ...))
                     ($new-then (... lots more code ...)))
        (el-patch-swap
          (if $cond
              $then
            $else)
          (cond
           ($cond $then)
           ($new-cond $new-then)
           (t $else))))

  Well, you can. Welcome to `el-patch`.

* `(el-patch-literal ARG)`

  Hopefully this will never happen, but you might need to use
  `el-patch` to modify functions that use symbols like `el-patch-add`
  at the beginning of lists. To avoid that, you can wrap a form in
  `el-patch-literal` to prevent anything within from being interpreted
  by `el-patch`. For example, the following form:

      (foo (el-patch-literal (el-patch-add bar baz)) quux)

  will be replaced with:

      (foo (el-patch-add bar baz) quux)

  in both the original and modified definitions.

## Defining patches

To patch a function, start by copying its definition into your
init-file, and replace `defun` with `el-patch-defun`. Then modify the
body of the function to use patch directives, so that the modified
definition is what you desire.

You can also patch other types of definitions using:

* `el-patch-defmacro`
* `el-patch-defsubst`
* `el-patch-defvar`
* `el-patch-defconst`
* `el-patch-defcustom`
* `el-patch-define-minor-mode`

Some warnings:

* Patching `defmacro`, `defsubst`, and `defconst` forms will not
  affect usages of them in already-defined functions, due to
  macroexpansion and byte-compilation. You may need to define no-op
  patches of client functions to get your changes to show up. Or take
  a different strategy—figuring out the best way to make a particular
  change to an internal function is often a complex process. You may
  also consider using advice, dynamic binding, and just plain forking
  the package.

* Patching `defvar`, `defconst`, and `defcustom` forms will not affect
  the value of the variable, if it has already been defined. Thus,
  they are only useful for lazy-loading by default. To override this
  behavior and force the patches to reset the value of the variable,
  if it is already defined, set `el-patch-use-aggressive-defvar`.

* Using `el-patch-use-aggressive-defvar` together with a custom
  `:variable` in `el-patch-define-minor-mode` is not currently
  supported. If you have a need for this use case, open an issue.

## Inspecting patches

You can run Ediff on a patch (original vs. modified definitions) by
running `M-x el-patch-ediff-patch` and selecting the desired patch.
Note that in this context, the "original" definition is the one
specified by the patch, not the actual definition that is checked when
you validate the patch (see below).

## Validating patches

To validate a patch, run `M-x el-patch-validate` and select the
desired patch. A warning will be printed if there is a difference
between what the patch definition asserts the original definition of
the function is and the actual original definition of the function.

If there is a difference, you can visualize it using Ediff with `M-x
el-patch-ediff-conflict`.

You can validate all the patches that have been defined so far using
`M-x el-patch-validate-all`.

## Removing patches

Use `M-x el-patch-unpatch`. Note that this does not technically remove
the patch: instead, it sets the function or variable definition to the
"original" definition as specified by the patch. These two actions
will, however, be equivalent as long as the patch is not outdated
(i.e., it is validated without errors by `M-x el-patch-validate`).

## Lazy-loading packages

`el-patch` does not mind if you patch a function that is not yet
defined. You can therefore use `el-patch` to help lazy-load a package.

As an example, consider the [Ivy][ivy] package. Ivy provides a minor
mode called `ivy-mode` that sets `completing-read-function` to
`ivy-completing-read`. The idea is that you call this function
immediately, so that when a `completing-read` happens, it calls into
the Ivy code.

Now, `ivy-completing-read` is autoloaded. So Ivy does not need to be
loaded immediately: as soon as `ivy-completing-read` is called, Ivy
will be loaded automatically. However, calling `ivy-mode` will trigger
the autoload for Ivy, so we can't do that if we want to lazy-load the
package. The natural thing to do is to copy the definition of
`ivy-mode` into our init-file, but what if the original definition
changes? That's where `el-patch` comes in. The code from Ivy looks
like this:

    (defvar ivy-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map [remap switch-to-buffer]
          'ivy-switch-buffer)
        (define-key map [remap switch-to-buffer-other-window]
          'ivy-switch-buffer-other-window)
        map)
      "Keymap for `ivy-mode'.")

    (define-minor-mode ivy-mode
      "Toggle Ivy mode on or off.
    Turn Ivy mode on if ARG is positive, off otherwise.
    Turning on Ivy mode sets `completing-read-function' to
    `ivy-completing-read'.

    Global bindings:
    \\{ivy-mode-map}

    Minibuffer bindings:
    \\{ivy-minibuffer-map}"
      :group 'ivy
      :global t
      :keymap ivy-mode-map
      :lighter " ivy"
      (if ivy-mode
          (progn
            (setq completing-read-function 'ivy-completing-read)
            (when ivy-do-completion-in-region
              (setq completion-in-region-function 'ivy-completion-in-region)))
        (setq completing-read-function 'completing-read-default)
        (setq completion-in-region-function 'completion--in-region)))

To enable `ivy-mode` while still lazy-loading Ivy, simply copy those
definitions to your init-file before the call to `ivy-mode`, replacing
`defvar` with `el-patch-defvar` and replacing `define-minor-mode` with
`el-patch-define-minor-mode`. That is:

    (el-patch-defvar ivy-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map [remap switch-to-buffer]
          'ivy-switch-buffer)
        (define-key map [remap switch-to-buffer-other-window]
          'ivy-switch-buffer-other-window)
        map)
      "Keymap for `ivy-mode'.")

    (el-patch-define-minor-mode ivy-mode
      "Toggle Ivy mode on or off.
    Turn Ivy mode on if ARG is positive, off otherwise.
    Turning on Ivy mode sets `completing-read-function' to
    `ivy-completing-read'.

    Global bindings:
    \\{ivy-mode-map}

    Minibuffer bindings:
    \\{ivy-minibuffer-map}"
      :group 'ivy
      :global t
      :keymap ivy-mode-map
      :lighter " ivy"
      (if ivy-mode
          (progn
            (setq completing-read-function 'ivy-completing-read)
            (when ivy-do-completion-in-region
              (setq completion-in-region-function 'ivy-completion-in-region)))
        (setq completing-read-function 'completing-read-default)
        (setq completion-in-region-function 'completion--in-region)))

    (ivy-mode 1)

    (featurep 'ivy) ;; => ivy is still not loaded!

It's really that simple!

## Validating patches that are not loaded yet

If you want to define a patch for a function provided by an unloaded
feature, it is likely that you will just put the patch in a
`with-eval-after-load` for the feature. But then `el-patch-validate`
and `el-patch-validate-all` will not be able to validate your patch,
because it is not yet defined.

To get around this problem, you can add functions to
`el-patch-pre-validate-hook` in order to make sure all your patches
are defined (for instance, you might need to require some features or
even enable a custom minor mode). This hook is run before
`el-patch-validate-all`, and also before `el-patch-validate` when you
provide a prefix argument.

Since defining some patches after a feature is loaded is such a common
operation, `el-patch` provides a convenience macro for it:
`el-patch-feature`. You can call this macro with an (unquoted) feature
name, and it will create a function that loads that feature, and add
it to `el-patch-pre-validate-hook` for you.

If you don't want all of your patches to be defined all the time, you
can put some functions in `el-patch-post-validate-hook` to disable
them again. For some examples of how to use these hooks, check out
[Radian Emacs][radian].

## But how does it work?

Magic.

## But how does it actually work?

The basic idea is simple. When a patch is defined, the patch
definition is resolved to figure out the modified definition is. Then
that definition is installed by evaluating it (but using
`el-patch--stealthy-eval`, so that looking up the function definition
will return the original location rather than the `el-patch`
invocation location, and also using `makunbound` to override a
previous variable definition if `el-patch-use-aggressive-defvar` is
non-nil).

The patch definition is also recorded in the hash `el-patch--patches`.
This allows for the functionality of `M-x el-patch-ediff-patch`.
Obtaining the actual original definition of a function is done using a
modified version of `find-function-noselect`, which provides for `M-x
el-patch-validate` and `M-x el-patch-ediff-conflict`.

When you call `M-x el-patch-unpatch`, the patch definition is resolved
again and the original version is installed by evaluating it.

## `el-patch` is not needed at runtime

El-patch computes the patched definition and generates the code to
register and apply it at compile time. Therefore, `el-patch` need not be loaded at runtime.
This means you can write:

    (eval-when-compile
      (require 'el-patch))
    (defvar el-patch--patches (make-hash-table))

And `el-patch` will not be loaded on init!

## But does it actually work?

It doesn't seem to crash [my Emacs][radian], at least.

[installation]: #installation
[lazy-loading]: #lazy-loading-packages

[advice]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
[company-statistics]: https://github.com/company-mode/company-statistics
[dry]: https://en.wikipedia.org/wiki/Don't_repeat_yourself
[ivy]: https://github.com/abo-abo/swiper
[melpa]: http://melpa.org
[package.el]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
[quelpa-use-package]: https://github.com/quelpa/quelpa-use-package
[quelpa]: https://github.com/quelpa/quelpa
[radian]: https://github.com/raxod502/radian
[straight.el]: https://github.com/raxod502/straight.el
[use-package]: https://github.com/jwiegley/use-package

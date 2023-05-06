# el-patch

> Future-proof your Emacs Lisp customizations!

## Table of contents

<!-- longlines-start -->

<!-- toc -->

- [TL;DR](#tldr)
  * [How do I get it](#how-do-i-get-it)
  * [What is it](#what-is-it)
- [Installation](#installation)
- [Why does it exist](#why-does-it-exist)
- [Basic usage](#basic-usage)
- [Patch directives](#patch-directives)
- [Defining patches](#defining-patches)
  * [Defining forks](#defining-forks)
- [Inspecting patches](#inspecting-patches)
- [Validating patches](#validating-patches)
- [Removing patches](#removing-patches)
- [Lazy-loading packages](#lazy-loading-packages)
- [Validating patches that are not loaded yet](#validating-patches-that-are-not-loaded-yet)
- [Integration with `use-package`](#integration-with-use-package)
- [Templates](#templates)
- [Patch variants](#patch-variants)
- [Usage with byte-compiled init-file](#usage-with-byte-compiled-init-file)
- [But how does it work?](#but-how-does-it-work)
- [But how does it actually work?](#but-how-does-it-actually-work)
- [But does it actually work?](#but-does-it-actually-work)
- [Contributor guide](#contributor-guide)

<!-- tocstop -->

<!-- longlines-stop -->

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
these are not enough and you must override an entire function in order
to change a detail of its implementation.

Such a situation is not ideal, since the original definition of the
function might change when you update Emacs or one of its packages,
and your overridden version would then be outdated. This could prevent
you from benefitting from bugfixes made to the original function, or
introduce new bugs into your configuration. Even worse, there is no
way to tell when the original definition has changed! The correctness
of your configuration is basically based on faith.

`el-patch` introduces another way to override Emacs Lisp functions.
You can provide a *patch* which simultaneously specifies both the
original and modified definitions of the function. When Emacs starts
up, your patches act just like you had overridden the functions they
are modifying. However, you can later ask `el-patch` to *validate*
your patches—that is, to make sure that the original function
definitions have not changed since you created the patches. If they
have, `el-patch` will show you the difference using Ediff.

Of course, in an ideal world, `el-patch` would not be necessary,
because user options and hooks could be made configurable enough to
satisfy everyone's needs. Unfortunately, that will never truly be
possible (or, arguably, desirable), so—like the advice
system—`el-patch` offers a concession to the practical needs of your
Emacs configuration.

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

    (el-patch-feature company-statistics)
    (with-eval-after-load 'company-statistics
      (el-patch-defun company-statistics--load ()
        "Restore statistics."
        (load company-statistics-file 'noerror
              (el-patch-swap nil 'nomessage)
              'nosuffix)))

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

Note that it is important to cause the patch to be loaded *after*
`company-statistics` is loaded. Otherwise, when `company-statistics`
is loaded, the patch will be overwritten!

You may also be wondering what `el-patch-feature` does. The patch will
still work without it; however, until `company-statistics` is actually
loaded, `el-patch` will not be aware that you have defined the patch
(since the code has not been run yet). Telling `el-patch` that you
define a patch inside a `with-eval-after-load` for
`company-statistics` allows [`M-x el-patch-validate-all`][validation]
to make sure to validate *all* your patches, and not just the ones
currently defined. See
also [Validating patches that are not loaded yet][not-loaded-yet].

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

* `(el-patch-wrap [TRIML [TRIMR]] ARGS...)`

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

* `(el-patch-splice [TRIML [TRIMR]] ARGS...)`

  Splice forms into their containing form, optionally removing some
  from the beginning and end first. This is just like `el-patch-wrap`,
  except that the roles of the original and modified definitions are
  exchanged.

* `(el-patch-let VARLIST ARGS...)`

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

* `(el-patch-literal ARGS...)`

  Hopefully this will never happen, but you might need to use
  `el-patch` to modify functions that use symbols like `el-patch-add`.
  In this case, you can wrap a form in `el-patch-literal` to prevent
  anything within from being interpreted by `el-patch`. For example,
  the following form:

      (foo (el-patch-literal (el-patch-add bar baz)) quux)

  will be replaced with:

      (foo (el-patch-add bar baz) quux)

  in both the original and modified definitions. Thus, you can happily
  write `el-patches` that patch other `el-patch` definitions :)

* `(el-patch-concat ARGS...)`

  This patch directive lets you concatenate strings. It is useful for
  modifying long string literals. For example, let's say that you have
  a string

      "Pretend this is a very long string we only want to write once"

  in a function you are patching. To change just a small part of this
  string, you could use `el-patch-swap` directly:

      (el-patch-swap
        "Pretend this is a very long string we only want to write once"
        "Pretend this is a really long string we only want to write once")

  But this repeats the rest of the string, violating DRY. Imagine if
  you just want to add a sentence to a 40-line docstring! Here's an
  alternative:

      (el-patch-concat
        "Pretend this is a "
        (el-patch-swap "very" "really")
        " long string we only want to write once")

  Basically, `el-patch-concat` just resolves all of its arguments,
  which may contain arbitrary patch directives, and then concatenates
  them as strings and splices the result into *both* the original and
  modified definition.

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
  even if it is already defined, set `el-patch-use-aggressive-defvar`.

You can patch any definition form, not just those above. To register
your own definition types, use the `el-patch-deftype` macro. For
example, the `el-patch-defun` function is defined as follows:

    (el-patch-deftype defun
      :classify el-patch-classify-function
      :locate el-patch-locate-function
      :font-lock el-patch-fontify-as-defun
      :declare ((doc-string 3)
                (indent defun)))

See the docstrings on the macro `el-patch-deftype` and the variable
`el-patch-deftype-alist` for more detailed information. See also the
source code of `el-patch` for examples of how to use
`el-patch-deftype`.

### Defining forks

Sometimes you want to define a *slightly modified* version of a
function, so that you can use the patched version in your own code but
you can still use the original version under its original name. This
is easy to do:

    (el-patch-defun (el-patch-swap my-old-fn my-new-fn) ...)

Be sure to include patch directives in the function body showing how
your modified function is derived from the original, just like in any
other patch.

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

Assuming you are byte-compiling your init-file, you can set
`el-patch-validate-during-compile` to non-nil to validate patches when
they are byte-compiled. There is no option to validate patches at
runtime during startup because this makes startup incredibly slow.
However, you could manually run `el-patch-validate-all` if such
behavior is truly desired.

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

## Integration with `use-package`

You can enable the `use-package` integration of `el-patch` by toggling
the global minor mode `el-patch-use-package-mode`, but it is more
convenient to set the variable
`el-patch-enable-use-package-integration` (defaults to non-nil) and
then the mode will be toggled appropriately once `el-patch` and
`use-package` have both been loaded.

The `use-package` integration defines two new `use-package` keywords,
`:init/el-patch` and `:config/el-patch`. They are analogous to `:init`
and `:config`, but each top-level form is converted into an `el-patch`
form: for example, a `defun` will be turned into an `el-patch-defun`,
and so on. (Definition forms that have no corresponding `el-patch`
macro are left as is.) The resulting code is prepended to the code in
`:init` or `:config`, respectively. Also, if you provide either
keyword, then a call to `el-patch-feature` is inserted into the
`:init` section.

## Templates

In some cases, you may want to patch one or two forms in a long
definition of a function or a macro. Defining the patch would still
require copying all unpatched forms and updating the patch when these
forms change. For these cases, it would be better if we can simply
search for the forms that we want to patch in the original definition
and patch only those. Enter `el-patch` templates.

As an example, say we want to define a patch of `restart-emacs` so
that it starts a new emacs instance without killing the current one.
Instead of defining a patch that includes the complete definition of
`restart-emacs`, we can define a template as follows

    (el-patch-define-template
      (defun (el-patch-swap restart-emacs radian-new-emacs))
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
      (restart-args ...)
      (el-patch-remove (kill-emacs-hook ...))
      (el-patch-swap
        (save-buffers-kill-emacs)
        (restart-emacs--launch-other-emacs restart-args)))

The first argument is a list that comprises the type, `defun` in this
case, and the name of the object that we are patching. Using an
`el-patch-swap` here allows us to define a fork, `radian-new-emacs`.
Had we wanted to simply patch the function we would pass `(defun
restart-emacs)` as the first argument. Every other argument defines a
template for a patch. To build the final patch, every argument is
resolved to figure out the original form which is then matched against
all forms in the original definition of the object and, if uniquely
found, the patch is spliced in its place. The special form `...` is
used to match one or more forms or, if it is inside `el-patch-concat`
as above, one or more characters in a string. Patch templates need not
be, or even contain, `el-patch-*` directives. For example, the purpose
of the argument `(restart-args ...)` is to make sure that such a form
exists in the function definition without actually patching it.

After defining the template, you can run the interactive command
`el-patch-insert-template` to insert the patch definition in the
current buffer based on the defined template. Alternatively, you may
use the command `el-patch-eval-template` which directly evaluates the
patch. The function `el-patch-define-and-eval-template` defines and
evaluates a template in one go. It is recommended that you compile
your init-file if you use `el-patch-define-and-eval-template` to avoid
the overhead of template matching when starting Emacs. `el-patch` will
issue a warning if `el-patch-define-and-eval-template` is called at
runtime and `el-patch-warn-on-eval-template` is non-nil (which is the
default).

Templates assume that the original definition of the object is
accessible, for example, using `find-function-noselect` for functions.

Like patches, templates can be validated using
`el-patch-validate-template` and `el-patch-validate-all-templates`.

## Patch variants

You can define multiple versions of the same patch. Normally,
(re)defining a patch will just overwrite the old version entirely.
However, if you dynamically bind `el-patch-variant` to a different
(symbol) value for each call, then the latter patch is still the one
that takes effect, but `el-patch` retains a record of both patches,
meaning they can be inspected and validated individually. See
[#29](https://github.com/radian-software/el-patch/issues/29).

You may also define patches of functions as `:override` advices
instead of overriding the original definition. This is done by setting
`el-patch-use-advice` to a non-nil value (either dynamically around a
patch or globally). The patched function must have the same name and
number of arguments as the original function.

## Usage with byte-compiled init-file

`el-patch` does not need to be loaded at runtime just to define
patches. This means that if you byte-compile your init-file, then
`el-patch` will not be loaded when you load the compiled code.

For this to work, you will need to stick to defining patches with
`el-patch-def*` and declaring features with `el-patch-feature`.
Anything else will cause `el-patch` to be loaded at runtime.

If you do not byte-compile your init-file, then all of this is
immaterial.

## But how does it work?

Magic.

## But how does it actually work?

The basic idea is simple. When a patch is defined, the patch
definition is resolved to figure out the modified definition is. Then
that definition is installed by evaluating it (by using
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

## But does it actually work?

It doesn't seem to crash [my Emacs][radian], at least.

## Contributor guide

Please see [the contributor guide for my
projects](https://github.com/radian-software/contributor-guide).

[installation]: #installation
[lazy-loading]: #lazy-loading-packages
[not-loaded-yet]: #validating-patches-that-are-not-loaded-yet
[validation]: #validating-patches

[advice]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
[company-statistics]: https://github.com/company-mode/company-statistics
[dry]: https://en.wikipedia.org/wiki/Don't_repeat_yourself
[ivy]: https://github.com/abo-abo/swiper
[melpa]: http://melpa.org
[package.el]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
[radian]: https://github.com/radian-software/radian
[straight.el]: https://github.com/raxod502/straight.el
[use-package]: https://github.com/jwiegley/use-package

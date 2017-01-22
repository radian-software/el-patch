# el-patch

> Future-proof your Emacs Lisp customizations!

## Quick start

Using `quelpa-use-package`:

    (use-package el-patch
      :ensure t
      :demand t
      :quelpa (el-patch :fetcher github :repo "raxod502/el-patch"))

Manually:

    $ git clone https://github.com/raxod502/el-patch.git

    (add-to-list 'load-path "/path/to/el-patch")
    (require 'el-patch)

`el-patch` is not yet hosted on MELPA. If you love `el-patch`, feel
free to open a pull request on MELPA to add it.

## Motivation

Emacs provides a comprehensive set of customizable variables and hooks
as well as a powerful [advice] system. Sometimes, however, these are
not enough and you must override the entire function in order to
change a detail of its implementation.

[advice]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html

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

Consider the following function defined in the [`company-statistics`]
package:

    (defun company-statistics--load ()
      "Restore statistics."
      (load company-statistics-file 'noerror nil 'nosuffix))

[`company-statistics`]: https://github.com/company-mode/company-statistics

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
patch: that is, it has no effect (well, not quite—see [later]).
However, by including *patch directives*, you can make the modified
version of the function different from the original.

[later]: #autoloading

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
  violated [DRY].

  [dry]: https://en.wikipedia.org/wiki/Don't_repeat_yourself

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

        (foo (patch-literal (patch-add bar baz)) quux)

  will be replaced with:

        (foo (patch-add bar baz) quux)

  in both the original and modified definitions.

## Defining patches

To patch a function, start by copying its definition into your
init-file, and replace `defun` with `el-patch-defun`. Then modify the
body of the function to use patch directives, so that the modified
definition is what you desire.

You can also patch other types of definitions using
`el-patch-defmacro` and `el-patch-defsubst`.

## Inspecting patches

You can run Ediff on a patch (original vs. modified definitions) by
running `M-x el-patch-ediff-patch` and selecting the desired patch.

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

Use `M-x el-patch-unpatch`.

## Autoloading

### Patching functions that are not loaded yet

If you've gotten far enough into Emacs to be interested in something
like `el-patch`, you probably lazy-load a lot of your packages.
`el-patch` uses advice under the hood to implement your patches, and
unfortunately it's not possible to define advice on functions that are
not loaded yet—even if they are autoloaded.

Therefore, trying to `el-patch-defun` an unloaded function will cause
an error. For example, the following will not work and will signal an
error:

        (el-patch-defsubst clojure-in-docstring-p ()
          "Check whether point is in a docstring."
          (el-patch-wrap 1 1
            (or
             (eq (get-text-property (point) 'face) 'font-lock-doc-face)
             (eq (get-text-property (point) 'face) 'font-lock-string-face))))

However, you can fix the problem by placing an `el-patch-feature`
directive somewhere in the patch definition:

        (el-patch-defsubst clojure-in-docstring-p ()
          "Check whether point is in a docstring."
          (el-patch-feature clojure-mode)
          (el-patch-wrap 1 1
            (or
             (eq (get-text-property (point) 'face) 'font-lock-doc-face)
             (eq (get-text-property (point) 'face) 'font-lock-string-face))))

Once `el-patch` knows what feature (in this case `clojure-mode`)
provides the function being patched, it can generate a stub function
that acts as an autoload, and you can therefore patch unloaded
functions just fine.

### Using `el-patch` to help lazy-load packages

One side effect of this is that you can use `el-patch` to help you
lazy-load packages. Many packages have a function you can call to
establish keybindings for the functions in the package. However,
calling this function during init defeats the point of lazy-loading,
since doing so will cause the whole package to be loaded immediately.

The obvious solution is to copy the keybinding-establishing function
into your init-file, but then what if the original definition changes?
You can use `el-patch` to validate your copy of the function: just
define a no-op patch by copying the function definition and replacing
`defun` with `el-patch-defun` (and making sure to specify
`el-patch-feature`).

### Validating patches that are not loaded yet

If you want to define a patch for a function provided by an unloaded
feature, it is more likely that you will just put the patch in a
`with-eval-after-load` for the feature. But then `el-patch-validate`
and `el-patch-validate-all` will not be able to validate your patch,
because it is not yet defined.

To get around this problem, you can add functions to
`el-patch-pre-validate-hook` in order to make sure all your patches
are defined (for instance, you might need to require some features or
even enable a custom minor mode). This hook is run before
`el-patch-validate-all`, and also before `el-patch-validate` when you
provide a prefix argument.

If you don't want all of your patches to be defined all the time, you
can put some functions in `el-patch-post-validate-hook` to disable
them again. For some examples of how to use these hooks, check out [my
Emacs init-file].

[my emacs init-file]: https://github.com/raxod502/radian/blob/master/init.el

## But how does it work?

The basic idea is simple. When a patch is defined, the patch
definition is resolved to figure out the modified definition is. Then
a private function is defined using that definition, and an
`:override` advice is added to the actual function to instal the
definition. When you call `M-x el-patch-unpatch`, the advice is
removed.

Whenever a patch is defined, it's also recorded in the hash
`el-patch--patches`, which allows for the functionality of `M-x
el-patch-ediff-patch`. Obtaining the actual original definition of a
function is done using a modified version of `find-function-noselect`,
which provides for `M-x el-patch-validate` and `M-x
el-patch-ediff-conflict`.

Unloaded functions are trickier, and the code to handle them is liable
to change soon. Basically, `el-patch` generates its own autoload stub
to override any existing autoload, and then adds the advice to that.

## But does it actually work?

It doesn't seem to crash [my Emacs], at least.

[my emacs]: https://github.com/raxod502/radian/blob/master/init.el

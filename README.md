# emacs.g

Portable editor configuration for development tasks.

## Installation

First clone the repository.  On Linux and Mac OS machines you can do this
with:

``` shell
git clone git@github.com:brett-lempereur/emacs.g $HOME/.emacs.d
```

Then, make sure that you have `coreutils` and `texinfo` on your path.  If
you're using Nix you can use the provided environment:

``` shell
cd $HOME/.emacs.d
nix-shell
```

Then bootstrap the configuration and build packages.  I recommend also
compiling native code at this point:

``` shell
make bootstrap-borg
make bootstrap
make native
```

## Features

In this iteration of the configuration I've tried to switch to lightweight
alternatives of critical packages.  I'm using `eglot` as a language server
client now, and `corfu` for completion.  Most other things have remained
the same.

The following programming languages are configured:

* Common Lisp
* Emacs Lisp
* Haskell
* OCaml
* Python
* Racket
* Rust

And the following markup languages:

* JSON
* Markdown
* TOML
* YAML

If you want to customise the minor modes for these languages then check
[user-hooks.el](user-lisp/user-hooks.el), the configuration is centralised
there now.

## Customisation

Machine-specific customisation is in the `machine-lisp` path.  The most
obvious things to customise are the appearance of the editor, check out
[user-settings.el](user-lisp/user-settings.el) for more options.

The names of these files are determined using the following function:

``` emacs-lisp
(defun user-machine-init ()
  "Return the symbol of the machine-specific initialisation module."
  (let ((hostname (car (split-string (system-name) "\\."))))
    (intern (downcase (format "user-machine-%s" hostname)))))
```

Such that `user-machine-rocinante.el` is the customisation file for the
machine `Rocinante.local`.  So that you don't end up accidentally checking
in sensitive local configuration, files matching `user-machine/*.el*` need
to be explicitly allowlisted in [.gitignore](.gitignore).

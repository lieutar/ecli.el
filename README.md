# ecli.el

Command Line App Framework for Emacs

Provide a mechanism for creating CLI applications in Emacs Lisp.
The applications you can create
have their logic determined by functions named in specific patterns,
and their documentation automatically creates help messages.

## Purpose

Emacs is a text editor, and no matter how powerful Emacs Lisp is, 
it is not suitable for creating general applications.

However, for things that deal with Emacs itself, 
such as processing that deals with the Emacs environment and the development 
environment for Emacs Lisp modules, Emacs Lisp, which has a detailed API, 
is the best choice for implementation.

ecli is a framework that focuses on command line argument processing to 
perform such processing in a CLI, and on creating help.

## Usage

### Define Functions to process your app's logic

``` emacs-lisp
(defun myapp-sub-foo ())

(defun myapp-sub-bar ())

(defun myapp-opt--help-h ()
  "Show this message.")

(defun myapp-opt--version-v ()
  "Show the version of this app.")
```

### Build your app's spec and use it

``` emacs-lisp
(let ((args command-line-args-left))
  (setq command-line-args-left nil)
  (ecli-process-args args :prefix 'myapp-))
```

### And then run your app

``` sh
emacs --script myapp.el
```

**with shell script**
``` sh
#! /usr/bin/env sh
self=`realpath $0`
here=`basename $self`
emacs --script $here/myapp.el
```
## License

This project is licensed under the GNU General Public License v3.
see: https://www.gnu.org/licenses/gpl-3.0.html

## Copyright

2025 lieutar <lieutar@gmail.com>


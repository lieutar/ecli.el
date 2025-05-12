;;; {{index}}.el --- {{brief}} -*-lexical-binding: t -*-

;; Copyright (C) {{copyright}}

;; Author: {{author}}
;; Version: {{version}}
;; Keywords: {{(keywords) (s-join ", " keywords)}}
;; URL: {{url}}
;; Package-Requires: {{(dependency)(lelde/tinplate/util::index-pr dependency)}}

;;; License:

;; {{(license) (s-replace "\n" "\n;; " license)}}

;;; Commentary:
;;
;; {{(commentary) (s-replace "[[:space:]]+\\(\n\\|\\'\\)" "\\1" (s-replace "\n" "
;; " (s-replace "\\`[[:space:]\n]+" "" commentary)))}}
;;
;;  About deteils of this, see: README.md
;;

;;; Code:


;;; Code:


;;;;; ecli/META
(defconst ecli-VERSION "0.1.0")



;;;; ecli/util
;;;;; internal utilities

(defun ecli/util::flatten (list)
  (apply #'append
         (mapcar
          (lambda (elem)
            (if (and elem
                     (listp elem)
                     (listp (cdr elem)))
                (ecli/util::flatten elem)
              (list elem)))
          list)))

(defun ecli/util::join (sep list)
  (mapconcat #'identity list sep))

(defun ecli/util::string-match (pattern string)
  (when (string-match pattern string)
    (let ((n 1)
          (result (list string)))
      (while n
        (let ((group (match-string n string)))
          (if group
              (progn (push group result)
                     (setq n (1+ n)))
            (setq n nil))))
      (reverse result))))

;;;; utilities for users

(define-error 'ecli-exit "Exit signal for `ecli'")

;;!export
(defun ecli/util::exit (&optional status)
  "Terminate your app's process.
ecli-process-args will returns the STATUS code.
If STATUS was omitted, it is recognized as 0."
  (signal 'ecli-exit (or status 0)))

;;!export
(defun ecli/util::say (string &rest objects)
  "Print string to standard output on tty.
"
  (princ (apply 'format (concat string "\n") objects)))

;;!export
(defun ecli/util::say-error (string &rest objects)
  ""
  (apply 'message string objects))



;;;; ecli/help

(defun ecli/help::--opts (opts)
  (mapcar (lambda (it)
            (let ((doc   (or (documentation (car it) "")))
                  (long  (cadr it))
                  (short (caddr it)))
              (format "\t%s %s\t%s"
                      long short
                      (replace-regexp-in-string "\n" "\n\t\t" doc))))
          (sort opts (lambda (a b)
                       (string< (car a) (car b))))))

(defun ecli/help::--subs (subs)
  (mapcar (lambda (it)
            (let ((cmd (car it))
                  (doc (or (documentation (cdr it)) "")))
              (format "\t%s\t%s" cmd
                      (replace-regexp-in-string "\n" "\n\t\t" doc))))
          (sort subs (lambda (a b)
                       (string< (car a) (car b))))))

;;!export
(defun ecli/help::help-message (spec)
  "Build help message from SPEC.
The message will be built your app's documentations on *opt-* and *sub-*
functions."
  (let ((header (plist-get spec :header))
        (prefix (plist-get spec :help-prefix))
        (opts   (plist-get spec :opts-src))
        (subs   (plist-get spec :subs)))
    (ecli/util::join
     "\n"
     (append
      (when prefix (list prefix))
      (when opts (cons "  Options:"        (ecli/help::--opts opts)))
      '("")
      (when subs (cons "  Sub Commands:" (ecli/help::--subs subs)))
      (list "")))))

;;!export
(defun ecli/help::help-default (spec)
  "Show help message built by SPEC.
And then exit by code 0"
  (ecli/util::say (ecli/help::help-message spec))
  (ecli/util::exit 0))


;;!export
(defun ecli/error::error-with-help ( spec string &rest objects )
  "Show error messages with the help message.
SPEC is an app spec `ecli' generated.
STRING and OBJECTS are format string having place holders and its values.
This error function shows message and help message.
If help isn't required, you can use `ecli-error-simple'"
  (ecli/util::say-error (format string objects))
  (ecli/util::say-error (ecli/help::help-message spec))
  (ecli/util::exit 1))

;;!export
(defun ecli/error::error-default ( spec string &rest objects )
  "Default error function for `ecli' apps.
SPEC is an app spec `ecli' generated. ( however this function doesn't use it )
STRING and OBJECTS are format string having place holders and its values.
This function shows only given message.
"
  (ecli/util::say-error (format string objects))
  (ecli/util::exit 1))



;;;; ecli/spec

;;;;; constructor for ecli/spec
(defun ecli/spec::--read-spec-from-obarray (prefix)
  ;; Generate spec by PREFIX.
  ;; For example:
  ;; where PREFIX := foo-
  ;; the function foo-sub-subcmd processes as a sub command
  ;; the function foo-opt--option-o processes options
  (let ((pattern-sub (format "\\`%ssub-\\(.+\\)\\'"            prefix))
        (pattern-opt (format "\\`%sopt--\\(.+?\\):-\\(.\\)\\'" prefix))
        (opts-src nil)
        (opts     nil)
        (subs     nil))
    (mapatoms ;; scan all symbols on current obarray
     (lambda (sym)
       (when (fboundp sym)
         (let* ((name (symbol-name sym))
                (match-opt (ecli/util::string-match pattern-opt name))
                (match-sub (ecli/util::string-match pattern-sub name)))
           (when match-opt
             (let ((long  (format "--%s" (nth 1 match-opt)))
                   (short (format "-%s"  (nth 2 match-opt))))
               (push (list sym long short) opts-src)
               (push (cons long  sym) opts)
               (push (cons short sym) opts)))
           (when match-sub
             (push (cons (nth 1 match-sub) sym) subs))))))
    (list :subs subs :opts opts :opts-src opts-src)))

(defun ecli/spec::--make-default (spec prefix)
  (let ((deffun (intern (format "%sdefault" prefix)))
        (help      (plist-get spec :help))
        (error-fun (plist-get spec :error)))
    (cond ((fboundp deffun) deffun)
          ((null (plist-get spec :subs))
           (lambda (args app-spec)
             (if (null args)
                 (progn (funcall help spec)
                        (ecli/util::exit 0))
               (funcall error (format "Error: Unprocessed args: %S\n" args)))))
          (t
           (lambda (args app-spec)
             (funcall error (format "Error: Unknown sub command: %s\n" (car args))))))))

(defun ecli/spec (options)
  (let* ((prefix (plist-get options :func-prefix))
         (spec   (append options
                         (ecli/spec::--read-spec-from-obarray prefix))))
    (unless (plist-get spec :help)
      (plist-put spec :help #'ecli/help::help-default))
    (unless (plist-get spec :error)
      (plist-put spec :error #'ecli/error::error-default))
    (unless (plist-get spec :default)
      (plist-put spec :default (ecli/spec::--make-default spec prefix)))
    spec))

;;;;; methods for ecli/spec

;;!export
(defun ecli/spec::get-options-plist (spec)
  (plist-get spec :options))

;;!export
(defun ecli/spec::put-option-value (spec prop value)
  ""
  (let ((plist (plist-get spec :options)))
    (if plist
        (plist-put plist prop value)
      (plist-put spec :options (list prop value)))))

;;!export
(defun ecli/spec::get-option-value (spec prop)
  ""
  (plist-get (plist-get spec :options) prop))

;;!export
(defun ecli/spec::push-option-value (spec prop value)
  ""
  (ecli/spec::put-option-value
   spec prop
   (cons value (ecli/spec::get-option-value spec prop))))

;;!export
(defun ecli/spec::help (spec)
  ""
  (funcall (plist-get spec :help) spec))

;;!export
(defun ecli/spec::error (spec message &rest opt)
  ""
  (apply (plist-get spec :error) spec message opt))


;;;; ecli/core

(defsubst ecli/core::--prepare-args (args)
  (mapcar (lambda (arg)
            (unless (stringp arg)
              (setq arg (cond ((keywordp arg)
                               (replace-regexp-in-string "\\`:" "--"
                                                         (symbol-name arg)))
                              ((null arg) "")
                              (t (format "%s" arg)))))
            (when (ecli/util::string-match "\\`@-" arg)
              (setq arg (replace-regexp-in-string "\\`@" "" arg)))
            arg)
          args))

(defsubst ecli/core::--process-args--process-opt (spec arg args opts)
  (when (ecli/util::string-match "\\`-" arg)
    (let ((optfun (cdr (assoc arg opts))))
      (if optfun
          (cons :args (funcall optfun args spec))
        (ecli/spec::error spec (format "Error: Unknown option %s\n" arg))))))

;;!export
(defun ecli/core::process-args (args &rest options)
  "`ecli''s main routine.
This function builds your app's specification and run it.
ARGS are command line arguments which given by the end user,
and OPTIONS are plist describes about your application.

OPTIONS receives following properties:

:func-prefix (string or symbol): Required.
        `ecli' build the app-spec according to this value.

        The function
        <func-prefix>opt--<long-option>:-<short-option> (args app-spec)
        will be a option handler.

        The function
        <func-prefix>sub-<sub-command> (args app-spec)
        will be a sub-command handler.

        The function
        <func-prefix>default (args app-spec)
        will be a default action handler.

:help-prefix (string): Optional but recommended to specify.
        The prefix of the auto generated help message.

:default (function): Optional.
        The function to process as a default action.
        If this is omitted, `ecli' will look up `<func-prefix>-default'
        function. And it isn't able defined, `ecli' will make it shows help
        message when empty args, and shows error when args will given.

:help (function) : Optional.
        The function to show help message.
        If this is omitted, `ecli' will make it from your app spec functions.

:error (function) : Optional.
        The function to show error message.
        This function receives error message and shows it on stderr.
        If this is omitted, `ecli' will make as display the message and
        help message built by your app spec.

The return value of this is the status code.
So, your app's main will be like:

(defun my-app-main ()
  (kill-emacs (ecli-process-args
               command-line-args-left
               :func-prefix 'my-app-
               :help-prefix \"Usage: my-app [options] <subcommand> <args>\")))
"

  (setq args (ecli/core::--prepare-args args))
  (condition-case err
      (let ((spec         (ecli/spec options)))
        (let ((subs         (plist-get spec :subs))
              (opts         (plist-get spec :opts))
              (default      (plist-get spec :default))
              subcommand)
          (while (and (null subcommand) args)
            (let* ((arg     (car args)))
              ;; process options by `ecli'
              (let ((result (ecli/core::--process-args--process-opt
                             spec arg args opts)))
                (if result (setq args (cdr result))
                  ;; process subcommand
                  (progn
                    (setq subcommand (or (when subs
                                           (let ((found (cdr (assoc arg subs))))
                                             (when found
                                               (setq args (cdr args))
                                               found)))
                                         default)))))))
          (funcall (or subcommand default) args spec)
          0))
    (ecli-exit (cdr err))))


(defconst ecli/app/installer/rsc::$installer.sh-src "#! /usr/bin/env bash

###
### Setup Installer's context
###

here=`dirname $BASH_SOURCE`
install_el=$(mktemp)

cleanup(){
    [ -f $install_el ] && rm $install_el
}

trap cleanup int
trap cleanup EXIT

###
### Location to be installed
###

# Path to be installed
prefix=<<default-prefix>>

# Path to the definition to local settings
local_config=
# About local settings
# See README.md on https://github.com/lieutar/ecli.el/
# The section \"Installer\" will express it.

###
### Process arguments and interaction
###

while [ $# -gt 0 ]; do
    arg=$1
    shift
    case \"$arg\" in
         # Path to be installed ( Default <<default-prefix>> )
        --prefix|-P)
            prefix=$1
            case \"$prefix\"; in
                \"\"|-*)
                    echo \"--prefix / -P requires an argument\"
                    while [ -z \"$prefix\" ]; do
                          printf \"Where? \"
                          read prefix
                    done
                    ;;
                *)
                    shift
                    ;;
            esac
            ;;

        # Setting to local elpa mirror and pre-downloaded scripts
        --local-config|-C)
            local_config=$1
            case \"$local_config\" in
                \"\" |-*)
                    echo \"--local-config / -C requires an argument\"
                    while [ -z \"$local_config\" ]; do
                        printf \"Where? \"
                        read local_config
                    done
                    while [ -n \"$local_config\" ] &&\\
                                ! [ -r \"$local_config\" ]; do
                        echo \"Can't read $local_config\"
                        printf \"Where? \"
                        echo \"(ignore local-config specification if blank)\"
                        printf \"> \"
                        read local_config
                    ;;
                *)
                    shift
                    ;;
            esac
            ;;

        # Process illegal arguments
        *)
            echo \"Warning: unrecognized argument: $arg\"
            printf \"Continue? (yes/no , default=no):\"
            read answer
            case \"$answer\" in
                yes|Yes|YES)
                    echo \"Continue ...\"
                    ;;
                *)
                    echo \"Abort\"
                    exit 1
                    ;;
            esac
        ;;
    esac
done

###
### Make Emacs Lisp installer
###

cat <<EOF >$install_el
<<logic-to-install>>
EOF

###
### Run the Emacs Lisp installer
###

# pass configurations by environment
export PREFIX=$prefix
export LOCAL_CONFIG_ELD=$local_config

if ! emacs --batch -Q -l $install_el -f eclinstall::main ; then
    echo \"Installation failed.\" >&2
    exit 1
fi
")


(defconst ecli/app/installer/rsc::$installer.el-src ";; -*- lexical-binding: t -*-
(require 'package)
(require 'url)

(defun eclinstall::get-local-config (eld)
  (when (and eld (file-exists-p eld))
    (read (with-temp-buffer
            (insert-file-contents eld)
            (buffer-substring-no-properties
             (point-min) (point-max))))))

(defun eclinstall::mkdirs (&rest dirs)
  (dolist (dir dirs)
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun eclinstall::modify-archives (package-archives
                                    local-config)
  ;; Update package-archives by local-config
  (let ((local-archives-alist (plist-get local-config :archives)))
    (dolist (lslot local-archives-alist)
      (let ((pslot (assoc (car lslot) package-archives)))
        (when pslot (setcdr pslot (cdr lslot)))))
    package-archives))

(defvar package-archives)
(defvar package-user-dir)
(defun eclinstall::install-deps (elpa-dir local-config archives depends-on)
  ;; Installation
  ;; Install dependencies using package.el
  (let ((package-archives
         (eclinstall::modify-archives archives local-config))
        (package-user-dir elpa-dir))
    (package-initialize)
    (dolist (dep depends-on)
      (package-install dep))))

(defun eclinstall::url-retrieve (url)
  (let ((response-buffer (url-retrieve-synchronously url t t nil))
        body)
    (unless response-buffer (error \"url-retrieve-synchronously returns nil\"))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      (when (string-match \"\\\\`https?://\" url)
        (let ((status-line
               (progn
                 (beginning-of-line 2)
                 (buffer-substring-no-properties (point-min)(point)))))
          (unless (string-match \"200 OK$\" status-line)
            (error \"url-retrieve %s failed: %s\" url status-line))))
      (search-forward \"\
\
\")
      (setq body (buffer-substring-no-properties (point)(point-max))))
    (kill-buffer response-buffer)
    body))

(defun eclinstall::install-from-url (url into mode)
  (let ((dst (expand-file-name
                (and (string-match \"\\\\([^/]+\\\\)\\\\'\" url) (match-string 1 url))
                into)))
    (with-temp-file dst (insert (eclinstall::url-retrieve url)))
    (chmod dst mode)))

(defun eclinstall::install-from-local (url url-to-local into mode)
  (let* ((src (funcall url-to-local url))
         (dst (expand-file-name (file-name-nondirectory src)
                                into)))
    (copy-file src dst)
    (chmod dst mode)))

(defun eclinstall::install-scripts
    (bin-dir url-launcher lisp-dir url-app.el local-config)
  ;; Install main scripts
  (let ((url-to-local         (plist-get local-config :url-to-local)))
    (let ((install
           (if (functionp url-to-local)
               ;; Get scripts from local repository
               (lambda (url into mode)
                 (eclinstall::install-from-local url url-to-local into mode))
             ;; Get scripts from public repository
             #'eclinstall::install-from-url)))
      (funcall install url-launcher bin-dir  #o755)
      (funcall install url-app.el   lisp-dir #o644))))

(defun eclinstall::main ()
  (let ((url-launcher \"<<url-launcher>>\")
        (url-app.el   \"<<url-app.el>>\")
        (prefix     (getenv \"PREFIX\"))
        (config.eld (getenv \"LOCAL_CONFIG_ELD\")))

    (let ((bin-dir  (format \"%s/bin\"        prefix))
          (lisp-dir (format \"%s/share/lisp\" prefix))
          (elpa-dir (format \"%s/cache/elpa\" prefix))
          (local-config (eclinstall::get-local-config config.eld)))

      ;; Make directory if not being.
      (eclinstall::mkdirs  bin-dir lisp-dir elpa-dir)
      (eclinstall::install-deps
       elpa-dir
       local-config
       '<<archives>>
       '<<depends-on>>)
      (eclinstall::install-scripts bin-dir  url-launcher
                                   lisp-dir url-app.el
                                   local-config))))
")


(defconst ecli/app/installer/rsc::$predefined-archives-alist
  '(("gnu"            . "https://elpa.gnu.org/packages/")
    ("nongnu"         . "https://elpa.nongnu.org/nongnu/")
    ("melpa"          . "https://melpa.org/packages/")
    ("melpa-stable"   . "https://stable.melpa.org/packages/")
    ("melpa-unstable" . "https://unstable.melpa.org/packages/")
    ("org"            . "https://orgmode.org/elpa/")
    ("looper"         . "https://lieutar.github.io/looper-elpa/")))


;;;; ecli/app/installer

(defsubst ecli/app/installer::--parse-elpa-spec (spec)
  ;; Convert "elpa-name=https://elpa.example.org/packages" into
  ;; ("elpa-name" . "https://elpa.example.org/packages" )
  (let ((result
         (when (stringp spec)
           (let ((match (ecli/util::string-match "\\`\\(.+?\\)=\\(.+\\)" spec)))
             (if match
                 (cons (nth 1 match) (nth 2 match))
               (assoc spec
                      ecli/app/installer/rsc::$predefined-archives-alist))))))
    (unless result (error "malformed elpa spec: %S" spec))
    result))

(defun ecli/app/installer::>opt--usage:-u (args app-spec)
  "Show this message.
"
  (ecli/spec::help app-spec)
  (ecli/util::exit 0))

(defun ecli/app/installer::>opt--show-version:-V (args app-spec)
  "Show version information.
"
  (ecli/util::say "ecli %s" ecli-VERSION)
  (ecli/util::exit 0))

(defun ecli/app/installer::>opt--archive:-a (args app-spec)
  "Specify package archive.

Basic specification method is, <name>=<url>.
For example: foo=https://foo.org/packages/

`ecli-make-installer' has several builtin sources:

    gnu, nongnu, melpa, melpa-stable,
    melpa-unstable, org and looper.

URLs of them can be omitted.

Default source specification is: --archive melpa-stable

This option is repeatable.
"
  (let ((info (ecli/app/installer::--parse-elpa-spec (cadr args))))
    (ecli/spec::push-option-value app-spec :archive info))
  (cddr args))

(defun ecli/app/installer::>opt--depends-on:-d (args app-spec)
  "Specify depending package.

This option is repeatable.
"
  (let ((mod (cadr args)))
    (ecli/spec::push-option-value app-spec :depends-on mod))
  (cddr args))

(defun ecli/app/installer::>opt--default-prefix:-P (args app-spec)
  "Specify path prefix to be installed.

This option is required.
"
  (ecli/spec::put-option-value app-spec :default-prefix (cadr args))
  (cddr args))

(defun ecli/app/installer::>opt--launcher:-L (args app-spec)
  "Specify the URL to download the launcher script's path.

This option is required.
"
  (ecli/spec::put-option-value app-spec :launcher (cadr args))
  (cddr args))

(defun ecli/app/installer::>opt--app-el:-A (args app-spec)
  "Specify the URL to download Emacs Lisp script entity.

This option is required.
"
  (ecli/spec::put-option-value app-spec :app-el (cadr args))
  (cddr args))

(defun ecli/app/installer::--fill-template (tmpl values)
  ;; Simple template filler
  ;; TMPL ..... template source
  ;; VALUES ... values alist
  (with-temp-buffer
    (insert tmpl)
    (goto-char (point-min))
    (while (re-search-forward "<<\\(.+?\\)>>" nil t)
      (let* ((to      (point))
             (ph-all  (match-string 0))
             (from    (- to (length ph-all)))
             (ph-name (match-string 1))
             (str (cdr (assoc ph-name values))))
        (unless str (error "Undefined placeholder value: %S" ph-all))
        (delete-region from to)
        (insert str)))
    (buffer-substring-no-properties (point-min)(point-max))))



(defun ecli/app/installer::--make-logic (values)
  ;; Create lisp script in bash the installer script
  (ecli/app/installer::--fill-template
   ecli/app/installer/rsc::$installer.el-src values))

(defun ecli/app/installer::--build (values)
  ;; Create bash installer script
  (ecli/app/installer::--fill-template
   ecli/app/installer/rsc::$installer.sh-src
   (append values
           (list (cons
                  "logic-to-install"
                  (ecli/app/installer::--make-logic values))))))

(defun ecli/app/installer::>default (args app-spec)
  ;; Default action for `ecli-make-installer'.
  (let ((default-prefix (ecli/spec::get-option-value app-spec :default-prefix))
        (url-launcher   (ecli/spec::get-option-value app-spec :launcher))
        (url-app.el     (ecli/spec::get-option-value app-spec :app-el))
        (archives (format "(\n%s)\n"
                          (mapconcat
                           (lambda (slot) (format "%S" slot))
                           (or (ecli/spec::get-option-value app-spec :archive)
                               (list (ecli/app/installer::--parse-elpa-spec
                                      "melpa-stable")))
                           "\n")))
        (depends-on (format "(\n%s)\n"
                            (mapconcat
                             (lambda (dep) (format "%s" dep))
                             (ecli/spec::get-option-value app-spec :depends-on)
                             "\n")))
        errors)

    ;; Check errors
    (unless default-prefix
      (push "--default-prefix / -P (The path to be installed) wasn't given."
            errors))
    (unless url-launcher
      (push "--launcher / -L (The URL to launcher) wasn't given." errors))
    (unless url-app.el
      (push "--app-el / -A (The URL to app.el) wasn't given." errors))
    (when errors
      (ecli/spec::error app-spec
                        (concat "Error occurs:\n    "
                                (mapconcat #'identity
                                           (reverse errors) "\n    ")
                                "\n")))

    ;; Output installer script
    (princ (ecli/app/installer::--build
            `(("archives"       . ,archives)
              ("depends-on"     . ,depends-on)
              ("default-prefix" . ,default-prefix)
              ("url-launcher"   . ,url-launcher)
              ("url-app.el"     . ,url-app.el))))))

(defun ecli/app/installer::make-installer--main (args)
  ;; avoid (kill-emacs) to test
  (ecli/core::process-args
   args
   :func-prefix 'ecli/app/installer::>
   :help-prefix (documentation 'ecli/app/installer::make-installer)))

;;!export
(defun ecli/app/installer::make-installer ()
  "Create installer script (written in bash) from options.

Usage: emacs --batch -Q -l ecli -f ecli-make-installer \\
      --default-prefix ~/.your-app \\
      --launcher https://github.com/you/your-app/your-app \\
      --app-el https://github.com/you/your-app/your-app.el \\
      [other options] > install.sh
"
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (kill-emacs (ecli/app/installer::make-installer--main args))))


;;;; ecli/app/context
;;!export
(defmacro ecli/app/context::setup-app-context (&optional elpa-dir)
  "Setup installable package"
  `(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
          (package-user-dir  (expand-file-name "../../cache/elpa" here)))
     (package-initialize)))

;;;###autoload
(defalias 'ecli-setup-app-context 'ecli/app/context::setup-app-context)

;;;###autoload
(defalias 'ecli-make-installer 'ecli/app/installer::make-installer)

;;;###autoload
(defalias 'ecli-process-args 'ecli/core::process-args)

;;;###autoload
(defalias 'ecli-error-with-help 'ecli/error::error-with-help)

;;;###autoload
(defalias 'ecli-error-default 'ecli/error::error-default)

;;;###autoload
(defalias 'ecli-help-message 'ecli/help::help-message)

;;;###autoload
(defalias 'ecli-help-default 'ecli/help::help-default)

;;;###autoload
(defalias 'ecli-get-options-plist 'ecli/spec::get-options-plist)

;;;###autoload
(defalias 'ecli-put-option-value 'ecli/spec::put-option-value)

;;;###autoload
(defalias 'ecli-get-option-value 'ecli/spec::get-option-value)

;;;###autoload
(defalias 'ecli-push-option-value 'ecli/spec::push-option-value)

;;;###autoload
(defalias 'ecli-help 'ecli/spec::help)

;;;###autoload
(defalias 'ecli-error 'ecli/spec::error)

;;;###autoload
(defalias 'ecli-exit 'ecli/util::exit)

;;;###autoload
(defalias 'ecli-say 'ecli/util::say)

;;;###autoload
(defalias 'ecli-say-error 'ecli/util::say-error)


(provide 'ecli)
;;; ecli.el ends here.

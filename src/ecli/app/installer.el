;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'ecli/app/installer)
(require 'ecli/META)
(require 'ecli/util)
(require 'ecli/core)
(require 'ecli/spec)
(require 'ecli/app/installer/rsc)
;;!end

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

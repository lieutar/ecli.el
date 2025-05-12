;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'ecli/core)
(require 'ecli/META)
(require 'ecli/spec)
(require 'ecli/util)
;;!end

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

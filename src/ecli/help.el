;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'ecli/help)
(require 'ecli/META)
(require 'ecli/util)
;;!end


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

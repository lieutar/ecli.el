;; -*- lexical-binding: t -*-
(require 'ecli/core)
(defvar ecli-test::app-record)

(defun ecli-test::opt--foo:-f (args app-spec)
  "opt-foo"
  (ecli/spec::push-option-value app-spec :foo (cons (car  args)
                                                    (cadr args)))
  (cddr args))

(defun ecli-test::opt--bar:-b (args app-spec)
  "opt-bar"
  (ecli/spec::put-option-value
   app-spec :bar
   (1+ (or (ecli/spec::get-option-value app-spec :bar) 0)))
  (cddr args))

(defun ecli-test::sub-a (args app-spec)
  "sub-a"
  (setq ecli-test::app-record
        (cons (cons 'ecli-test::sub-a args)
              app-spec)))

(defun ecli-test::sub-b (args app-spec)
  "sub-b"
    (setq ecli-test::app-record
        (cons (cons 'ecli-test::sub-b args)
              app-spec)))

(defun ecli-test::default (args app-spec)
  "default"
  (setq ecli-test::app-record
        (cons (cons 'ecli-test::default args)
              app-spec)))

(defun ecli-test::main (&rest args)
  (ecli/core::process-args args
                           :func-prefix 'ecli-test::
                           :help-prefix "ecli-test::main"))

(provide 'ecli-sample-env-0000)

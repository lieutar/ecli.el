;; -*- lexical-binding: t -*-
(require 'lelde)
(lelde-test-setup)
(require 'ecli/spec)
(require 'ecli-sample-env-0000)

(lelde-ert ecli/error
  (let ((spec (ecli/spec '(:func-prefix "ecli-test::"
                                        :help-prefix "--help--")))
        msg-args
        status)
    (with-advice ((message    (fmt &rest args) (push (cons fmt args) msg-args)))
      (setq status (condition-case err
                       (progn (ecli/spec::error spec "foo") 0)
                     (ecli-exit (cdr err))))
      (should (= status 1))
      (should (equal (cadr msg-args) '("foo")))
      )))

;; -*- lexical-binding: t -*-
(require 'ecli/spec)
(require 'ecli/help)
(require 'lelde)
(lelde-test-setup)
(require 'ecli-sample-env-0000)

(lelde-ert ecli/help::help-message
  (let* ((spec (ecli/spec '(:func-prefix ecli-test::)))
         (msg (ecli/help::help-message
               (append spec (list :help-prefix "hoge")))))
    ;;(message "help-message:\n %s" (s-replace-regexp "^" "\t" msg))
    (should (s-match "\\`hoge\n" msg))))

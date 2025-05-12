;; -*- lexical-binding: t -*-
(require 'ecli/core)
(require 'lelde)
(lelde-test-setup)
(require 'ecli-sample-env-0000)

(lelde-ert ecli/core::--prepare-args
  (let ((result (ecli/core::--prepare-args
                 '("@-f" "@--foo" :bar 1 nil "3" shh))))
    (should (equal result
                   '("-f" "--foo" "--bar" "1" "" "3" "shh")))))


(lelde-ert ecli/core
  ;; TODO Define `with-recording-advice' macro on the `with-advice` package.
  ;; Because following form is slightly boring.
  ;;
  ;; Like:
  ;; (with-recording-advice ((func args-record-var &optional result-record-var
  ;;                               &rest advicing) ... ) ... )
  ;;
  ;; When implement it , use (add-to-list record-var vals-to-record t) instead
  ;; of `push'.
  (let* ( ecli-test::app-record
          (init (lambda () (setq ecli-test::app-record nil))) )


    ;; (funcall init)
    ;; (ecli-test::main)
    ;; (let ((subcmd   (caar ecli-test::app-record))
    ;;       (args     (cdar ecli-test::app-record))
    ;;       (app-spec (cdr  ecli-test::app-record)))
    ;;   (should (null args))
    ;;   (should (eq subcmd (plist-get app-spec :default)))
    ;;   (should (eq subcmd 'ecli-test::default))
    ;;   (should (null (ecli/spec::get-options-plist app-spec)))
    ;;   )

    ;; (funcall init)
    ;; (ecli-test::main "a")
    ;; (let ((subcmd   (caar ecli-test::app-record))
    ;;       (args     (cdar ecli-test::app-record))
    ;;       (app-spec (cdr  ecli-test::app-record)))
    ;;   (should (null args))
    ;;   (should (eq subcmd 'ecli-test::sub-a))
    ;;   (should (null (ecli/spec::get-options-plist app-spec)))
    ;;   )

    (funcall init)
    (ecli-test::main "--foo" "1" "-f" "2" "b" "--bar")
    (let ((subcmd   (caar ecli-test::app-record))
          (args     (cdar ecli-test::app-record))
          (app-spec (cdr  ecli-test::app-record)))

      (should (equal args '("--bar")))
      (should (eq subcmd 'ecli-test::sub-b))
      (should (equal (ecli/spec::get-options-plist app-spec)
                     '(:foo (("-f" . "2") ("--foo" . "1")))))
      )

    ))

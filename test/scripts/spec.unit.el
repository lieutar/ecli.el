;; -*- lexical-binding: t -*-
(require 'lelde)
(lelde-test-setup)
(require 'ecli/spec)
(require 'ecli-sample-env-0000)

(lelde-ert ecli/spec
  (let ((spec (ecli/spec '(:func-prefix "ecli-test::"))))

    ;;(message  "spec: %S" spec)
    (should spec)

    (let ((opts     (plist-get spec :opts)))
      ;;(message "opts: %S" opts)
      (should (eq 4 (length opts)))
      (should (assoc "--foo" opts))
      (should (assoc "--bar" opts))
      (should (assoc "-f"    opts))
      (should (assoc "-b"    opts)))

    (let ((subs     (plist-get spec :subs)))
      ;;(message "subs: %S" subs)
       (should (eq 2 (length subs)))
       (should (assoc "a" subs))
       (should (assoc "b" subs)))

    (let ((opts-src (plist-get spec :opts-src)))
      ;;(message "opts-src: %S" opts-src)
      (should (eq 2 (length opts-src)))
      (should (assq 'ecli-test::opt--foo:-f opts-src))
      (should (assq 'ecli-test::opt--bar:-b opts-src)))

    ))

(lelde-ert ecli/spec::props ()
  (let ((spec '(:foo bar)))
    (should-not (ecli/spec::get-option-value spec :foo))
    (ecli/spec::put-option-value spec :foo 1)
    (should (eq (ecli/spec::get-option-value spec :foo) 1))
    (ecli/spec::push-option-value spec :bar nil)
    (should (equal (ecli/spec::get-option-value spec :bar) '(nil)))))

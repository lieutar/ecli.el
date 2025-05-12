;; -*- lexical-binding: t -*-
(require 'lelde)
(lelde-test-setup)
(require 'ecli/app/installer)

(lelde-ert ecli/app/installer::--parse-elpa-spec
  (let ((result (ecli/app/installer::--parse-elpa-spec "melpa-stable")))
    (should (equal (car result) "melpa-stable"))
    (should (equal (cdr result) "https://stable.melpa.org/packages/")))
  (let ((result (ecli/app/installer::--parse-elpa-spec
                 "foo=https://foo.org/packages/")))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "https://foo.org/packages/"))))

(lelde-ert cli/app/installer::--fill-template
  (let ((result (ecli/app/installer::--fill-template
                 "<<a>><<b>><<a>><<b>>" '(("a" . "A")("b"."B")))))
    (should (equal result "ABAB"))))

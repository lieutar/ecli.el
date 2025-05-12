(require 'ecli/util)
(require 'lelde)
(lelde-test-setup)

(lelde-ert ecli/util::flatten
  (let ((result (ecli/util::flatten '(nil (a (b c))))))
    (should (equal result '(nil a b c)))))

(lelde-ert ecli/util::join
  (let ((result (ecli/util::join "-" '("a" "b" "c"))))
    (should (equal result "a-b-c"))))

(lelde-ert ecli/util::string-match
  (let ((result (ecli/util::string-match "\\(a+\\)-\\(b+\\)-\\(c+\\)"
                                         "xxx-aaa-b-ccccc-yyy")))
    (should (equal result '("xxx-aaa-b-ccccc-yyy"
                            "aaa"
                            "b"
                            "ccccc")))))

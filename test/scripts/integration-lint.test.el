(require 'lelde)
(lelde-test-setup)
(require 'check-declare)
(require 'checkdoc)

(ert-deftest ecli-itest/check-declare ()
  "Test integrated files by check-declare"
  (let* ((file (locate-library "ecli.el"))
         (warnings (with-current-buffer (get-buffer-create "*Warnings*")
                     (let ((head (point)))
                       (check-declare-file file)
                       (buffer-substring-no-properties head (point-max))))))
    (should (equal warnings ""))))

(ert-deftest ecli-itest/check-doc ()
  "Test integrated files by checkdoc"
  (let* ((file (locate-library "ecli.el"))
         (warnings (with-current-buffer (get-buffer-create "*Warnings*")
                     (let ((head (point)))
                       (checkdoc-file file)
                       (buffer-substring-no-properties head (point-max))))))
    (should (equal warnings ""))))

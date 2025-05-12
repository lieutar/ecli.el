;; -*- lexical-binding: t -*-
(defvar package-archives)
(defvar package-user-dir)
(let ((here (expand-file-name (file-name-directory
                               (or load-file-name buffer-file-name)))))
  (let ((package-archives `(("test" . ,(expand-file-name "packages" here))))
        (package-user-dir (expand-file-name "elpa" here)))
    (when (file-directory-p package-user-dir)
      (delete-directory package-user-dir t))
    (make-directory package-user-dir t)
    (package-initialize)
    (package-install 'a)))

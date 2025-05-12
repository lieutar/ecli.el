;; -*- lexical-binding: t -*-
(let ((local-package-dir
       (expand-file-name (format "../.cask/%s/elpa" emacs-version)
                         (file-name-directory
                          (or load-file-name buffer-file-name)))))
  (when (file-directory-p local-package-dir)
    (dolist (file (directory-files local-package-dir))
      (when (not (or (equal file ".")
                     (equal file "..")
                     (equal file "archives")))
        (let ((path (expand-file-name file local-package-dir)))
          (when (file-directory-p path)
            (unless (member path load-path)
              (push path load-path))))))))

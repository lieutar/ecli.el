;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'ecli/app/context)
(require 'ecli/META)
;;!end

;;;; ecli/app/context
;;!export
(defmacro ecli/app/context::setup-app-context (&optional elpa-dir)
  "Setup installable package"
  `(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
          (package-user-dir  (expand-file-name "../../cache/elpa" here)))
     (package-initialize)))

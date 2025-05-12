;; -*- lexical-binding: t -*-
(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "scripts/exec.el" here))
  (let ((src (expand-file-name "src" here)))
    (unless (member src load-path)
      (push src load-path)))
  (setq load-path (delete-dups load-path))
  (require 'lelde))

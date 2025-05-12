;; -*- lexical-binding: t -*-

;;!drop-when-bundled
(provide 'ecli/util)
(require 'ecli/META)
;;!end

;;;; ecli/util
;;;;; internal utilities

(defun ecli/util::flatten (list)
  (apply #'append
         (mapcar
          (lambda (elem)
            (if (and elem
                     (listp elem)
                     (listp (cdr elem)))
                (ecli/util::flatten elem)
              (list elem)))
          list)))

(defun ecli/util::join (sep list)
  (mapconcat #'identity list sep))

(defun ecli/util::string-match (pattern string)
  (when (string-match pattern string)
    (let ((n 1)
          (result (list string)))
      (while n
        (let ((group (match-string n string)))
          (if group
              (progn (push group result)
                     (setq n (1+ n)))
            (setq n nil))))
      (reverse result))))

;;;; utilities for users

(define-error 'ecli-exit "Exit signal for `ecli'")

;;!export
(defun ecli/util::exit (&optional status)
  "Terminate your app's process.
ecli-process-args will returns the STATUS code.
If STATUS was omitted, it is recognized as 0."
  (signal 'ecli-exit (or status 0)))

;;!export
(defun ecli/util::say (string &rest objects)
  "Print string to standard output on tty.
"
  (princ (apply 'format (concat string "\n") objects)))

;;!export
(defun ecli/util::say-error (string &rest objects)
  ""
  (apply 'message string objects))

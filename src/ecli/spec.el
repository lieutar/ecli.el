;; -*- lexical-binding: t -*-

;;!drop-when-bundled
(provide 'ecli/spec)
(require 'ecli/META)
(require 'ecli/error)
(require 'ecli/help)
(require 'ecli/util)
;;!end


;;;; ecli/spec

;;;;; constructor for ecli/spec
(defun ecli/spec::--read-spec-from-obarray (prefix)
  ;; Generate spec by PREFIX.
  ;; For example:
  ;; where PREFIX := foo-
  ;; the function foo-sub-subcmd processes as a sub command
  ;; the function foo-opt--option-o processes options
  (let ((pattern-sub (format "\\`%ssub-\\(.+\\)\\'"            prefix))
        (pattern-opt (format "\\`%sopt--\\(.+?\\):-\\(.\\)\\'" prefix))
        (opts-src nil)
        (opts     nil)
        (subs     nil))
    (mapatoms ;; scan all symbols on current obarray
     (lambda (sym)
       (when (fboundp sym)
         (let* ((name (symbol-name sym))
                (match-opt (ecli/util::string-match pattern-opt name))
                (match-sub (ecli/util::string-match pattern-sub name)))
           (when match-opt
             (let ((long  (format "--%s" (nth 1 match-opt)))
                   (short (format "-%s"  (nth 2 match-opt))))
               (push (list sym long short) opts-src)
               (push (cons long  sym) opts)
               (push (cons short sym) opts)))
           (when match-sub
             (push (cons (nth 1 match-sub) sym) subs))))))
    (list :subs subs :opts opts :opts-src opts-src)))

(defun ecli/spec::--make-default (spec prefix)
  (let ((deffun (intern (format "%sdefault" prefix)))
        (help      (plist-get spec :help))
        (error-fun (plist-get spec :error)))
    (cond ((fboundp deffun) deffun)
          ((null (plist-get spec :subs))
           (lambda (args app-spec)
             (if (null args)
                 (progn (funcall help spec)
                        (ecli/util::exit 0))
               (funcall error (format "Error: Unprocessed args: %S\n" args)))))
          (t
           (lambda (args app-spec)
             (funcall error (format "Error: Unknown sub command: %s\n" (car args))))))))

(defun ecli/spec (options)
  (let* ((prefix (plist-get options :func-prefix))
         (spec   (append options
                         (ecli/spec::--read-spec-from-obarray prefix))))
    (unless (plist-get spec :help)
      (plist-put spec :help #'ecli/help::help-default))
    (unless (plist-get spec :error)
      (plist-put spec :error #'ecli/error::error-default))
    (unless (plist-get spec :default)
      (plist-put spec :default (ecli/spec::--make-default spec prefix)))
    spec))

;;;;; methods for ecli/spec

;;!export
(defun ecli/spec::get-options-plist (spec)
  (plist-get spec :options))

;;!export
(defun ecli/spec::put-option-value (spec prop value)
  ""
  (let ((plist (plist-get spec :options)))
    (if plist
        (plist-put plist prop value)
      (plist-put spec :options (list prop value)))))

;;!export
(defun ecli/spec::get-option-value (spec prop)
  ""
  (plist-get (plist-get spec :options) prop))

;;!export
(defun ecli/spec::push-option-value (spec prop value)
  ""
  (ecli/spec::put-option-value
   spec prop
   (cons value (ecli/spec::get-option-value spec prop))))

;;!export
(defun ecli/spec::help (spec)
  ""
  (funcall (plist-get spec :help) spec))

;;!export
(defun ecli/spec::error (spec message &rest opt)
  ""
  (apply (plist-get spec :error) spec message opt))

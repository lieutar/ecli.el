;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'ecli/error)
(require 'ecli/META)
(require 'ecli/help)
(require 'ecli/util)
;;!end

;;!export
(defun ecli/error::error-with-help ( spec string &rest objects )
  "Show error messages with the help message.
SPEC is an app spec `ecli' generated.
STRING and OBJECTS are format string having place holders and its values.
This error function shows message and help message.
If help isn't required, you can use `ecli-error-simple'"
  (ecli/util::say-error (format string objects))
  (ecli/util::say-error (ecli/help::help-message spec))
  (ecli/util::exit 1))

;;!export
(defun ecli/error::error-default ( spec string &rest objects )
  "Default error function for `ecli' apps.
SPEC is an app spec `ecli' generated. ( however this function doesn't use it )
STRING and OBJECTS are format string having place holders and its values.
This function shows only given message.
"
  (ecli/util::say-error (format string objects))
  (ecli/util::exit 1))

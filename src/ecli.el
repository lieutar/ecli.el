;;; {{index}}.el --- {{brief}} -*-lexical-binding: t -*-

;; Copyright (C) {{copyright}}

;; Author: {{author}}
;; Version: {{version}}
;; Keywords: {{(keywords) (s-join ", " keywords)}}
;; URL: {{url}}
;; Package-Requires: {{(dependency)(lelde/tinplate/util::index-pr dependency)}}

;;; License:

;; {{(license) (s-replace "\n" "\n;; " license)}}

;;; Commentary:
;;
;; {{(commentary) (s-replace "[[:space:]]+\\(\n\\|\\'\\)" "\\1" (s-replace "\n" "
;; " (s-replace "\\`[[:space:]\n]+" "" commentary)))}}
;;
;;  About deteils of this, see: README.md
;;

;;; Code:


;;; Code:

;;!drop-when-bundled
(require 'ecli/META)
(require 'ecli/core)
(require 'ecli/app/installer)
(require 'ecli/app/context)
;;!end
;;!insert-bundled
;;;###autoload
(defalias 'ecli-setup-app-context 'ecli/app/context::setup-app-context)

;;;###autoload
(defalias 'ecli-make-installer 'ecli/app/installer::make-installer)

;;;###autoload
(defalias 'ecli-process-args 'ecli/core::process-args)

;;;###autoload
(defalias 'ecli-error-with-help 'ecli/error::error-with-help)

;;;###autoload
(defalias 'ecli-error-default 'ecli/error::error-default)

;;;###autoload
(defalias 'ecli-help-message 'ecli/help::help-message)

;;;###autoload
(defalias 'ecli-help-default 'ecli/help::help-default)

;;;###autoload
(defalias 'ecli-get-options-plist 'ecli/spec::get-options-plist)

;;;###autoload
(defalias 'ecli-put-option-value 'ecli/spec::put-option-value)

;;;###autoload
(defalias 'ecli-get-option-value 'ecli/spec::get-option-value)

;;;###autoload
(defalias 'ecli-push-option-value 'ecli/spec::push-option-value)

;;;###autoload
(defalias 'ecli-help 'ecli/spec::help)

;;;###autoload
(defalias 'ecli-error 'ecli/spec::error)

;;;###autoload
(defalias 'ecli-exit 'ecli/util::exit)

;;;###autoload
(defalias 'ecli-say 'ecli/util::say)

;;;###autoload
(defalias 'ecli-say-error 'ecli/util::say-error)


(provide 'ecli)
;;; ecli.el ends here.

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
;;!static-macro
(lelde-emit-for-index)

(provide 'ecli)
;;; ecli.el ends here.

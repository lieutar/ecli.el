;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'ecli/app/installer/rsc)
(require 'ecli/META)
;;!end

;;!static-macro
(replace-regexp-in-string
 "\\\\n" "\n"
 (ppp-sexp-to-string
  `(defconst ecli/app/installer/rsc::$installer.sh-src
     ,(f-read (f-expand "../../../../installer-template.sh"
                        (f-dirname stmax-current-processing-file))))))

;;!static-macro
(replace-regexp-in-string
 "\\\\n" "\n"
 (ppp-sexp-to-string
  `(defconst ecli/app/installer/rsc::$installer.el-src
     ,(f-read (f-expand "../../../../installer-template.el"
                        (f-dirname stmax-current-processing-file))))))

(defconst ecli/app/installer/rsc::$predefined-archives-alist
  '(("gnu"            . "https://elpa.gnu.org/packages/")
    ("nongnu"         . "https://elpa.nongnu.org/nongnu/")
    ("melpa"          . "https://melpa.org/packages/")
    ("melpa-stable"   . "https://stable.melpa.org/packages/")
    ("melpa-unstable" . "https://unstable.melpa.org/packages/")
    ("org"            . "https://orgmode.org/elpa/")
    ("looper"         . "https://lieutar.github.io/looper-elpa/")))

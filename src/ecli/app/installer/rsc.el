;; -*- lexical-binding: t -*-
;;!drop-when-bundled
(provide 'ecli/app/installer/rsc)
(require 'ecli/META)
;;!end

(defconst ecli/app/installer/rsc::$installer.sh-src "#! /usr/bin/env bash

###
### Setup Installer's context
###

here=`dirname $BASH_SOURCE`
install_el=$(mktemp)

cleanup(){
    [ -f $install_el ] && rm $install_el
}

trap cleanup int
trap cleanup EXIT

###
### Location to be installed
###

# Path to be installed
prefix=<<default-prefix>>

# Path to the definition to local settings
local_config=
# About local settings
# See README.md on https://github.com/lieutar/ecli.el/
# The section \"Installer\" will express it.

###
### Process arguments and interaction
###

while [ $# -gt 0 ]; do
    arg=$1
    shift
    case \"$arg\" in
         # Path to be installed ( Default <<default-prefix>> )
        --prefix|-P)
            prefix=$1
            case \"$prefix\"; in
                \"\"|-*)
                    echo \"--prefix / -P requires an argument\"
                    while [ -z \"$prefix\" ]; do
                          printf \"Where? \"
                          read prefix
                    done
                    ;;
                *)
                    shift
                    ;;
            esac
            ;;

        # Setting to local elpa mirror and pre-downloaded scripts
        --local-config|-C)
            local_config=$1
            case \"$local_config\" in
                \"\" |-*)
                    echo \"--local-config / -C requires an argument\"
                    while [ -z \"$local_config\" ]; do
                        printf \"Where? \"
                        read local_config
                    done
                    while [ -n \"$local_config\" ] &&\\
                                ! [ -r \"$local_config\" ]; do
                        echo \"Can't read $local_config\"
                        printf \"Where? \"
                        echo \"(ignore local-config specification if blank)\"
                        printf \"> \"
                        read local_config
                    ;;
                *)
                    shift
                    ;;
            esac
            ;;

        # Process illegal arguments
        *)
            echo \"Warning: unrecognized argument: $arg\"
            printf \"Continue? (yes/no , default=no):\"
            read answer
            case \"$answer\" in
                yes|Yes|YES)
                    echo \"Continue ...\"
                    ;;
                *)
                    echo \"Abort\"
                    exit 1
                    ;;
            esac
        ;;
    esac
done

###
### Make Emacs Lisp installer
###

cat <<EOF >$install_el
<<logic-to-install>>
EOF

###
### Run the Emacs Lisp installer
###

# pass configurations by environment
export PREFIX=$prefix
export LOCAL_CONFIG_ELD=$local_config

if ! emacs --batch -Q -l $install_el -f eclinstall::main ; then
    echo \"Installation failed.\" >&2
    exit 1
fi
")


(defconst ecli/app/installer/rsc::$installer.el-src ";; -*- lexical-binding: t -*-
(require 'package)
(require 'url)

(defconst eclinstall::$ecli-url
  \"https://raw.githubusercontent.com/lieutar/ecli.el/refs/heads/main/ecli.el\")

(defun eclinstall::get-local-config (eld)
  (when (and eld (file-exists-p eld))
    (read (with-temp-buffer
            (insert-file-contents eld)
            (buffer-substring-no-properties
             (point-min) (point-max))))))

(defun eclinstall::mkdirs (&rest dirs)
  (dolist (dir dirs)
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun eclinstall::modify-archives (package-archives
                                    local-config)
  ;; Update package-archives by local-config
  (let ((local-archives-alist (plist-get local-config :archives)))
    (dolist (lslot local-archives-alist)
      (let ((pslot (assoc (car lslot) package-archives)))
        (when pslot (setcdr pslot (cdr lslot)))))
    package-archives))

(defvar package-archives)
(defvar package-user-dir)
(defun eclinstall::install-deps (elpa-dir local-config archives depends-on)
  ;; Installation
  ;; Install dependencies using package.el
  (let ((package-archives
         (eclinstall::modify-archives archives local-config))
        (package-user-dir elpa-dir))
    (package-initialize)
    (dolist (dep depends-on)
      (package-install dep))))

(defun eclinstall::url-retrieve (url)
  (let ((response-buffer (url-retrieve-synchronously url t t nil))
        body)
    (unless response-buffer (error \"url-retrieve-synchronously returns nil\"))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      (when (string-match \"\\\\`https?://\" url)
        (let ((status-line
               (progn
                 (beginning-of-line 2)
                 (buffer-substring-no-properties (point-min)(point)))))
          (unless (string-match \"200 OK$\" status-line)
            (error \"url-retrieve %s failed: %s\" url status-line))))
      (search-forward \"\
\
\")
      (setq body (buffer-substring-no-properties (point)(point-max))))
    (kill-buffer response-buffer)
    body))

(defun eclinstall::install-from-url (url into)
  (unless (string-match \"\\\\`[a-z]+://\" url)
    (setq url (format \"file://%s\" (expand-file-name url))))
  (let ((dst (expand-file-name
                (and (string-match \"\\\\([^/]+\\\\)\\\\'\" url) (match-string 1 url))
                into)))
    (with-temp-file dst (insert (eclinstall::url-retrieve url)))
    dst))

(defun eclinstall::install-scripts
    (bin-dir url-launcher lisp-dir url-app.el local-config)
  ;; Install main scripts
  (let ((url-to-local (or (plist-get local-config :url-to-local) identity)))
    (chmod (eclinstall::install-from-url
            (funcall url-to-local url-launcher) bin-dir)
           #o755)
    (let ((load-path (cons lisp-dir load-path)))
      (byte-compile-file
       (eclinstall::install-from-url
        (funcall url-to-local eclinstall::$ecli-url) lisp-dir))
      (byte-compile-file
       (eclinstall::install-from-url
        (funcall url-to-local url-app.el)            lisp-dir)))))

(defun eclinstall::main ()
  (let ((url-launcher \"<<url-launcher>>\")
        (url-app.el   \"<<url-app.el>>\")
        (prefix     (getenv \"PREFIX\"))
        (config.eld (getenv \"LOCAL_CONFIG_ELD\")))

    (let ((bin-dir  (format \"%s/bin\"        prefix))
          (lisp-dir (format \"%s/share/lisp\" prefix))
          (elpa-dir (format \"%s/cache/elpa\" prefix))
          (local-config (eclinstall::get-local-config config.eld)))

      ;; Make directory if not being.
      (eclinstall::mkdirs  bin-dir lisp-dir elpa-dir)
      (eclinstall::install-deps
       elpa-dir
       local-config
       '<<archives>>
       '<<depends-on>>)
      (eclinstall::install-scripts bin-dir  url-launcher
                                   lisp-dir url-app.el
                                   local-config))))
")


(defconst ecli/app/installer/rsc::$predefined-archives-alist
  '(("gnu"            . "https://elpa.gnu.org/packages/")
    ("nongnu"         . "https://elpa.nongnu.org/nongnu/")
    ("melpa"          . "https://melpa.org/packages/")
    ("melpa-stable"   . "https://stable.melpa.org/packages/")
    ("melpa-unstable" . "https://unstable.melpa.org/packages/")
    ("org"            . "https://orgmode.org/elpa/")
    ("looper"         . "https://lieutar.github.io/looper-elpa/")))

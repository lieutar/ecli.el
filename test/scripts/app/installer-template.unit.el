;; -*- lexical-binding: t -*-
(require 'lelde)
(lelde-test-setup)
(require 'slash-tmp)

(let* ((test-spec (lelde-test-spec))
       (tmpl-el   (f-expand "installer-template.el"
                            (plist-get test-spec :project-path))))
  (load tmpl-el))

(lelde-ert eclinstall::get-local-config
  (/tmp/let (config.eld)
    (/tmp/weird-magic-spell)
    (with-temp-file config.eld (insert "(a b c)"))
    (should (equal (eclinstall::get-local-config config.eld)
                   '(a b c)))))

(lelde-ert eclinstall::mkdirs
  (/tmp/with-temp-dir
    (/tmp/weird-magic-spell)
    (eclinstall::mkdirs "a" "b/c" "d/e/f")
    (should (file-directory-p "a"))
    (should (file-directory-p "b/c"))
    (should (file-directory-p "d/e/f"))))

(lelde-ert eclinstall::modify-archives
  (let ((orig '(("foo" . "bar")
                ("hoge" . "piyo")))
        (lc   '(:archives (("hoge" . "fuga")))))
    (should (equal (eclinstall::modify-archives orig lc)
                   '(("foo" . "bar")("hoge" . "fuga"))))))

(setq minelpa/ (lelde-test-call rsc-expand "minelpa/packages"))
(lelde-ert eclinstall::install-deps
  (/tmp/let (elpa/)
    (/tmp/weird-magic-spell)
    (eclinstall::install-deps
     elpa/ nil `(("minelpa" . ,minelpa/)) '(a))
    (should (file-exists-p (expand-file-name "a-20250511.1234/a.el" elpa/)))
    (should (file-exists-p (expand-file-name "b-20250511.1234/b.el" elpa/)))
    (should (file-exists-p (expand-file-name "c-20250511.1234/c.el" elpa/)))
    (should (file-exists-p (expand-file-name "d-20250511.1234/d.el" elpa/)))
    ))

(lelde-ert eclinstall::install-from-url
  (/tmp/let (orig
             into/)
    (/tmp/weird-magic-spell)
    (with-temp-file orig (insert "bar!!"))
    (let ((url (format "file://%s" orig)))
      (eclinstall::install-from-url url into/ #o700)
      (let* ((bn (file-name-nondirectory orig))
             (dst (expand-file-name bn into/)))
        (when (should (file-exists-p dst))
          (should (equal (f-read dst) "bar!!"))
          (let ((stat (file-attributes dst)))
            (should (equal (file-attribute-modes stat)
                           "-rwx------"))))))))

(lelde-ert eclinstall::install-from-lolcal
  (/tmp/let (orig
             into/)
    (/tmp/weird-magic-spell)
    (with-temp-file orig (insert "bar!!"))
    (eclinstall::install-from-local "https://exapmle.org"
                                    (lambda (src) orig)
                                    into/ #o700)
    (let* ((bn (file-name-nondirectory orig))
           (dst (expand-file-name bn into/)))
      (when (should (file-exists-p dst))
        (should (equal (f-read dst) "bar!!"))
        (let ((stat (file-attributes dst)))
          (should (equal (file-attribute-modes stat)
                         "-rwx------")))))))

(lelde-ert eclinstall::install-scripts
  (/tmp/let (bin/
             lisp/
             launcher
             app.el)
    (/tmp/weird-magic-spell)
    (with-temp-file launcher (insert "content of launcher"))
    (with-temp-file app.el   (insert "content of app.el"))
    (eclinstall::install-scripts
     bin/  (format "http://example.org%s" launcher)
     lisp/ (format "http://example.org%s" app.el)
     '(:url-to-local (lambda (url) (replace-regexp-in-string
                                    "\\`http://example\\.org" "" url))))
    (let ((dst-launcher
           (expand-file-name (file-name-nondirectory launcher) bin/))
          (dst-app.el
           (expand-file-name (file-name-nondirectory app.el) lisp/)))
      (when (should (file-exists-p dst-launcher))
        (should (equal (f-read dst-launcher) "content of launcher")))
      (when (should (file-exists-p dst-app.el))
        (should (equal (f-read dst-app.el) "content of app.el"))))))

(lelde-ert eclinstall::main
  (let ((prefix "~/apps/hoge")
        (local-config.eld "~/local-config.eld")
        (called nil))
    (setenv "PREFIX" prefix)
    (setenv "LOCAL_CONFIG_ELD" local-config.eld)
    (let ((bin-dir  "~/apps/hoge/bin")
          (lisp-dir "~/apps/hoge/share/lisp")
          (elpa-dir "~/apps/hoge/cache/elpa"))
    (with-advice
      ((eclinstall::get-local-config
        (cfg)
        (push 'get-local-config called)
        (should (equal cfg local-config.eld))
        'local-config-result)

       (eclinstall::mkdirs
        (&rest dirs)
        (push 'mkdirs called)
        (should (equal dirs (list bin-dir lisp-dir elpa-dir))))

       (eclinstall::install-deps
        (a-elpa-dir a-local-config a-archives a-depends-on)
        (push 'install-deps called)
        (should (equal a-elpa-dir elpa-dir))
        (should (eq a-local-config 'local-config-result))
        (should (equal a-archives '<<archives>>))
        (should (equal a-depends-on '<<depends-on>>)))

       (eclinstall::install-scripts
        (a-bin-dir a-url-launcher a-lisp-dir a-url-app.el a-local-config)
        (push 'install-scripts called)
        (should (equal a-bin-dir bin-dir))
        (should (equal a-url-launcher "<<url-launcher>>"))
        (should (equal a-lisp-dir lisp-dir))
        (should (equal a-url-app.el "<<url-app.el>>"))
        (should (equal a-local-config 'local-config-result))))

      (eclinstall::main)
      (should (memq 'get-local-config called))
      (should (memq 'mkdirs called))
      (should (memq 'install-deps called))
      (should (memq 'install-scripts called))))))

(let ((current-directory
       (file-name-directory
        (if load-file-name load-file-name buffer-file-name))))
  (setq pt-test/test-path (expand-file-name "." current-directory))
  (setq pt-test/root-path (expand-file-name "../lib" current-directory)))

(defvar cask-initialize-run nil)
(defvar cask-source-mapping
  '((melpa . "http://melpa.milkbox.net/packages/")))
(defun cask-initialize ()
  (setq cask-initialize-run t))
(provide 'cask)

(add-to-list 'load-path pt-test/root-path)
(if (featurep 'pallet)
    (unload-feature 'pallet t))
(require 'pallet)
(require 'cl)

(defun mock-package-alist ()
  '((wgrep-ack .
               [(20121201 2230)
                ((wgrep
                  (2 1 1)))
                "Writable ack-and-a-half buffer and apply the changes to files [source: github]"])
    (yaml-mode .
               [(20120901 1329)
                nil "Major mode for editing YAML files [source: github]"])
    (yasnippet .
               [(20130218 2229)
                nil "Yet another snippet extension for Emacs. [source: github]"])))

(defun mock-upgrade-alist ()
  (append '((yasnippet .
               [(20130123 2111)
                nil "Yet another snippet extension for Emacs. [source: github]"]))
          (mock-package-alist)))

(defun mock-archive-alist ()
  '((melpa . "http://melpa.milkbox.net/packages/")
    (unknown . "http://example.com")))

(defun mock-caskfile () nil
  (concat "(source \"unknown\" \"http://example.com\")\n"
          "(source melpa)\n\n"
          "(depends-on \"wgrep-ack\")\n(depends-on \"yaml-mode\")\n(depends-on \"yasnippet\")"))

(defun mock-package-list ()
  '("wgrep-ack" "yaml-mode" "yasnippet"))

(defun mock-cask-dependencies ()
  '([cl-struct-cask-dependency yasnippet nil]
    [cl-struct-cask-dependency yaml-mode nil]
    [cl-struct-cask-dependency wgrep-ack nil]))

(ert-deftest pt-test/installed-p ()
  "it should accept any valid package designator"
  (let ((package-alist (mock-package-alist)))
    (should (equal t (pt/installed-p :yasnippet)))
    (should (equal t (pt/installed-p 'yasnippet)))
    (should (equal t (pt/installed-p "yasnippet")))

    (should (equal nil (pt/installed-p :foo)))
    (should (equal nil (pt/installed-p 'foo)))
    (should (equal nil (pt/installed-p "foo")))

    (should-error (pt/installed-p 42))))

(ert-deftest pt-test/pallet-pick-packages ()
  "it should get a list of package name strings from package-alist"
  (let ((package-alist (mock-package-alist)))
    (should (equal (pt/pallet-pick-packages) (mock-package-list)))))

(ert-deftest pt-test/pallet-pack ()
  "it should construct a valid caskfile from pt/pallet-pick-packages"
  (let ((package-archives (mock-archive-alist))
        (package-alist (mock-package-alist)))
    (should (equal
             (pt/pallet-pack package-archives (pt/pallet-pick-packages)) (mock-caskfile)))))

(ert-deftest pt-test/pallet-repack ()
  "it should write a Caskfile to the user's emacs directory based on package-alist"
  (let ((package-archives (mock-archive-alist))
        (package-alist (mock-package-alist))
        (file-path "")
        (file-contents ""))
    (flet ((pt/write-file (file contents)
                          (setq file-path file)
                          (setq file-contents contents)))
      (pallet-repack)
      (should (equal file-path (expand-file-name "Cask" user-emacs-directory)))
      (should (equal file-contents (mock-caskfile))))))

(ert-deftest pt-test/repack-on-close ()
  "it should run pt/pallet-repack on close."
  (let ((repacked nil) (pallet-repack-on-close t))
    (flet ((pallet-repack ()
                           (setq repacked t)))
      (run-hooks 'kill-emacs-hook)
      (should (equal repacked t)))))

(ert-deftest pt-test/cask-up-on-load ()
  "it should run pt/cask-up on load."
  (run-hooks 'after-init-hook)
  (should (equal cask-initialize-run t)))

(ert-deftest pt-test/pack-on-install ()
  "it should pack a package when installed."
  (let ((packed nil) (pallet-pack-on-install t))
    (flet ((pt/pallet-pack-one (package)
                               (setq packed package))
           (package-install (name)))
      (package-install "test-package")
      (should (equal packed "test-package")))))

(ert-deftest pt-test/unpack-on-delete ()
  "it should unpack a package on delete."
  (let ((unpacked nil) (pallet-unpack-on-delete t))
    (flet ((pt/pallet-unpack-one (package)
                                 (setq unpacked package))
           (package-delete (name version)))
      (package-delete "test-package" "012")
      (should (equal unpacked "test-package")))))

(ert-deftest pt-test/suspend-delete-on-update ()
  "it should suspend deletes on update."
  (let ((suspended nil))
    (flet ((pt/suspend-delete (body) (setq suspended t))
           (cask-update nil))
      (pallet-update)
      (should (equal suspended t)))))

(ert-deftest pt-test/no-delete-on-upgrade ()
  "it should suspend deletes on update."
  (let ((package-alist (mock-upgrade-alist))
        (caskfile (mock-caskfile))
        (cask-runtime-dependencies (mock-cask-dependencies))
        (package-archives (mock-archive-alist))
        (file-contents ""))
    (flet ((package-delete (name version)
                           (setq package-alist
                                 (if (and (string= name "yasnippet")
                                          (string= version "20130123.2111"))
                                     (mock-package-alist)
                                   package-alist)))
           (pt/write-file (file contents)
                          (setq file-contents contents))
           (pt/cask-up (body) (funcall body)))
     (package-delete "yasnippet" "20130123.2111")
     (pallet-repack)
     (should (string-match "yasnippet" file-contents)))))

(ert-deftest pt-test/pack-one ()
  "it should add a package definition to the Cask file."
  (let ((caskfile (mock-caskfile))
        (cask-runtime-dependencies (mock-cask-dependencies))
        (package-archives (mock-archive-alist))
        (file-contents "")
        (cask-add-dependency-called nil))
    (flet ((package-install (package))
           (cask-add-dependency (arg)
                       (setq cask-add-dependency-called t))
           (pt/write-file (file contents)
                          (setq file-contents contents))
           (pt/cask-up (body) (funcall body)))
      (package-install "test-package")
      (should (eq cask-add-dependency-called t)))))

(ert-deftest pt-test/unpack-one ()
  "it should remove a package definition from the Cask file."
  (let ((caskfile (mock-caskfile))
        (cask-runtime-dependencies (mock-cask-dependencies))
        (package-archives (mock-archive-alist))
        (file-contents ""))
    (flet ((package-delete (package version))
           (pt/write-file (file contents)
                          (setq file-contents contents))
           (pt/cask-up (body) (funcall body)))
      (package-delete "yasnippet" nil)
      (should (string-match "yaml-mode" file-contents))
      (should-not (string-match "yasnippet" file-contents)))))

(ert-deftest pt-test/pallet-pick-cask ()
  "it should get a list of package names from a Cask file."
  (let ((cask-runtime-dependencies (mock-cask-dependencies)))
    (flet ((pt/cask-up))
      (should (equal (pt/pallet-pick-cask) (mock-package-list))))))

(ert-deftest pt/pallet-pick-cask-except ()
  "it should exclude a package from a package list."
  (let ((cask-runtime-dependencies (mock-cask-dependencies))
        (package-list (cdr (mock-package-list))))
    (flet ((pt/cask-up))
      (should (equal (pt/pallet-pick-cask-except 'wgrep-ack) package-list)))))

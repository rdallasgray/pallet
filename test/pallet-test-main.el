(let ((current-directory (file-name-directory (if load-file-name load-file-name buffer-file-name))))
  (setq pt-test/test-path (expand-file-name "." current-directory))
  (setq pt-test/root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path pt-test/root-path)
(unload-feature 'pallet t)
(require 'pallet)
(require 'cl)

(defun mock-package-alist ()
  '((yasnippet .
                      [(20130218 2229)
                       nil "Yet another snippet extension for Emacs. [source: github]"])
           (yaml-mode .
                      [(20120901 1329)
                       nil "Major mode for editing YAML files [source: github]"])
           (wgrep-ack .
                      [(20121201 2230)
                       ((wgrep
                         (2 1 1)))
                       "Writable ack-and-a-half buffer and apply the changes to files [source: github]"])))

(defun mock-archive-alist ()
  '(("melpa" . "http://melpa.milkbox.net/packages/")))

(defun mock-cartonfile () nil
  (concat "(source \"melpa\" \"http://melpa.milkbox.net/packages/\")\n\n"
  "(depends-on \"wgrep-ack\")\n(depends-on \"yaml-mode\")\n(depends-on \"yasnippet\")"))

(ert-deftest pt-test/pt/pallet-pick ()
  "it should get a list of package name strings from package-alist"
  (let ((package-alist (mock-package-alist)))
    (should (equal (pt/pallet-pick) '("yasnippet" "yaml-mode" "wgrep-ack")))))

(ert-deftest pt-test/pt/pallet-pack ()
  "it should construct a valid cartonfile"
  (let ((package-archives (mock-archive-alist)) (package-alist (mock-package-alist)))
    (message "pack: %s" (pt/pallet-pack))
    (message "carton: %s" (mock-cartonfile))
    (should (equal (pt/pallet-pack) (mock-cartonfile)))))

(ert-deftest pt-test/pt/pallet-ship ()
  "it should write a Cartonfile to the user's emacs directory"
  (let ((package-archives (mock-archive-alist))
        (package-alist (mock-package-alist))
        (file-path "")
        (file-contents ""))
    (flet ((pt/write-file (file contents)
                          (setq file-path file)
                          (setq file-contents contents)))
      (pt/pallet-ship)
      (should (equal file-path (expand-file-name "Carton" user-emacs-directory)))
      (should (equal file-contents (mock-cartonfile))))))

(ert-deftest pt-test/repack-on-close ()
  "it should run pt/pallet-ship on close."
  (let ((shipped nil))
    (flet ((pt/pallet-ship ()
                          (setq shipped t)))
      (run-hooks 'kill-emacs-hook)
      (should (equal shipped t)))))

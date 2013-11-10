(let ((current-directory
       (file-name-directory
        (if load-file-name load-file-name buffer-file-name))))
  (setq pt-test/test-path (expand-file-name "." current-directory))
  (setq pt-test/root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path pt-test/root-path)
(if (featurep 'pallet)
    (unload-feature 'pallet t))
(require 'pallet)

(defvar cask-initialize-run nil)
(defvar cask-source-mapping
  '((melpa . "http://melpa.milkbox.net/packages/")))
(defun cask-initialize ()
  (setq cask-initialize-run t))
(defun epl-package-installed-p (package-name) t)
(provide 'cask)

(require 'cl)
(require 'el-mock)

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

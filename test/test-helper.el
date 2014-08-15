(let ((current-directory (file-name-directory load-file-name)))
  (setq pallet-test-test-path (f-expand "." current-directory)
        pallet-test-root-path (f-expand ".." current-directory)
        pallet-test-sandbox-path (f-expand "sandbox" pallet-test-test-path)))

(add-to-list 'load-path pallet-test-root-path)

(require 'package)

(defvar pallet-test-servant-url "http://127.0.0.1:9191/packages/")

(setq package-archives
      `(("servant" . ,pallet-test-servant-url)))
(package-initialize)
(package-refresh-contents)

(ignore-errors (make-directory pallet-test-sandbox-path))

(if (featurep 'pallet)
    (unload-feature 'pallet t))

(require 'pallet)
(require 'el-mock)
(require 'cl)
(require 'package)

(defun pallet-test-cask-file ()
  "Path to the sandboxed Cask file"
  (f-expand "Cask" pallet-test-sandbox-path))

(defun pallet-test-create-cask-file (text)
  "Create a Cask file in the sandbox containing `text'"
  (f-write text 'utf-8 (pallet-test-cask-file)))

(defun pallet-test-create-cask-file-with-servant (text)
  (pallet-test-create-cask-file
   (format "(source \"servant\" \"%s\")%s" pallet-test-servant-url text)))

(defun pallet-test-do-package-delete (name &optional version)
  "Run package delete in 24.3.1 or >= 24.3.5 environments."
    (if (fboundp 'package-desc-create)
        (package-delete (package-desc-create :name name :version version))
      (package-delete name version)))

(defmacro pallet-test-with-sandbox (&rest body)
  "Clean the sandbox, run body."
  `(let ((default-directory ,pallet-test-sandbox-path)
         (user-emacs-directory ,pallet-test-sandbox-path)
         (package-user-dir ,(f-expand "elpa" pallet-test-sandbox-path)))
     (progn
       (pallet-test-cleanup-sandbox)
       ,@body)))

(defun pallet-test-cleanup-sandbox ()
  "Clean the sandbox."
  (f-entries pallet-test-sandbox-path
             (lambda (entry)
               (f-delete entry t))))

(let ((current-directory (file-name-directory load-file-name)))
  (setq pallet-test-test-path (f-expand "." current-directory)
        pallet-test-root-path (f-expand ".." current-directory)
        pallet-test-sandbox-path (f-expand "sandbox" pallet-test-test-path)
        pallet-test-package-path (f-expand "servant/packages" pallet-test-test-path)
        pallet-test-servant-url "http://127.0.0.1:9191/packages/"
        package-archives `(("servant" . ,pallet-test-servant-url))))

(add-to-list 'load-path pallet-test-root-path)

(require 'package)

(defvar pallet-test-packages '((package-one (0 0 1))
                               (package-two (0 0 1))
                               (package-two (0 0 2))))

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

(defun pallet-test-create-cask-file-with-servant (&optional text)
  (pallet-test-create-cask-file
   (format "(source \"servant\" \"%s\")%s" pallet-test-servant-url text)))

(defun pallet-test-do-package-delete (name &optional version)
  "Run package delete in 24.3.1 or >= 24.3.5 environments."
  (if (fboundp 'package-desc-create)
      (package-delete (package-desc-create
                       :name name
                       :version version
                       :summary ""
                       :dir (f-expand
                             (pallet-test-versioned-name name version)
                             package-user-dir)))
    (package-delete name version)))

(defun pallet-test-versioned-name (name version)
  "Return a versioned file name as string from string `name' and list `version'"
  (s-concat (symbol-name name) "-" (pallet-test-version-string version)))

(defun pallet-test-version-string (version)
  "Return a string from a version list"
  (s-join "." (mapcar 'number-to-string version)))

(defun pallet-test-package-file (package)
  "Return the file path for PACKAGE"
  (let* ((pkg-versioned-name (pallet-test-versioned-name (car package) (cadr package)))
         (pkg-file-name (format "%s.el" pkg-versioned-name)))
         (f-expand pkg-file-name pallet-test-package-path)))

(defmacro pallet-test-with-sandbox (&rest body)
  "Run BODY in a clean environment"
  `(let ((default-directory ,pallet-test-sandbox-path)
         (user-emacs-directory ,pallet-test-sandbox-path)
         (package-user-dir ,(f-expand "elpa" pallet-test-sandbox-path)))
     (progn
       (pallet-mode -1)
       (pallet-test-cleanup-packages)
       (pallet-test-cleanup-sandbox)
       ,@body)))

(defun pallet-test-cleanup-sandbox ()
  "Clean the sandbox."
  (f-entries pallet-test-sandbox-path
             (lambda (entry)
               (f-delete entry t))))

(defun pallet-test-cleanup-packages ()
  "Uninstall any installed test packages"
  (package-initialize)
  (mapcar (lambda (package)
            (when (package-installed-p (car package) (cadr package))
              (ignore-errors
                (pallet-test-do-package-delete (car package) (cadr package)))))
          pallet-test-packages)
  (package-initialize))

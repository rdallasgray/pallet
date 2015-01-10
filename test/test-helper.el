(let ((current-directory (file-name-directory (f-this-file))))
  (setq test/test-path (f-expand "." current-directory)
        test/root-path (f-expand ".." current-directory)
        test/sandbox-path (f-expand "sandbox" test/test-path)
        test/package-path (f-expand "servant/packages" test/test-path)
        test/servant-url "http://127.0.0.1:9191/packages/"
        test/cask-file (f-expand "Cask" test/sandbox-path)
        test/package-template ";;; <name>.el --- desc\n;; Version: <version>\n(provide '<name>)\n;;; <name>.el ends here"
        package-archives `(("servant" . ,test/servant-url))))

(add-to-list 'load-path test/root-path)

(require 'package)

(defvar test/packages '((package-one (0 0 1))
                        (package-two (0 0 1))
                        (package-two (0 0 2))))

(ignore-errors (make-directory test/sandbox-path))

(if (featurep 'pallet)
    (unload-feature 'pallet t))

(require 'pallet)
(require 'el-mock)
(require 'cl)
(require 'package)

(defun test/package-text (package)
  "Return text defining a given PACKAGE"
  (let ((name (symbol-name (car package)))
        (version (test/version-string (cadr package))))
    (s-replace-all `(("<name>" . ,name) ("<version>" . ,version))
                   test/package-template)))

(defun test/add-servant-package (package)
  "Add PACKAGE to the servant repository"
  (ignore-errors (make-directory test/package-path))
  (f-write (test/package-text package)
           'utf-8
           (f-expand (format "%s.el"
                             (test/versioned-name (car package)
                                                  (cadr package)))
                     test/package-path))
  (test/servant-command "index"))

(defun test/cask-file-contains-p (text)
  "Return whether the Cask file contains TEXT"
  (s-contains? text (f-read test/cask-file)))

(defun test/create-cask-file (text)
  "Create a Cask file in the sandbox containing `text'"
  (f-write text 'utf-8 test/cask-file))

(defun test/create-cask-file-with-servant (&optional text)
  (test/create-cask-file
   (format "(source \"servant\" \"%s\")%s" test/servant-url text)))

(defun test/package-delete (package)
  "Delete PACKAGE in 24.3.1 or >= 24.3.5 environments."
  (if (fboundp 'package-desc-create)
      (package-delete (test/package-desc-create package))
    (package-delete (symbol-name (car package))
                    (test/version-string (cadr package)))))

(defun test/package-desc-create (package)
  "Return a package-desc for PACKAGE"
  (let ((name (car package))
        (version (cadr package)))
    (package-desc-create
     :name name
     :version version
     :kind 'single
     :summary ""
     :archive "servant"
     :dir (f-expand
           (test/versioned-name name version)
           package-user-dir))))

(defun test/versioned-name (name version)
  "Return a versioned file name as string from string `name' and list `version'"
  (s-concat (symbol-name name) "-" (test/version-string version)))

(defun test/version-string (version)
  "Return a string from a version list"
  (s-join "." (mapcar 'number-to-string version)))

(defun test/package-file (package)
  "Return the file path for PACKAGE"
  (let* ((pkg-versioned-name (test/versioned-name (car package) (cadr package)))
         (pkg-file-name (format "%s.el" pkg-versioned-name)))
    (f-expand pkg-file-name test/package-path)))

(defun test/servant-command (cmd &optional async)
  "Run servant CMD in the test path"
  (let ((c (format "cd %s && cask exec servant %s --path %s %s"
                         test/root-path
                         cmd
                         test/test-path
                         (if async "& nohup" ""))))
    (shell-command c t nil)))

(defmacro test/with-sandbox (&rest body)
  "Run BODY and clean up afterwards"
  `(let ((default-directory ,test/sandbox-path)
         (user-emacs-directory ,test/sandbox-path)
         (package-user-dir ,(f-expand "elpa" test/sandbox-path)))
     (unwind-protect
         (progn
           ,@body)
       (progn
         (pallet-mode -1)
         (test/cleanup-packages)
         (test/cleanup-sandbox)
         (test/cleanup-servant)))))

(defun test/cleanup-packages ()
  "Uninstall any installed test packages"
  (package-initialize)
  (mapcar (lambda (package)
            (when (package-installed-p (car package) (cadr package))
              (ignore-errors
                (test/package-delete package))))
          test/packages)
  (package-initialize))

(defun test/cleanup-sandbox ()
  "Clean the sandbox."
  (f-entries test/sandbox-path
             (lambda (entry)
               (f-delete entry t))))

(defun test/cleanup-servant ()
  "Clean the servant package directory."
  (f-entries test/package-path
             (lambda (entry)
               (f-delete entry t))))

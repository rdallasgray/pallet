(let ((current-directory (file-name-directory load-file-name)))
  (setq pallet-test-test-path (f-expand "." current-directory)
        pallet-test-root-path (f-expand ".." current-directory)
        pallet-test-sandbox-path (f-expand "sandbox" pallet-test-test-path)))

(add-to-list 'load-path pallet-test-root-path)

(require 'package)

(defvar pallet-test-servant-url "http://127.0.0.1:9191/packages/")
(defvar pallet-test-servant-dir
  (f-expand "servant/package" pallet-test-sandbox-path))
(defvar pallet-test-packages '(("package-one" (0 0 1))
                               ("package-two" (0 0 1))
                               ("package-two" (0 0 2))))

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

(defun pallet-test-create-cask-file-with-servant (&optional text)
  (pallet-test-create-cask-file
   (format "(source \"servant\" \"%s\")%s" pallet-test-servant-url text)))

(defun package-delete (pkg-desc)
  (let ((dir (package-desc-dir pkg-desc)))
    (if (not (string-prefix-p (file-name-as-directory
                               (expand-file-name package-user-dir))
                              (expand-file-name dir)))
        ;; Don't delete "system" packages.
	(error "Package `%s' is a system package, not deleting"
               (package-desc-full-name pkg-desc))
      (delete-directory dir t t)
      ;; Remove NAME-VERSION.signed file.
      (let ((signed-file (concat dir ".signed")))
	(if (file-exists-p signed-file)
	    (delete-file signed-file)))
      ;; Update package-alist.
      (let* ((name (package-desc-name pkg-desc))
             (pkgs (assq name package-alist)))
        (delete pkg-desc pkgs)
        (unless (cdr pkgs)
          (setq package-alist (delq pkgs package-alist))))
      (message "Package `%s' deleted." (package-desc-full-name pkg-desc)))))

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
  (s-concat name "-" (s-join "." (mapcar 'number-to-string version))))

(defmacro pallet-test-with-sandbox (&rest body)
  "Run BODY in a clean environment."
  `(let ((default-directory ,pallet-test-sandbox-path)
         (user-emacs-directory ,pallet-test-sandbox-path)
         (package-user-dir ,(f-expand "elpa" pallet-test-sandbox-path)))
     (progn
       (pallet-mode -1)
       (pallet-test-cleanup-sandbox)
       (pallet-test-cleanup-packages)
       (pallet-test-cleanup-cask)
       ,@body)))

(defun pallet-test-cleanup-sandbox ()
  "Clean the sandbox."
  (f-entries pallet-test-sandbox-path
             (lambda (entry)
               (f-delete entry t))))

(defun pallet-test-cleanup-packages ()
  "Uninstall any installed test packages"
  (mapcar (lambda (package)
            (when (package-installed-p (intern (car package)))
              (pallet-test-do-package-delete (car package) (cadr package))))
          pallet-test-packages))

(defun pallet-test-cleanup-cask ()
  "Remove the Cask file and reset Cask"
  (ignore-errors
    (delete-file (pallet--cask-file))))

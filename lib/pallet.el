;;; pallet.el --- Manage your packages with Cask.

;; Copyright (C) 2013 Robert Dallas Gray

;; Author: Robert Dallas Gray
;; URL: https://github.com/rdallasgray/pallet
;; Version: 0.4.0
;; Created: 2013-02-24
;; Keywords: elpa, package

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; #Pallet
;; 
;; Pallet is a package-management tool for Emacs.
;; 
;; It uses rejeep's excellent
;; [Cask](https://github.com/rejeep/cask.el) as a platform to keep
;; track of your installed packages.
;; 
;; ##Target platform
;; 
;; Pallet is well tested on Emacs 24.3.1, and should work on
;; previous versions of Emacs 24. Emacs snapshot is at present making
;; changes to the package system which will prevent Pallet from working correctly.
;; 
;; ##Installation
;; 
;; To install Pallet, you should first install Cask, following the
;; instructions [here](https://github.com/rejeep/cask.el). **At present,
;; just install Cask -- don't add anything to your .emacs or init.el file**.
;; 
;; After installing Cask, there are two ways you can go, depending on
;; your situation:
;; 
;; 1. **I have a working Emacs install, with packages already installed,
;;    and can access [MELPA](http://melpa.milbox.org).**
;; 
;;    In this case run `M-x list-packages`, and install Pallet. Then, below
;;    the lines which initialize your package system, add:
;; 
;;    ```lisp
;;    (require 'pallet)
;;    ```
;; 
;;    Restart Emacs, and run `pallet-init`. Now you have a Cask file in your
;;    emacs.d directory which contains listings for all files you've
;;    previously installed via `package-install`, and your .emacs.d/elpa
;;    directory has been replicated under .emacs.d/.cask/.
;; 
;;    You can if you wish now delete your .emacs.d/elpa directory, and
;;    remove any lines from your init.el adding archives to
;;    `package-archive`, or running `package-initialize`.
;; 
;; 2. **I have a newly installed Emacs and/or am not set up to use
;;    package-install.**
;; 
;;    In this case, create a file called `Cask` in your emacs.d
;;    directory. Add the following lines to it:
;; 
;;    ```lisp
;;    (source melpa)
;; 
;;    (depends-on "pallet")
;;    ```
;; 
;;    Then, in terminal and in your emacs.d directory, run
;; 
;;    ```
;;    cask install
;;    ```
;; 
;;    This will create a `.cask` directory inside your .emacs.d directory,
;;    initialize a package directory under .emacs.d/.cask/, and install
;;    Pallet to it.
;; 
;; **Finally, make sure the following lines are in your init.el, before any
;;   packages are required:**
;; 
;; ```lisp
;; (require 'cask "~/.cask/cask.el")
;; (cask-initialize)
;; (require 'pallet)
;; ```
;; 
;; ##What problem does Pallet solve?
;; 
;; You are an Emacs user, and you use package.el to maintain a set of
;; installed packages, via the Elpa archive (and/or Melpa, Marmalade,
;; whatever). You use Emacs at more than one site or on more than one
;; machine, and you want to use the same packages with each Emacs
;; installation.
;; 
;; The standard way of synchronising settings among Emacs installs is to
;; keep your emacs.d directory under version control, perhaps using
;; Git. That way, you can simply push your settings to a repo when they
;; change, and pull them at another site. This way of working also works
;; with the `/elpa` subdirectory of emacs.d, keeping your installed
;; packages in a versioned repository.
;; 
;; ###Package management
;; 
;; Working this way with the `/elpa` directory quickly becomes a chore,
;; though. The directory can become large and complex, and you can
;; encounter merge conflicts when trying to synchronise it across Emacs
;; installs. The solution is a dependency management system (like Ruby's
;; [Bundler](http://gembundler.com), for example) which allows you to
;; keep one 'manifest' file under version control, and ignore the
;; *actual* installed packages; the manifest file lists packages and
;; versions, and the dependency manager can install and update the listed
;; packages whenever required to.
;; 
;; ###Cask
;; 
;; [Cask](https://github.com/rejeep/cask.el) is a dependency manager
;; for Emacs, which is gaining currency especially in new Elisp
;; projects. It provides a simple format for creating manifest files, and
;; a set of functionality to install and update packages (as well as some
;; very useful utilities for package *development*).
;; 
;; The piece missing from Cask is the functionality to create and
;; maintain a manifest file *in tandem* with package.el, Emacs' built-in
;; package system.
;; 
;; You can, of course, manually maintain your Cask (manifest) file, but
;; most of us like to use `M-x package-list-packages` to discover and
;; install packages. Pallet lets you do exactly this.
;; 
;; ##How does it work?
;; 
;; First, you need to install Pallet (see above).
;; 
;; `M-x pallet-init` will look at your installed packages and source
;; archives and create a valid Cask file in your Emacs directory. You
;; now no longer need to keep your `/elpa` directory under version
;; control (in fact, you can delete it as Cask will now manage your
;; packages for you); simply keep your Cask file under version control, and use
;; Cask and Pallet to keep your packages synchronised across Emacs
;; installs. Pallet will update your Cask file when you add or delete packages via
;; `list-packages`.
;; 
;; ##Alternatives
;; 
;; [el-get](https://github.com/dimitri/el-get) is a popular and
;; feature-packed project which does much more than Pallet. Pallet just
;; tries to do one simple thing well enough.
;;
;;; Code:

;; We need to get a copy of the package-archives alist
;; before requiring Cask, as doing so will empty the list.
(package-initialize)

(defvar pallet--package-archives-copy
  (copy-alist package-archives))


(require 'cask)


;; interactive/api functions

;;;###autoload
(defun pallet-init ()
  "Bootstrap a Cask setup from package.el information."
  (interactive)
  (pallet-repack t)
  (pallet-install))

;;;###autoload
(defun pallet-repack (&optional use-copy)
  "Recreate the Cask file from package.el information;
use `pallet--package-archives-copy' if USE-COPY is true."
  (let ((archive-alist
         (if use-copy pallet--package-archives-copy package-archives)))
    (pallet--ship archive-alist (pallet--pick-packages))))

;;;###autoload
(defun pallet-install ()
  "Install packages from the Cask file."
  (interactive)
  (pallet--cask-up
   (lambda () (cask-install))))

;;;###autoload
(defun pallet-update ()
  "Update installed packages."
  (interactive)
  (pallet--cask-up
      (lambda () (cask-update))))


;;; private functions

(defun pallet--cask-up (&optional body)
  "Attempt to initialize Cask, optionally running BODY if initialisation succeeds."
  (if (file-exists-p (pallet--cask-file))
      (progn
        (setq cask-runtime-dependencies '())
        (cask-initialize)
        (when body (funcall body)))
    (message "No Cask file found. Run `pallet-init' to create one.")))

(defun pallet--cask-file ()
  "Location of the Cask file."
  (expand-file-name "Cask" user-emacs-directory))

(defun pallet--enable-cask-up-on-load ()
  "Add a hook to run `pallet--cask-up' when Emacs has initialised."
  (add-hook 'after-init-hook 'pallet--cask-up))

(defun pallet--package-name (package-name-or-desc)
  "Return a package name from a string or package-desc struct in PACKAGE-NAME-OR-DESC."
  (if (or (stringp package-name-or-desc)
          (symbolp package-name-or-desc))
      (format "%s" package-name-or-desc)
    (if (fboundp 'package-desc-name)
        (format "%s" (package-desc-name package-name-or-desc))
      nil)))

(defun pallet--installed-p (package-name)
  "Return t if (string) PACKAGE-NAME is installed, or nil otherwise."
  ;; Ensure we have up-to-date information -- package-delete doesn't
  ;; recreate package-alist automatically.
  (pallet--cask-up
   (lambda () (epl-package-installed-p (intern package-name)))))

(defun pallet--pick-packages ()
  "Get a simple list of installed packages."
  (if package-alist
      (let ((picked '()))
        (dolist (package-details package-alist)
          (push (symbol-name (car package-details)) picked))
        (reverse picked))
    nil))

(defun pallet--pick-cask ()
  "Get a list of dependencies from the Cask file."
  (pallet--pick-cask-except nil))

(defun pallet--pick-cask-except (excluded-package-name)
  "Get a list of dependencies from the Cask file, excluding EXCLUDED-PACKAGE-NAME."
  (let ((picked '()))
    (dolist (package-details cask-runtime-dependencies)
      (let ((package-name (aref package-details 1)))
        (when (not (equal package-name excluded-package-name))
          (push (format "%s" package-name) picked))))
    picked))

(defun pallet--pack (archives packages)
  "Construct a Caskfile from ARCHIVES and PACKAGES."
  (format "%s\n\n%s"
          (pallet--write-sources archives)
          (pallet--write-depends packages)))

(defun pallet--pack-one (package-name)
  "Add PACKAGE-NAME to the Caskfile."
  (pallet--cask-up
   (lambda ()
     (cask-add-dependency (format "%s" package-name))
     (pallet--ship package-archives (pallet--pick-cask)))))

(defun pallet--unpack-one (package-name)
  "Remove a PACKAGE-NAME from the Caskfile."
  (message "unpacking %s" package-name)
  (pallet--cask-up
   (lambda ()
     (pallet--ship package-archives
                     (pallet--pick-cask-except (intern package-name))))))

(defun pallet--ship (archives packages)
  "Create and save a Caskfile based on installed ARCHIVES and PACKAGES."
  (pallet--write-file (pallet--cask-file)
                 (pallet--pack archives packages)))

(defun pallet--write-sources (archive-list)
  "Create a Caskfile source set from ARCHIVE-LIST."
  (let ((source-list '()))
    (dolist (source archive-list)
      (push (pallet--format-source source) source-list))
    (mapconcat 'identity source-list "\n")))

(defun pallet--format-source (source)
  "Return a string correctly formatting an archive SOURCE."
  (let ((cask-source `(,(intern (car source)) . ,(cdr source))))
    (if (member cask-source cask-source-mapping)
        (format "(source %s)" (car source))
      (format "(source \"%s\" \"%s\")" (car source) (cdr source)))))

(defun pallet--write-depends (package-list)
  "Create a Caskfile dependency set from PACKAGE-LIST."
  (let ((depends-list '()))
    (dolist (package package-list)
      (push (format "(depends-on \"%s\")" package) depends-list))
    (let ((depends-list (sort depends-list #'string<)))
      (mapconcat 'identity depends-list "\n"))))

(defun pallet--write-file (file contents)
  "Write to FILE the given (string) CONTENTS."
  (with-temp-file file
    (insert contents)))


;; add hook to enable Cask init on load

(pallet--enable-cask-up-on-load)


;; advise package.el functions

(defadvice package-install
    (after pallet--after-install (package-name-or-desc) activate)
  "Add a dependency to the Cask file after `package-install'."
  (let ((package-name (pallet--package-name package-name-or-desc)))
    (pallet--pack-one package-name)))

(defadvice package-delete
    (after pallet--after-delete (package-name-or-desc &optional version) activate)
  "Remove a dependency from the Cask file after `package-delete'."
  ;; NB check if package is still installed; updates trigger deletes
  (let ((package-name (pallet--package-name package-name-or-desc)))
    (when (not (pallet--installed-p package-name))
      (pallet--unpack-one package-name))))


(provide 'pallet)

;;; pallet.el ends here

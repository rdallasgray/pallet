;;; pallet.el --- Manage your packages with Cask.

;; Copyright (C) 2014 Robert Dallas Gray

;; Author: Robert Dallas Gray
;; URL: https://github.com/rdallasgray/pallet
;; Version: 0.6.5
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
;; Pallet is a package management tool for Emacs.
;; 
;; It uses @rejeep's excellent
;; [Cask](https://github.com/rejeep/cask.el) as a platform to keep
;; track of your installed packages.
;; 
;; ##News
;; Pallet version 0.6 is now available. This version introduces some
;; breaking changes against previous versions, the most significant being
;; that Pallet will *only* work with Cask v0.6 or later.
;; 
;; ##Target platform
;; 
;; Pallet should work with Emacs 24 (including recent snapshots).
;; 
;; ##Use
;; Pallet has a very simple interface:
;; - `M-x pallet-init` creates a Cask file using information about
;;   installed packages from the package.el system
;; - `M-x pallet-install` installs packages listed in your Cask file
;; - `M-x pallet-update` updates installed packages
;; 
;; Pallet's main job, though, is to add and delete package references
;; from your Cask file as you install and delete them using the built-in
;; Emacs package management system. It does this automatically and silently.
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
;;    In this case run `M-x list-packages`, and install Pallet.
;;    Then, run `M-x pallet-init`. Now you have a Cask file in your
;;    emacs.d directory which contains listings for all files you've
;;    previously installed via `package-install`, and your .emacs.d/elpa
;;    directory has been replicated under .emacs.d/.cask/.
;; 
;;    You can if you wish now delete your .emacs.d/elpa directory, and
;;    remove any lines from your init.el adding archives to
;;    `package-archive`, or running `package-initialize`.
;; 
;; 2. **I have a newly installed Emacs and/or am not set up to access
;;    MELPA.**
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
;;    This will create a .cask directory inside your .emacs.d directory,
;;    initialize a package directory under .emacs.d/.cask/, and install
;;    Pallet to it.
;; 
;; **Finally, make sure the following lines are in your init.el, before any
;;   packages are required:**
;; 
;; ```lisp
;; (require 'cask "<path-to-cask>/cask.el")
;; (cask-initialize)
;; (require 'pallet)
;; ```
;; 
;; `<path-to-cask>` will vary depending on how you installed Cask: if you
;; installed via the `curl` method, it is likely to be `~/.cask`; if you
;; installed via Homebrew, it is likely to be `/usr/local/Cellar/cask/<version>`.
;; 
;; ##Contributing
;; Contributions to Pallet are very welcome.
;; 
;; Fork and clone the repo, then run `git
;; submodule update --init`, which will install
;; [el.mk](http://github.com/rdallasgray/el.mk).
;; 
;; Now, [install Cask](https://github.com/rejeep/cask.el).
;; 
;; Then run `cask install`. You should now be able to run the tests using
;; `make test`.
;; 
;; Any new feature or bugfix should be covered by tests -- see the files
;; in /test for guidance on how to write your own. When you've
;; created your feature, make a pull request against master in this repo.
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
  (pallet--repack t)
  (pallet-install))

;;;###autoload
(defun pallet-install ()
  "Install packages from the Cask file."
  (interactive)
  (pallet--cask-up
   (lambda (bundle) (cask-install bundle))))

;;;###autoload
(defun pallet-update ()
  "Update installed packages."
  (interactive)
  (pallet--suspend-deletes
   (pallet--cask-up
    (lambda (bundle) (cask-update bundle)))))

;;; private functions

(defun pallet--repack (&optional use-copy)
  "Recreate the Cask file from package.el information;
use `pallet--package-archives-copy' if USE-COPY is true."
  (let ((archive-alist
         (if use-copy pallet--package-archives-copy package-archives)))
    (pallet--ship archive-alist (pallet--pick-packages))))

(defun pallet--cask-up (&optional body)
  "Attempt to initialize Cask, optionally running BODY if initialisation succeeds."
  (if (file-exists-p (pallet--cask-file))
      (let ((bundle (cask-initialize)))
        (when body (funcall body bundle)))
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

(defun pallet--suspend-deletes (body)
  (ad-disable-advice 'package-delete 'after 'pallet--after-delete)
  (when body (funcall body))
  (ad-enable-advice 'package-delete 'after 'pallet--after-delete))

(defun pallet--pick-packages ()
  "Get a simple list of installed packages."
  (if package-alist
      (let ((picked '()))
        (dolist (package-details package-alist)
          (push (symbol-name (car package-details)) picked))
        (reverse picked))
    nil))

(defun pallet--pick-cask (bundle)
  "Get a list of dependencies from the Cask BUNDLE."
  (pallet--pick-cask-except bundle nil))

(defun pallet--pick-cask-except (bundle excluded-package-name)
  "Get a list of dependencies from the Cask BUNDLE, excluding EXCLUDED-PACKAGE-NAME."
  (let ((picked '()))
    (dolist (package-details (cask-runtime-dependencies bundle))
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
   (lambda (bundle)
     (cask-add-dependency bundle (format "%s" package-name) :scope 'runtime)
     (pallet--ship package-archives (pallet--pick-cask bundle)))))

(defun pallet--unpack-one (package-name)
  "Remove a PACKAGE-NAME from the Caskfile."
  (pallet--cask-up
   (lambda (bundle)
     (pallet--ship package-archives
                     (pallet--pick-cask-except bundle (intern package-name))))))

(defun pallet--ship (archives packages)
  "Create and save a Caskfile based on installed ARCHIVES and PACKAGES."
  (pallet--write-file (pallet--cask-file)
                 (pallet--pack archives packages)))

(defun pallet--write-sources (archive-list)
  "Create a Caskfile source set from ARCHIVE-LIST."
  (let ((source-list '()))
    (dolist (source archive-list)
      (push (pallet--format-source source) source-list))
    (mapconcat 'identity (sort source-list #'string<) "\n")))

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

(defun pallet--installed-p (package-name)
  "Return t if (string) PACKAGE-NAME is installed, or nil otherwise."
  ;; Ensure we have up-to-date information -- package-delete doesn't
  ;; recreate package-alist automatically.
  (epl-initialize t)
  (epl-package-installed-p (intern package-name)))


;; add hook to enable Cask init on load

(pallet--enable-cask-up-on-load)


;; advise package.el functions

(defadvice package-install
  (after pallet--after-install (package-name-or-desc) activate)
  "Add a dependency to the Cask file after `package-install'."
  (let ((package-name (pallet--package-name package-name-or-desc)))
    (message "Pallet: packing %s" package-name)
    (pallet--pack-one package-name)))

(defadvice package-delete
  (after pallet--after-delete (package-name-or-desc &optional version) activate)
  "Remove a dependency from the Cask file after `package-delete'."
  ;; NB check if package is still installed; updates trigger deletes
  (let ((package-name (pallet--package-name package-name-or-desc)))
    (when (not (pallet--installed-p package-name))
      (message "Pallet: unpacking %s" package-name)
      (pallet--unpack-one package-name))))


(provide 'pallet)

;;; pallet.el ends here

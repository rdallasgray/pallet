;;; pallet.el --- Manage your packages with Carton.

;; Copyright (C) 2013 Robert Dallas Gray

;; Author: Robert Dallas Gray
;; URL: https://github.com/rdallasgray/pallet
;; Version: 0.1.5
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
;; ##tl;dr
;; 
;; Pallet is a simple package-management system for Emacs.
;; 
;; It uses rejeep's excellent
;; [Carton](https://github.com/rejeep/carton.git) as a platform to keep
;; track of your installed packages.
;; 
;; You can install it, via [Melpa](http://melpa.milkbox.net), using
;; `list-packages`. Use it by adding ```(require 'pallet)``` to your
;; `init.el` or `.emacs` file. You should add the line *before* you
;; require other packages; it will run `(package initialize)` for you,
;; and add the package archives listed in your Carton file.
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
;; ###Carton
;; 
;; [Carton](https://github.com/rejeep/carton.git) is a dependency manager
;; for Emacs, which is gaining currency especially in new Elisp
;; projects. It provides a simple format for creating manifest files, and
;; a set of functionality to install and update packages (as well as some
;; very useful utilities for package *development*).
;; 
;; The piece missing from Carton is the functionality to create and
;; maintain a manifest file *in tandem* with package.el, Emacs' built-in
;; package system.
;; 
;; You can, of course, manually maintain your Carton (manifest) file, but
;; most of us like to use `M-x package-list-packages` to discover and
;; install packages. Pallet lets you do exactly this.
;; 
;; ##How does it work?
;; 
;; First, you need to install Pallet (via Melpa being the easiest way),
;; and `(require 'pallet)` in your Emacs initialisation file.
;; 
;; `M-x pallet-init` will look at your installed packages and source
;; archives and create a valid Carton file in your Emacs directory. You
;; now no longer need to keep your `/elpa` directory under version
;; control; simply keep your Carton file under version control, and use
;; Carton to keep your packages synchronised across Emacs installs.
;; 
;; Pallet will update your Carton file when you add or delete packages via
;; `list-packages`, or when you run `M-x pallet-repack`.  You can install
;; your Carton-managed packages using `pallet-install`, and update them
;; using `pallet-update`. These commands are just interactive aliases of
;; the relevant Carton functions.
;; 
;; ##Alternatives
;; 
;; [el-get](https://github.com/dimitri/el-get) is a popular and
;; feature-packed project which does much more than Pallet. Pallet just
;; tries to do one simple thing well enough.
;; 
;; ##What's coming?
;; 
;; More configurability, maybe package versioning and rollbacks,
;; dependency awareness ... tell me what you need, or, better,
;; contribute.
;; 
;;
;;; Code:

(package-initialize)

(require 'carton)

(defgroup pallet nil
  "Settings for the Pallet package manager.")

(defcustom pallet-repack-on-close nil
  "Whether to update the Carton file on closing Emacs."
  :type 'boolean
  :group 'pallet)

(defcustom pallet-cartonise-on-load t
  "Whether to read the Carton file on loading pallet."
  :type 'boolean
  :group 'pallet)

(defcustom pallet-pack-on-install t
  "Whether to add a package to the Carton file on `package-install'."
  :type 'boolean
  :group 'pallet)

(defcustom pallet-unpack-on-delete t
  "Whether to remove a package from the Carton file on package-delete."
  :type 'boolean
  :group 'pallet)

(defun pallet-init ()
  "Bootstrap a Carton setup from Elpa details."
  (interactive)
  (pallet-repack))

(defun pallet-repack ()
  "Recreate the Carton file from Elpa details."
  (interactive)
  (pt/pallet-ship package-archives (pt/pallet-pick-packages)))

(defun pallet-install ()
  "Install packages from the Carton file."
  (interactive)
  (pt/cartonise)
  (carton-command-install))

(defun pallet-update ()
  "Update installed packages."
  (interactive)
  (pt/suspend-delete
   (lambda ()
     (pt/cartonise)
     (carton-command-update))))

(defun pt/suspend-delete (body)
  "Suspend delete during execution of BODY."
  (let ((pallet-unpack-on-delete nil))
    (funcall body)))

(defun pt/cartonise ()
  "Set up a carton project in the user's Emacs directory."
  (setq carton-runtime-dependencies '())
  (carton-setup user-emacs-directory))

(defun pt/carton-file ()
  "Location of the Carton file."
  (expand-file-name "Carton" user-emacs-directory))

(defun pt/enable-repack-on-close ()
  "Add a hook to run pallet-repack when Emacs closes."
  (add-hook 'kill-emacs-hook 'pt/maybe-repack-on-close))

(defun pt/enable-cartonise-on-load ()
  "Add a hook to run pt/cartonise when Emacs has initialised."
  (add-hook 'after-init-hook 'pt/maybe-cartonise-on-load))

(defadvice package-install (after pt/after-install (package-name) activate)
  "Run pt/pallet-pack-one after `package-install'."
  (pt/maybe-pack-on-install package-name))

(defadvice package-delete (after pt/after-delete (package-name version) activate)
  "Run pt/pallet-unpack-one after `package-delete'."
  (pt/maybe-unpack-on-delete package-name))

(defun pt/maybe-repack-on-close ()
  "Repack if pallet-repack-on-close is true."
  (when pallet-repack-on-close (pallet-repack)))

(defun pt/maybe-cartonise-on-load ()
  "Load the Carton file if pallet-cartonise-on-load is true."
  (when pallet-cartonise-on-load (pt/cartonise)))

(defun pt/maybe-pack-on-install (package-name)
  "Pack PACKAGE-NAME if pallet-pack-on-install is true."
  (when pallet-pack-on-install (pt/pallet-pack-one package-name)))

(defun pt/maybe-unpack-on-delete (package-name)
  "Unpack PACKAGE-NAME if pallet-unpack-on-delete is true."
  (when pallet-unpack-on-delete (pt/pallet-unpack-one package-name)))

(defun pt/pallet-pick-packages ()
  "Get a simple list of Elpa-installed packages."
  (if package-alist
      (let ((picked '()))
        (dolist (package-details package-alist)
          (push (symbol-name (car package-details)) picked))
        (reverse picked))
    nil))

(defun pt/pallet-pick-carton ()
  "Get a list of dependencies from the Carton file."
  (pt/pallet-pick-carton-except nil))

(defun pt/pallet-pick-carton-except (excluded-package-name)
  "Get a list of dependencies from the Carton file, excluding EXCLUDED-PACKAGE-NAME."
  (let ((picked '()))
    (dolist (package-details carton-runtime-dependencies)
      (let ((package-name (aref package-details 1)))
        (when (not (equal package-name excluded-package-name))
          (push (format "%s" package-name) picked))))
    picked))

(defun pt/pallet-pack (archives packages)
  "Construct a Cartonfile from ARCHIVES and PACKAGES."
  (format "%s\n\n%s"
          (pt/write-sources archives)
          (pt/write-depends packages)))

(defun pt/pallet-pack-one (package-name)
  "Add PACKAGE-NAME to the Cartonfile."
  (pt/cartonise)
  (carton-add-dependency (format "%s" package-name))
  (pt/pallet-ship package-archives (pt/pallet-pick-carton)))

(defun pt/pallet-unpack-one (package-name)
  "Remove a PACKAGE-NAME from the Cartonfile."
  (pt/cartonise)
  (pt/pallet-ship package-archives
                  (pt/pallet-pick-carton-except (intern package-name))))

(defun pt/pallet-ship (archives packages)
  "Create and save a Cartonfile based on installed ARCHIVES and PACKAGES."
    (pt/write-file (pt/carton-file)
                   (pt/pallet-pack archives packages)))

(defun pt/write-sources (archive-list)
  "Create a Cartonfile source set from ARCHIVE-LIST."
  (let ((source-list '()))
    (dolist (source archive-list)
      (push (format "(source \"%s\" \"%s\")" (car source) (cdr source)) source-list))
    (mapconcat 'identity source-list "\n")))

(defun pt/write-depends (package-list)
  "Create a Cartonfile dependency set from PACKAGE-LIST."
  (let ((depends-list '()))
    (dolist (package package-list)
      (push (format "(depends-on \"%s\")" package) depends-list))
    (let ((depends-list (sort depends-list #'string<)))
      (mapconcat 'identity depends-list "\n"))))

(defun pt/write-file (file contents)
  "Write to FILE the given (string) CONTENTS."
  (with-temp-file file
    (insert contents)))

(pt/enable-cartonise-on-load)
(pt/enable-repack-on-close)

(provide 'pallet)
;;; pallet.el ends here

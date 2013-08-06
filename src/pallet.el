;;; pallet.el --- Manage your packages with Cask.

;; Copyright (C) @YEAR Robert Dallas Gray

;; Author: Robert Dallas Gray
;; URL: https://github.com/rdallasgray/pallet
;; Version: @VERSION
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
@COMMENTARY
;;
;;; Code:

(package-initialize)

(require 'cask)

(defgroup pallet nil
  "Settings for the Pallet package manager.")

(defcustom pallet-repack-on-close nil
  "Whether to update the Cask file on closing Emacs."
  :type 'boolean
  :group 'pallet)

(defcustom pallet-cask-up-on-load t
  "Whether to read the Cask file on loading pallet."
  :type 'boolean
  :group 'pallet)

(defcustom pallet-pack-on-install t
  "Whether to add a package to the Cask file on `package-install'."
  :type 'boolean
  :group 'pallet)

(defcustom pallet-unpack-on-delete t
  "Whether to remove a package from the Cask file on package-delete."
  :type 'boolean
  :group 'pallet)

(defun pallet-init ()
  "Bootstrap a Cask setup from Elpa details."
  (interactive)
  (pallet-repack))

(defun pallet-repack ()
  "Recreate the Cask file from Elpa details."
  (interactive)
  (pt/pallet-ship package-archives (pt/pallet-pick-packages)))

(defun pallet-install ()
  "Install packages from the Cask file."
  (interactive)
  (pt/cask-up)
  (cask-command-install))

(defun pallet-update ()
  "Update installed packages."
  (interactive)
  (pt/suspend-delete
   (lambda ()
     (pt/cask-up)
     (cask-command-update))))

(defun pt/suspend-delete (body)
  "Suspend delete during execution of BODY."
  (let ((pallet-unpack-on-delete nil))
    (funcall body)))

(defun pt/cask-up ()
  "Set up a cask project in the user's Emacs directory."
  (setq cask-runtime-dependencies '())
  (cask-setup user-emacs-directory))

(defun pt/cask-file ()
  "Location of the Cask file."
  (expand-file-name "Cask" user-emacs-directory))

(defun pt/enable-repack-on-close ()
  "Add a hook to run pallet-repack when Emacs closes."
  (add-hook 'kill-emacs-hook 'pt/maybe-repack-on-close))

(defun pt/enable-cask-up-on-load ()
  "Add a hook to run pt/cask-up when Emacs has initialised."
  (add-hook 'after-init-hook 'pt/maybe-cask-up-on-load))

(defadvice package-install (after pt/after-install (package-name) activate)
  "Run pt/pallet-pack-one after `package-install'."
  (pt/maybe-pack-on-install package-name))

(defadvice package-delete (after pt/after-delete (package-name version) activate)
  "Run pt/pallet-unpack-one after `package-delete'."
  (pt/maybe-unpack-on-delete package-name))

(defun pt/maybe-repack-on-close ()
  "Repack if pallet-repack-on-close is true."
  (when pallet-repack-on-close (pallet-repack)))

(defun pt/maybe-cask-up-on-load ()
  "Load the Cask file if pallet-cask-up-on-load is true."
  (when pallet-cask-up-on-load (pt/cask-up)))

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

(defun pt/pallet-pick-cask ()
  "Get a list of dependencies from the Cask file."
  (pt/pallet-pick-cask-except nil))

(defun pt/pallet-pick-cask-except (excluded-package-name)
  "Get a list of dependencies from the Cask file, excluding EXCLUDED-PACKAGE-NAME."
  (let ((picked '()))
    (dolist (package-details cask-runtime-dependencies)
      (let ((package-name (aref package-details 1)))
        (when (not (equal package-name excluded-package-name))
          (push (format "%s" package-name) picked))))
    picked))

(defun pt/pallet-pack (archives packages)
  "Construct a Caskfile from ARCHIVES and PACKAGES."
  (format "%s\n\n%s"
          (pt/write-sources archives)
          (pt/write-depends packages)))

(defun pt/pallet-pack-one (package-name)
  "Add PACKAGE-NAME to the Caskfile."
  (pt/cask-up)
  (cask-add-dependency (format "%s" package-name))
  (pt/pallet-ship package-archives (pt/pallet-pick-cask)))

(defun pt/pallet-unpack-one (package-name)
  "Remove a PACKAGE-NAME from the Caskfile."
  (pt/cask-up)
  (pt/pallet-ship package-archives
                  (pt/pallet-pick-cask-except (intern package-name))))

(defun pt/pallet-ship (archives packages)
  "Create and save a Caskfile based on installed ARCHIVES and PACKAGES."
    (pt/write-file (pt/cask-file)
                   (pt/pallet-pack archives packages)))

(defun pt/write-sources (archive-list)
  "Create a Caskfile source set from ARCHIVE-LIST."
  (let ((source-list '()))
    (dolist (source archive-list)
      (push (format "(source \"%s\" \"%s\")" (car source) (cdr source)) source-list))
    (mapconcat 'identity source-list "\n")))

(defun pt/write-depends (package-list)
  "Create a Caskfile dependency set from PACKAGE-LIST."
  (let ((depends-list '()))
    (dolist (package package-list)
      (push (format "(depends-on \"%s\")" package) depends-list))
    (let ((depends-list (sort depends-list #'string<)))
      (mapconcat 'identity depends-list "\n"))))

(defun pt/write-file (file contents)
  "Write to FILE the given (string) CONTENTS."
  (with-temp-file file
    (insert contents)))

(pt/enable-cask-up-on-load)
(pt/enable-repack-on-close)

(provide 'pallet)
;;; pallet.el ends here

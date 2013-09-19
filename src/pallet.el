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

;; We need to get a copy of the package-archives alist
;; before requiring Cask, as doing so will empty the list.
(package-initialize)

(defvar pt/package-archives-copy
  (copy-alist package-archives))

(require 'cask)

(defgroup pallet nil
  "Settings for the Pallet package management tool."
  :group 'tools
  :group 'package)

(defcustom pallet-repack-on-close nil
  "Whether to run `pallet-repack' on closing Emacs."
  :type 'boolean
  :group 'pallet)

(defcustom pallet-cask-up-on-load t
  "Whether to run `pt/cask-up' on loading pallet."
  :type 'boolean
  :group 'pallet)

(defcustom pallet-pack-on-install t
  "Whether to run `pt/pallet-pack-one' on `package-install'."
  :type 'boolean
  :group 'pallet)

(defcustom pallet-unpack-on-delete t
  "Whether to run `pt/pallet-unpack-one' on `package-delete'."
  :type 'boolean
  :group 'pallet)

(defun pallet-init ()
  "Bootstrap a Cask setup from package.el information."
  (interactive)
  (pallet-repack t)
  (pallet-install))

(defun pallet-repack (&optional use-copy)
  "Recreate the Cask file from package.el information;
use `pt/package-archives-copy' if USE-COPY is true."
  (let ((archive-alist
         (if use-copy pt/package-archives-copy package-archives)))
    (pt/pallet-ship archive-alist (pt/pallet-pick-packages))))

(defun pallet-install ()
  "Install packages from the Cask file."
  (interactive)
  (pt/cask-up
   (lambda () (cask-install))))

(defun pallet-update ()
  "Update installed packages."
  (interactive)
  (pt/suspend-delete
   (lambda ()
     (pt/cask-up
      (lambda () (cask-update))))))

(defun pt/suspend-delete (body)
  "Suspend deletion of packages from the Cask file during execution of BODY.
We want to do this e.g. whie updating packages, as this is done with an install
followed by a delete."
  (let ((pallet-unpack-on-delete nil))
    (funcall body)))

(defun pt/cask-up (&optional body)
  "Attempt to initialize Cask, optionally running BODY if initialisation succeeds."
  (if (file-exists-p (pt/cask-file))
      (progn
        (setq cask-runtime-dependencies '())
        (cask-initialize)
        (when body (funcall body)))
    (message "No Cask file found. Run `pallet-init' to create one.")))

(defun pt/cask-file ()
  "Location of the Cask file."
  (expand-file-name "Cask" user-emacs-directory))

(defun pt/enable-repack-on-close ()
  "Add a hook to run `pallet-repack' when Emacs closes."
  (add-hook 'kill-emacs-hook 'pt/maybe-repack-on-close))

(defun pt/enable-cask-up-on-load ()
  "Add a hook to run `pt/cask-up' when Emacs has initialised."
  (add-hook 'after-init-hook 'pt/maybe-cask-up-on-load))

(defadvice package-install (after pt/after-install (package-name) activate)
  "Run `pt/maybe-pack-on-install' after `package-install'."
  (pt/maybe-pack-on-install package-name))

(defadvice package-delete (after pt/after-delete (package-name version) activate)
  "Run `pt/maybe-unpack-on-delete' after `package-delete'."
  (pt/maybe-unpack-on-delete package-name))

(defun pt/maybe-repack-on-close ()
  "Run `pallet-repack' if `pallet-repack-on-close' is true."
  (when pallet-repack-on-close (pallet-repack)))

(defun pt/maybe-cask-up-on-load ()
  "Run `pt/cask-up' if `pallet-cask-up-on-load' is true."
  (when pallet-cask-up-on-load (pt/cask-up)))

(defun pt/maybe-pack-on-install (package-name)
  "Pack PACKAGE-NAME if `pallet-pack-on-install' is true."
  (when pallet-pack-on-install (pt/pallet-pack-one package-name)))

(defun pt/installed-p (package-name)
  "Return t if (string) PACKAGE-NAME is installed, or nil otherwise."
  ;; Ensure we have up-to-date information -- package-delete doesn't
  ;; recreate package-alist automatically.
  (pt/cask-up
   (lambda () (epl-package-installed-p (intern package-name)))))

(defun pt/maybe-unpack-on-delete (package-name)
  "Unpack PACKAGE-NAME if `pallet-unpack-on-delete' is t, and the
package is no longer installed."
  (when (and pallet-unpack-on-delete
             (not (pt/installed-p package-name)))
    (pt/pallet-unpack-one package-name)))

(defun pt/pallet-pick-packages ()
  "Get a simple list of installed packages."
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
  (pt/cask-up
   (lambda ()
     (cask-add-dependency (format "%s" package-name))
     (pt/pallet-ship package-archives (pt/pallet-pick-cask)))))

(defun pt/pallet-unpack-one (package-name)
  "Remove a PACKAGE-NAME from the Caskfile."
  (pt/cask-up
   (lambda ()
     (pt/pallet-ship package-archives
                     (pt/pallet-pick-cask-except (intern package-name))))))

(defun pt/pallet-ship (archives packages)
  "Create and save a Caskfile based on installed ARCHIVES and PACKAGES."
  (pt/write-file (pt/cask-file)
                 (pt/pallet-pack archives packages)))

(defun pt/write-sources (archive-list)
  "Create a Caskfile source set from ARCHIVE-LIST."
  (let ((source-list '()))
    (dolist (source archive-list)
      (push (pt/format-source source) source-list))
    (mapconcat 'identity source-list "\n")))

(defun pt/format-source (source)
  "Return a string correctly formatting an archive SOURCE."
  (if (member source cask-source-mapping)
      (format "(source %s)" (car source))
    (format "(source \"%s\" \"%s\")" (car source) (cdr source))))

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

;; Add hooks
(pt/enable-cask-up-on-load)
(pt/enable-repack-on-close)

(provide 'pallet)

;;; pallet.el ends here

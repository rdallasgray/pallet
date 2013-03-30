;;; pallet.el --- A minor mode to manage Elpa packages using Carton.

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

(require 'carton)

(defgroup pallet nil
  "Settings for the Pallet package manager.")

(defcustom pallet-repack-on-close nil
  "Whether to update the Carton file on closing Emacs."
  :type 'boolean
  :group 'pallet)

(defcustom pallet-pack-on-install t
  "Whether to add a package to the Carton file on package-install."
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
  (carton-install))

(defun pallet-update ()
  "Update installed packages."
  (interactive)
  (pt/cartonise)
  (carton-update))

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

(defadvice package-install (after pt/after-install (package-name) activate)
  "Run pt/pallet-pack-one after package-install."
  (pt/maybe-pack-on-install package-name))

(defadvice package-delete (after pt/after-delete (package-name version) activate)
  "Run pt/pallet-unpack-one after package-delete."
  (pt/maybe-unpack-on-delete package-name))

(defun pt/maybe-repack-on-close ()
  "Repack if pallet-repack-on-close is true."
  (when pallet-repack-on-close (pallet-repack)))

(defun pt/maybe-pack-on-install (package-name)
  "Pack the package if pallet-pack-on-install is true."
  (when pallet-pack-on-install (pt/pallet-pack-one package-name)))

(defun pt/maybe-unpack-on-delete (package-name)
  "Unpack the pacakge if pallet-unpack-on-delete is true."
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
  "Get a list of dependencies from the Carton file."
  (let ((picked '()))
    (dolist (package-details carton-runtime-dependencies)
      (let ((package-name (aref package-details 1)))
        (when (not (equal package-name excluded-package-name))
          (push (format "%s" package-name) picked))))
    (reverse picked)))

(defun pt/pallet-pack (archives packages)
  "Construct a Cartonfile from Elpa's package-alist and package-archives."
  (format "%s\n\n%s"
          (pt/write-sources archives)
          (pt/write-depends packages)))

(defun pt/pallet-pack-one (package-name)
  "Add a package to the Cartonfile."
  (pt/cartonise)
  (depends-on (format "%s" package-name))
  (pt/pallet-ship package-archives (pt/pallet-pick-carton)))

(defun pt/pallet-unpack-one (package-name)
  "Remove a package from the Cartonfile."
  (pt/cartonise)
  (pt/pallet-ship package-archives
                  (pt/pallet-pick-carton-except (intern package-name))))

(defun pt/pallet-ship (archives packages)
  "Create and save a Cartonfile based on installed packages and archives."
    (pt/write-file (pt/carton-file)
                   (pt/pallet-pack archives packages)))

(defun pt/write-sources (archive-list)
  "Create a Cartonfile source set from Elpa's package-archives."
  (let ((source-list '()))
    (dolist (source archive-list)
      (push (format "(source \"%s\" \"%s\")" (car source) (cdr source)) source-list))
    (mapconcat 'identity source-list "\n")))

(defun pt/write-depends (package-list)
  "Create a Cartonfile dependency set from Elpa's package-alist."
  (let ((depends-list '()))
    (dolist (package package-list)
      (push (format "(depends-on \"%s\")" package) depends-list))
    (sort depends-list #'string<)
    (mapconcat 'identity depends-list "\n")))

(defun pt/write-file (file contents)
  "Write the given (string) contents to the file at the given path."
  (with-temp-file file
    (insert contents)))

(pt/enable-repack-on-close)

(provide 'pallet)
;;; pallet.el ends here

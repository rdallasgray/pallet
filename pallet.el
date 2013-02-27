;;; pallet.el --- A minor mode to manage Elpa packages using Carton.

;; Copyright (C) 2012 Robert Dallas Gray

;; Author: Robert Dallas Gray
;; URL: https://github.com/rdallasgray/pallet
;; Version: 0.0.1
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
;; [Carton](https://github.com/rejeep/carton.git) is quickly becoming the de-facto package management system for Emacs.
;; It provides a simple format for creating manifest files, and a set of functionality to install and update packages
;; (as well as some very useful utilities for package *development*).
;;
;; The piece missing from Carton is the functionality to create and maintain a manifest file *in tandem* with package.el,
;; Emacs' built-in package system.
;;
;; You can, of course, manually maintain your Carton (manifest) file, but most of us like to use `M-x package-list-packages`
;; to discover and install packages. Pallet lets you do exactly this.
;;
;; `M-x pallet-init` will look at your installed packages and source archives and create a valid Carton file in your Emacs directory.
;; You now no longer need to keep your `/elpa` directory under version control; simply keep your Carton file under version
;; control, and use Carton to keep your packages synchronised across Emacs installs.
;;
;; Pallet will update your Carton file when you close Emacs, or when you run `M-x pallet-repack`, so you can use
;; `M-x package-list-packages` (or any other method) to install and delete packages as normal.
;;
;; You can install your Carton-managed packages using `pallet-install`, and update them using `pallet-update`.
;; These commands are just interactive aliases of the relevant Carton functions.
;;
;;
;; See the README for more details.
;;
;;; Code:

(require 'carton)

(defgroup pallet nil
  "Settings for the Pallet package manager.")

(defcustom pallet-repack-on-close t
  "Whether to update the Carton file on closing Emacs."
  :type 'boolean
  :group 'pallet)

(defvar pt/cartonised nil)

(defun pallet-init ()
  "Bootstrap a Carton setup from Elpa details."
  (interactive)
  (pt/pallet-ship))

(defun pallet-repack ()
  "Recreate the Carton file from Elpa details."
  (interactive)
  (pt/pallet-ship))

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
  (when (not pt/cartonised)
    (carton-setup user-emacs-directory)
    (setq pt/cartonised t)))

(defun pt/carton-file ()
  "Location of the Carton file."
  (expand-file-name "Carton" user-emacs-directory))

(defun pt/maybe-enable-repack-on-close ()
  "Add a hook to run pallet-repack when Emacs closes."
  (when pallet-repack-on-close
    (add-hook 'kill-emacs-hook 'pallet-repack)))

(defun pt/pallet-pick ()
  "Get a simple list of Elpa-installed packages."
  (if package-alist
      (let ((picked '()))
        (dolist (package-details package-alist)
          (push (symbol-name (car package-details)) picked))
        (reverse picked))
    nil))

(defun pt/pallet-pack ()
  "Construct a Cartonfile from Elpa's package-alist and package-archives."
  (format "%s\n\n%s" (pt/write-sources) (pt/write-depends)))

(defun pt/pallet-ship ()
  "Create and save a Cartonfile based on installed packages and archives."
    (pt/write-file (pt/carton-file) (pt/pallet-pack)))

(defun pt/write-sources ()
  "Create a Cartonfile source set from Elpa's package-archives."
  (if package-archives
      (let ((source-list '()))
        (dolist (source package-archives)
          (push (format "(source \"%s\" \"%s\")" (car source) (cdr source)) source-list))
        (mapconcat 'identity source-list "\n"))
    ""))

(defun pt/write-depends ()
  "Create a Cartonfile dependency set from Elpa's package-alist-alist."
  (let ((depends-list '()))
    (dolist (package (pt/pallet-pick))
      (push (format "(depends-on \"%s\")" package) depends-list))
    (mapconcat 'identity depends-list "\n")))

(defun pt/write-file (file contents)
  "Write the given (string) contents to the file at the given path."
  (with-temp-file file
    (insert contents)))

(pt/maybe-enable-repack-on-close)

(provide 'pallet)
;;; pallet.el ends here

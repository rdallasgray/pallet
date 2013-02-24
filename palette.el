;;; palette.el --- A minor mode to manage Elpa packages using Carton.

;; Copyright (C) 2012 Robert Dallas Gray

;; Author: Robert Dallas Gray
;; URL: https://github.com/rdallasgray/palette
;; Version: 0
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
;;
;; See the README for more details.
;;
;;; Code:

(defgroup palette nil
  "Settings for the Palette package manager.")

(defcustom palette-update-on-close t
  "Whether to update the Carton file on closing Emacs."
  :type 'boolean
  :group 'palette)

(defun palette-init ()
  "Bootstrap a Carton setup from Elpa details."
  (interactive)
  (pt/palette-ship)
  (carton-setup user-emacs-directory))

(defun palette-update ()
  "Recreate the Carton file from Elpa details."
  (interactive)
  (pt/palette-ship))

(defun pt/carton-file ()
  "Location of the Carton file."
  (expand-file-name "Carton" user-emacs-directory))

(defun pt/maybe-enable-update-on-close ()
  "Add a hook to run palette when Emacs closes."
  (when palette-update-on-close
    (add-hook 'kill-emacs-hook 'palette-update)))

(defun pt/palette-pick ()
  "Get a simple list of Elpa-installed packages."
  (if package-alist
      (let ((picked '()))
        (dolist (package-details package-alist)
          (push (symbol-name (car package-details)) picked))
        (reverse picked))
    nil))

(defun pt/palette-pack ()
  "Construct a Cartonfile from Elpa's package-alist and package-archives."
  (let ((packed-palette ""))
    (concat packed-palette
            (pt/write-sources)
            "\n\n"
            (pt/write-depends))
    packed-palette))

(defun pt/palette-ship ()
  "Create and save a Cartonfile based on installed packages and archives."
    (pt/write-file (pt/carton-file) (pt/palette-pack)))

(defun pt/write-sources ()
  "Create a Cartonfile source set from Elpa's package-archives."
  (if package-archives
      (let ((sources ""))
        (dolist (source package-archives)
          (concat sources
                  (format "(source %s %s)\n" (car source) (cdr source))))
        sources)
      nil))

(defun pt/write-depends ()
  "Create a Cartonfile dependency set from Elpa's package-alist-alist."
  (let ((depends ""))
    (dolist (package (pt/palette-pick))
      (concat depends
              (format "(depends-on %s)\n" package)))
    depends))

(defun pt/write-file (file contents)
  "Write the given (string) contents to the file at the given path."
  (with-temp-file file
    (insert contents)))

(pt/maybe-enable-update-on-close)

(provide 'palette)
;;; palette.el ends here

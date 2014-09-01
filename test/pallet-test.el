;; initialising Cask

;; (ert-deftest pallet-test-init ()
;;   "it writes a Cask file on pallet-init"
;;   (pallet-test-with-sandbox
;;    (package-refresh-contents)
;;    (package-install 'package-one)
;;    (should (not (f-exists? (pallet--cask-file))))
;;    (pallet-mode t)
;;    (pallet-init)
;;    (should (f-exists? (pallet--cask-file)))))


;; ;; installing and updating packages

;; (ert-deftest pallet-test-install ()
;;   "it installs packages from the Cask file"
;;   (pallet-test-with-sandbox
;;    (pallet-test-create-cask-file-with-servant "(depends-on \"package-one\")")
;;    (should (not (package-installed-p 'package-one)))
;;    (pallet-mode t)
;;    (pallet-install)
;;    (should (package-installed-p 'package-one))))

(ert-deftest pallet-test-update ()
  "it updates packages in the Cask file"
  (pallet-test-with-sandbox
   (package-refresh-contents)
   (package-install 'package-two)
   (pallet-mode t)
   (pallet-init)
   (pallet-update)
   (package-initialize)
   (should (package-installed-p 'package-two '(0 0 2)))))

;; advising package.el functions to add to and delete from the Cask file

;; (ert-deftest pallet-test-pack-on-install ()
;;   "it adds a package to the Cask file on package-install"
;;   (pallet-test-with-sandbox
;;    (pallet-mode t)
;;    (pallet-init)
;;    (should (not (s-contains? "(depends-on \"package-one\")"
;;                              (f-read-text (pallet--cask-file)))))
;;    (package-refresh-contents)
;;    (package-install 'package-one)
;;    (should (s-contains? "(depends-on \"package-one\")"
;;                         (f-read-text (pallet--cask-file))))))

;; (ert-deftest pallet-test-unpack-on-delete ()
;;   "it removes a package from the Cask file on package-delete"
;;   (pallet-test-with-sandbox
;;    (package-refresh-contents)
;;    (package-install 'package-one)
;;    (package-install 'package-two)
;;    (pallet-mode t)
;;    (pallet-init)
;;    (should (s-contains? "(depends-on \"package-one\")"
;;                         (f-read-text (pallet--cask-file))))
;;    (should (s-contains? "(depends-on \"package-two\")"
;;                         (f-read-text (pallet--cask-file))))
;;    (pallet-test-do-package-delete "package-one" '(0 0 1))
;;    (should (s-contains? "(depends-on \"package-two\")"
;;                         (f-read-text (pallet--cask-file))))
;;    (should (not (s-contains? "(depends-on \"package-one\")"
;;                              (f-read-text (pallet--cask-file)))))))


;; ;; handling 24.3.1 and >= 24.3.5 package.el systems

;; (ert-deftest pallet-test-package-name-symbol ()
;;   "it handles a package name as a symbol"
;;   (should (string= (pallet--package-name 'package-one) "package-one")))

;; (if (fboundp 'package-desc-create)
;;     (ert-deftest pallet-test-package-name-desc ()
;;       "it handles a package name as a package-desc"
;;       (let ((desc (package-desc-create :name "package-one" :version "0.0.1")))
;;         (should (string= (pallet--package-name desc) "package-one")))))

;; (ert-deftest pallet-test-package-name-string ()
;;   "it handles a package name as a string"
;;   (should (string= (pallet--package-name "package-one") "package-one")))

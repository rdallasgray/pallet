;; initialising Cask

(ert-deftest pallet-test-cask-up-on-load ()
  "it should initialize Cask on load"
  (with-mock
   (mock (cask-initialize))
   (run-hooks 'after-init-hook)))

(ert-deftest pallet-test-init ()
  "it should write a Cask file on pallet-init"
  (pallet-test-with-sandbox
   (package-install 'package-one)
   (pallet-init)
   (should (s-contains? "(depends-on \"package-one\")"
                         (f-read-text (pallet--cask-file))))))


;; installing and updating packages

(ert-deftest pallet-test-install ()
  "it should install packages from the Cask file"
  (pallet-test-with-sandbox
   (when (package-installed-p 'package-one)
     (pallet-test-do-package-delete "package-one" '(0 0 1)))
   (pallet-test-create-cask-file-with-servant
    "(depends-on \"package-one\")")
   (pallet-install)
   (should (package-installed-p 'package-one))))

;; (ert-deftest pallet-test-update ()
;;   "it should update packages in the Cask file without deleting them"
;;   (pallet-test-with-sandbox
;;    (pallet-test-create-cask-file
;;     "(source \"servant\" \"http://127.0.0.1:9191/packages/\")(depends-on \"package-two\")")
;;    (package-install "package-two-0.0.1.el")
;;    (pallet-update)
;;    (should (package-installed-p 'package-two '(0 0 2)))
;;    (should (s-contains? "(depends-on \"package-one\")"
;;                         (f-read-text (pallet--cask-file))))))

;; ;; advising package.el functions to add to and delete from the Cask file

;; (ert-deftest pallet-test-pack-on-install ()
;;   "it should add a package to the Cask file on package-install"
;;   (pallet-test-with-sandbox
;;    (when (package-installed-p 'package-one)
;;      (pallet-test-do-package-delete "package-one"))
;;    (pallet-test-create-cask-file
;;     "(source \"servant\" \"http://127.0.0.1:9191/packages/\")")
;;    (pallet-init)
;;    (package-install 'package-one)
;;    (should (s-contains? "(depends-on \"package-one\")"
;;                         (f-read-text (pallet--cask-file))))))

(ert-deftest pallet-test-unpack-on-delete ()
  "it should remove a package from the Cask file on package-delete"
  (pallet-test-with-sandbox
   (package-install 'package-one)
   (package-install 'package-two)
   (pallet-init)
   (pallet-test-do-package-delete "package-one" '(0 0 1))
   (should (s-contains? "(depends-on \"package-two\")"
                        (f-read-text (pallet--cask-file))))
   (should (not (s-contains? "(depends-on \"package-one\")"
                             (f-read-text (pallet--cask-file)))))))


;; handling 24.3  and >= 24.3.5 package.el systems

(ert-deftest pallet-test-package-name-symbol ()
  "it should handle a package name as a symbol"
  (should (string= (pallet--package-name 'package-one) "package-one")))

(if (fboundp 'package-desc-create)
    (ert-deftest pallet-test-package-name-desc ()
      "it should handle a package name as a package-desc"
      (let ((desc (package-desc-create :name "package-one" :version "0.0.1")))
        (should (string= (pallet--package-name desc) "package-one")))))

(ert-deftest pallet-test-package-name-string ()
  "it should handle a package name as a string"
  (should (string= (pallet--package-name "package-one") "package-one")))

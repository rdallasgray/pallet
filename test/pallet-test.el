;; getting installed package names from the existing package.el setup

(ert-deftest pallet-test-pick-packages ()
  "it should pick a list of installed package names"
  (pallet-test-with-sandbox
   (package-install-file (pallet-test-package-file "package-one-0.0.1.el"))
   (package-install-file (pallet-test-package-file "package-two-0.0.1.el"))
   (should (equal (-difference (pallet--pick-packages)
                               '("package-one" "package-two")) nil))))


;; writing a correct Cask file

(ert-deftest pallet-test-write-sources ()
  "it should write Cask-formatted sources"
  (let* ((sources '(("example" . "http://example.com/packages/")
                    ("gnu" . "http://elpa.gnu.org/packages/")))
         (cask-sources (pallet--write-sources sources)))
    (should (and
             (s-contains?
              "(source gnu)" cask-sources)
             (s-contains?
              "(source \"example\" \"http://example.com/packages/\")" cask-sources)))))

(ert-deftest pallet-test-write-depends ()
  "it should write Cask-formatted dependencies"
  (let* ((depends '("package-one" "package-two"))
         (cask-depends (pallet--write-depends depends)))
    (should (and
             (s-contains?
              "(depends-on \"package-one\")" cask-depends)
             (s-contains?
              "(depends-on \"package-two\")" cask-depends)))))


;; initialising Cask

(ert-deftest pallet-test-cask-up-on-load ()
  "it should clear Cask runtime dependencies and initialize Cask on load"
  (with-mock
   (mock (cask-initialize))
   (run-hooks 'after-init-hook)
   (should (equal cask-runtime-dependencies nil))))

(ert-deftest pallet-test-init ()
  "it should write a Cask file on pallet-init"
  (pallet-test-with-sandbox
   (with-mock
    (stub pallet-install)
    (package-install-file (pallet-test-package-file "package-one-0.0.1.el"))
    (pallet-init)
    (should (s-contains? "(depends-on \"package-one\")"
                         (f-read-text (pallet--cask-file)))))))


;; advising package.el functions to add to and delete from the Cask file

(ert-deftest pallet-test-pack-on-install ()
  "it should add a package to the Cask file on package-install"
  (pallet-test-with-sandbox
   (with-mock
    (stub pallet-install)
    (pallet-init)
    (package-install-file (pallet-test-package-file "package-one-0.0.1.el"))
    (should (s-contains? "(depends-on \"package-one\")"
                         (f-read-text (pallet--cask-file)))))))

(ert-deftest pallet-test-unpack-on-delete ()
  "it should remove a package from the Cask file on package-delete"
  (pallet-test-with-sandbox
   (with-mock
    (stub pallet-install)
    (package-install-file (pallet-test-package-file "package-one-0.0.1.el"))
    (package-install-file (pallet-test-package-file "package-two-0.0.1.el"))
    (pallet-init)
    (pallet-test-do-package-delete "package-one" "0.0.1")
    (should (s-contains? "(depends-on \"package-two\")"
                         (f-read-text (pallet--cask-file))))
    (should (not (s-contains? "(depends-on \"package-one\")"
                              (f-read-text (pallet--cask-file))))))))


;; handling upgrades (which are composed of an install then a delete)

(ert-deftest pallet-test-delete-installed-package ()
  "it shouldn't unpack an installed package on delete."
  (pallet-test-with-sandbox
   (with-mock
    (stub pallet-install)
    (stub pallet--installed-p => t)
    (package-install-file (pallet-test-package-file "package-one-0.0.1.el"))
    (package-install-file (pallet-test-package-file "package-two-0.0.1.el"))
    (package-install-file (pallet-test-package-file "package-two-0.0.2.el"))
    (pallet-init)
    (pallet-test-do-package-delete "package-two" "0.0.1")
    (should (s-contains? "(depends-on \"package-one\")"
                         (f-read-text (pallet--cask-file))))
    (should (s-contains? "(depends-on \"package-two\")"
                         (f-read-text (pallet--cask-file)))))))


;; handling 24.3.1 and 24.3.5 package.el systems

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

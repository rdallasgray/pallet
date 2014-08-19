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
  "it should initialize Cask on load"
  (with-mock
   (mock (cask-initialize))
   (run-hooks 'after-init-hook)))

(ert-deftest pallet-test-init ()
  "it should write a Cask file on pallet-init"
  (pallet-test-with-sandbox
   (with-mock
    (stub pallet-install)
    (package-install-file (pallet-test-package-file "package-one-0.0.1.el"))
    (pallet-init)
    (should (s-contains? "(depends-on \"package-one\")"
                         (f-read-text (pallet--cask-file)))))))


;; installing and updating packages

(ert-deftest pallet-test-install ()
  "it should install packages from the Cask file"
  (pallet-test-with-sandbox
   (when (package-installed-p 'ack)
     (pallet-test-do-package-delete "ack"))
   (pallet-test-create-cask-file "(source gnu)(depends-on \"ack\")")
   (pallet-install)
   (should (package-installed-p 'ack))))

(ert-deftest pallet-test-update ()
  "it should update packages in the Cask file without deleting them"
  (pallet-test-with-sandbox
   (pallet-test-create-cask-file "(source gnu)(depends-on \"ack\")")
   (pallet-install)
   (pallet-update)
   (should (package-installed-p 'ack))
   (should (s-contains? "(depends-on \"ack\")"
                        (f-read-text (pallet--cask-file))))))

;; advising package.el functions to add to and delete from the Cask file

(ert-deftest pallet-test-pack-on-install ()
  "it should add a package to the Cask file on package-install"
  (pallet-test-with-sandbox
   (with-mock
    (pallet-test-create-cask-file "(source gnu)")
    (pallet-init)
    (package-install 'ack)
    (should (s-contains? "(depends-on \"ack\")"
                         (f-read-text (pallet--cask-file)))))))

(ert-deftest pallet-test-no-pack-on-duplicate-install ()
  "it should not add a duplicate package to the Cask file on package-install"
  (pallet-test-with-sandbox
   (with-mock
    (pallet-test-create-cask-file "(source gnu)")
    (pallet-init)
    (package-install 'ack)
    (package-install 'ack)
    (should-not (s-matches? "\\((depends-on \"ack\")\\)\\(.\\|\n\\)*\\1"
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

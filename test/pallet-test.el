(ert-deftest test/init ()
  "it writes a Cask file on pallet-init, listing installed packages"
  (test/with-sandbox
   (test/add-servant-package '(package-one (0 0 1)))
   (package-refresh-contents)
   (package-install 'package-one)
   (should (not (f-exists? test/cask-file)))
   (pallet-mode t)
   (pallet-init)
   (should (f-exists? test/cask-file))
   (should (test/cask-file-contains-p "(depends-on \"package-one\")"))))

(ert-deftest test/install ()
  "it installs packages from the Cask file"
  (test/with-sandbox
   (test/add-servant-package '(package-one (0 0 1)))
   (test/create-cask-file-with-servant "(depends-on \"package-one\")")
   (should (not (package-installed-p 'package-one)))
   (pallet-mode t)
   (pallet-install)
   (should (package-installed-p 'package-one))))

(ert-deftest test/update ()
  "it updates packages in the Cask file"
  (test/with-sandbox
   (test/add-servant-package '(package-two (0 0 1)))
   (test/create-cask-file-with-servant "(depends-on \"package-two\")")
   (pallet-mode t)
   (pallet-install)
   (should (package-installed-p 'package-two '(0 0 1)))
   (test/add-servant-package '(package-two (0 0 2)))
   (pallet-update)
   (package-initialize)
   (should (package-installed-p 'package-two '(0 0 2)))))

(ert-deftest test/update-no-delete ()
  "when updating, it doesn't delete existing packages from the Cask file"
  (test/with-sandbox
   (test/add-servant-package '(package-two (0 0 1)))
   (test/create-cask-file-with-servant "(depends-on \"package-two\")")
   (pallet-mode t)
   (pallet-install)
   (test/add-servant-package '(package-two (0 0 2)))
   (pallet-update)
   (should (test/cask-file-contains-p "(depends-on \"package-two\")"))))

(ert-deftest test/pack-on-install ()
  "it adds a package to the Cask file on package-install"
  (test/with-sandbox
   (test/add-servant-package '(package-one (0 0 1)))
   (pallet-mode t)
   (pallet-init)
   (should (not (test/cask-file-contains-p "(depends-on \"package-one\")")))
   (package-refresh-contents)
   (package-install 'package-one)
   (should (test/cask-file-contains-p "(depends-on \"package-one\")"))))

(ert-deftest test/pack-on-install-unique ()
  "it doesn't create duplicate entries on repeated installs"
  (test/with-sandbox
   (test/add-servant-package '(package-two (0 0 1)))
   (pallet-mode t)
   (pallet-init)
   (package-refresh-contents)
   (package-install 'package-two)
   (test/add-servant-package '(package-two (0 0 2)))
   (package-refresh-contents)
   (package-install 'package-two)
   (should (eq (s-count-matches "package-two" (f-read test/cask-file)) 1))))

(ert-deftest test/ignored-text ()
  "it ignores text below a magic comment"
  (test/with-sandbox
   (test/create-cask-file-with-servant ";;;pallet-ignore\n;;ignored text")
   (test/add-servant-package '(package-one (0 0 1)))
   (pallet-mode t)
   (pallet-init)
   (package-refresh-contents)
   (package-install 'package-one)
   (should (test/cask-file-contains-p ";;ignored text"))))

(ert-deftest test/no-ignored-text ()
  "it doesn't insert an ignored text comment if there is no ignored text"
  (test/with-sandbox
   (test/create-cask-file-with-servant "")
   (test/add-servant-package '(package-one (0 0 1)))
   (pallet-mode t)
   (pallet-init)
   (package-refresh-contents)
   (package-install 'package-one)
   (should-not (test/cask-file-contains-p ";;;pallet-ignore"))))

(ert-deftest test/pack-on-install-desc ()
  "it responds correctly to package-install when the argument is a package-desc"
  (when (fboundp 'package-desc-create)
    (test/with-sandbox
     (test/add-servant-package '(package-one (0 0 1)))
     (pallet-mode t)
     (pallet-init)
     (should (not (test/cask-file-contains-p "(depends-on \"package-one\")")))
     (package-refresh-contents)
     (package-install (test/package-desc-create '(package-one (0 0 1))))
     (should (test/cask-file-contains-p "(depends-on \"package-one\")")))))

(ert-deftest test/unpack-on-delete ()
  "it removes a package from the Cask file on package-delete"
  (test/with-sandbox
   (test/add-servant-package '(package-one (0 0 1)))
   (test/add-servant-package '(package-two (0 0 1)))
   (package-refresh-contents)
   (package-install 'package-one)
   (package-install 'package-two)
   (pallet-mode t)
   (pallet-init)
   (should (test/cask-file-contains-p "(depends-on \"package-one\")"))
   (should (test/cask-file-contains-p "(depends-on \"package-two\")"))
   (test/package-delete '(package-one (0 0 1)))
   (should (test/cask-file-contains-p "(depends-on \"package-two\")"))
   (should (not (test/cask-file-contains-p "(depends-on \"package-one\")")))))

(ert-deftest test/preserves-version-metadata ()
  "it does not remove existing version metadata from the Cask file"
  (test/with-sandbox
   (test/create-cask-file-with-servant "(depends-on \"versioned-package\" \"1\")")
   (cask-initialize)
   (test/add-servant-package '(package-one (0 0 1)))
   (package-refresh-contents)
   (pallet-mode t)
   (package-install 'package-one)
   (should (test/cask-file-contains-p "(depends-on \"package-one\")"))
   (should (test/cask-file-contains-p "(depends-on \"versioned-package\" \"1\")"))
   (test/package-delete '(package-one (0 0 1)))
   (should (test/cask-file-contains-p "(depends-on \"versioned-package\" \"1\")"))
   (should (not (test/cask-file-contains-p "(depends-on \"package-one\")")))
))

(ert-deftest test/preserves-vc-metadata ()
  "it does not remove existing VC dependency metadata from the Cask file"
  (test/with-sandbox
   (let ((vc-dependency "(depends-on \"vc-package\" :git \"url\" :ref \"abcdef\" :branch \"feature\" :files (\"pattern1\" (\"target\" (\"source\" \"pattern2\"))))"))
     (test/create-cask-file-with-servant vc-dependency)
     (cask-initialize)
     (test/add-servant-package '(package-one (0 0 1)))
     (package-refresh-contents)
     (pallet-mode t)
     (package-install 'package-one)
     (should (test/cask-file-contains-p "(depends-on \"package-one\")"))
     (should (test/cask-file-contains-p vc-dependency))
     (test/package-delete '(package-one (0 0 1)))
     (should (test/cask-file-contains-p vc-dependency))
     (should (not (test/cask-file-contains-p "(depends-on \"package-one\")"))))))

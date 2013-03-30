PROJECT=Pallet
SRC_DIR=src
BUILD_DIR=lib
TMP_DIR=tmp

TAG=$(shell git describe --abbrev=0)
REV=$(shell git describe --long | sed -E 's/([^\-]*)\-([0-9]+).*/\2/')
VERSION=$(TAG).$(REV)
YEAR=$(shell date +"%Y")
VERSIONED_FILES=src-pkg.el pallet.el
COMMENTARY_FILE=README.md

all: rename-package

# readme, melpa recipe, test

setup: clean
	@echo "Copying src to tmp"
	@`cp -R ${SRC_DIR} ${TMP_DIR}`

clean:
	@echo "Removing tmp"
	@`rm -Rf tmp`

version: setup carton
	@for FILE in $(VERSIONED_FILES); do \
	echo "Setting version number ${VERSION} and year ${YEAR} in ${TMP_DIR}/$$FILE"; \
	sed -i '' -e 's/@VERSION/${VERSION}/g' ${TMP_DIR}/$$FILE; \
	sed -i '' -e 's/@YEAR/${YEAR}/g' ${TMP_DIR}/$$FILE; \
	done

carton:
	@echo "Creating pkg file"
	@`cd src && carton package`

rename-package: setup carton version
	@echo "Renaming lib/src-pkg.el to lib/pallet-pkg.el"
	@`mv lib/src-pkg.el lib/pallet-pkg.el`

commentary: setup
	@echo "Inserting commentary"
	@sed 's/^/;; /' < ${COMMENTARY_FILE} > ${TMP_DIR}/commentary
	@sed -i '' -e '/@COMMENTARY/r ${TMP_DIR}/commentary' -e '//d' ${TMP_DIR}/pallet.el

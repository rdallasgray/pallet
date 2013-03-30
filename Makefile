PROJECT=Pallet
SRC_DIR=src
BUILD_DIR=lib

TAG=$(shell git describe --abbrev=0)
REV=$(shell git describe --long | sed -E 's/([^\-]*)\-([0-9]+).*/\2/')
VERSION=$(TAG).$(REV)
YEAR=$(shell date +"%Y")
VERSIONED_FILES=src-pkg.el pallet.el
CLEAN_FILES=src/pkg.el lib/*

all: clean rename-package

# readme, melpa recipe, test

clean:
	@for FILE in $(CLEAN_FILES); do \
	echo "Removing $$FILE"; \
	`rm $$FILE`; \
	done

version: carton
	@for FILE in $(VERSIONED_FILES); do \
	echo "Setting version number ${VERSION} and year ${YEAR} in $$FILE"; \
	sed 's/@VERSION/${VERSION}/' < ${SRC_DIR}/$$FILE | sed 's/@YEAR/${YEAR}/' > ${BUILD_DIR}/$$FILE; \
	done

carton:
	@echo "Creating pkg file"
	@`cd src && carton package`

rename-package: carton version
	@`mv lib/src-pkg.el lib/pallet-pkg.el`

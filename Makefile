PROJECT=Pallet
PROJECT_L=pallet
SRC_DIR=src
BUILD_DIR=lib

VERSION=$(shell git describe --tags --long | sed s/\-/\./)
YEAR=$(shell date +"%Y")
VERSIONED_FILES=src-pkg.el pallet.el
CLEAN_FILES=src/pkg.el lib/*

all: clean rename-package

# carton, readme, melpa recipe

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

PROJECT=Pallet
SRC_DIR=src
BUILD_DIR=lib

VERSION=$(shell git describe --tags --long | sed s/\-/\./)
YEAR=$(shell date +"%Y")
VERSIONED_FILES=pallet-pkg.el pallet.el

all: version

# carton, readme, melpa recipe

version:
	@for FILE in $(VERSIONED_FILES); do \
	echo "Setting version number ${VERSION} and year ${YEAR} in $$FILE"; \
	sed 's/@VERSION/${VERSION}/' < ${SRC_DIR}/$$FILE | sed 's/@YEAR/${YEAR}/' > ${BUILD_DIR}/$$FILE; \
	done



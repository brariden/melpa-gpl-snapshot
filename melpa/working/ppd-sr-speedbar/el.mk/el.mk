TMP_DIR=tmp
LIB_DIR=lib
DIST_DIR=dist
SRC_FILES=${shell ls *.el} Cask

ifeq (${VERSION},)
VERSION=${shell git fetch --tags && git describe --abbrev=0}
endif
YEAR=${shell date +"%Y"}
COMMENTARY_FILE=README.md
TEST_FILE=test/${PROJECT_LCNAME}-test-main.el

all: build-clean

.PHONY : tmp clean error version pkg-file commentary build

tmp:
	@make clean
	@echo "Creating ${TMP_DIR}"
	@mkdir ${TMP_DIR}
	@echo "Copying src files to tmp"
	@cp ${SRC_FILES} ${TMP_DIR}/

clean:
	@echo "Removing tmp"
	@rm -Rf tmp || true

clean-lib:
	@rm ${LIB_DIR}/* || true

build-clean: build clean

error:
	@make clean
	@exit 1

version: tmp
	@for FILE in ${shell ls ${TMP_DIR}}; do \
	echo "Setting version number ${VERSION} and year ${YEAR} in $$FILE"; \
	sed -e 's/@VERSION/${VERSION}/g' -e 's/@YEAR/${YEAR}/g' $$FILE > ${TMP_DIR}/$$FILE.versioned; \
	mv ${TMP_DIR}/$$FILE.versioned ${TMP_DIR}/$$FILE; \
	done

pkg-file: version commentary
	@echo "Creating pkg file"
	@cask --path ${TMP_DIR} pkg-file

dist: build
	@echo "Creating tar file"
	@cask --path ${TMP_DIR} package

commentary: tmp
	@echo "Inserting commentary"
	@sed 's/^/;; /' ${COMMENTARY_FILE} > ${TMP_DIR}/commentary
	@sed -e '/@COMMENTARY/r ${TMP_DIR}/commentary' -e '//d' \
	${TMP_DIR}/${PROJECT_LCNAME}.el > ${TMP_DIR}/${PROJECT_LCNAME}.commented.el
	@mv ${TMP_DIR}/${PROJECT_LCNAME}.commented.el ${TMP_DIR}/${PROJECT_LCNAME}.el
	@rm ${TMP_DIR}/commentary

build: pkg-file clean-lib
	@cp tmp/*.el lib/

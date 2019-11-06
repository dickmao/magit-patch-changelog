EMACS ?= $(shell which emacs)
SRC = $(shell cask files)
PKBUILD = 2.3
ELCFILES = $(SRC:.el=.elc)
CASK = $(shell cask package-directory)

ifeq ($(TRAVIS_PULL_REQUEST_BRANCH),)
TRAVIS_PULL_REQUEST_BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
endif
ifeq ($(TRAVIS_PULL_REQUEST_SLUG),)
ifeq ($(TRAVIS_PULL_REQUEST_BRANCH),HEAD)
TRAVIS_PULL_REQUEST_SLUG := $(TRAVIS_REPO_SLUG)
else
TRAVIS_PULL_REQUEST_SLUG := $(shell git config --global user.name)/$(shell basename `git rev-parse --show-toplevel`)
endif
endif
ifeq ($(TRAVIS_PULL_REQUEST_SHA),)
TRAVIS_PULL_REQUEST_SHA := $(shell git rev-parse origin/$(TRAVIS_PULL_REQUEST_BRANCH) 2>/dev/null)
endif

.DEFAULT_GOAL := test-compile

README.rst: README.in.rst magit-patch-changelog.el
	sed "/CI VERSION/c"`grep -o 'emacs-[0-9][.0-9]*' .travis.yml | sort -n | head -1 | grep -o '[.0-9]*'` README.in.rst > README.rst0
	grep ';;' magit-patch-changelog.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.rst0 > README.rst
	rm -f README.rst0 README.rst1

.PHONY: test-install
test-install:
	mkdir -p test/test-install
	if [ ! -s "test/test-install/$(PKBUILD).tar.gz" ] ; then \
	  cd test/test-install ; curl -sLOk https://github.com/melpa/package-build/archive/$(PKBUILD).tar.gz ; fi
	cd test/test-install ; tar xfz $(PKBUILD).tar.gz
	cd test/test-install ; rm -f $(PKBUILD).tar.gz
	cd test/test-install/package-build-$(PKBUILD) ; make -s loaddefs
	mkdir -p test/test-install/recipes
	cd test/test-install/recipes ; curl -sfLOk https://raw.githubusercontent.com/melpa/melpa/master/recipes/magit-patch-changelog || cp -f ../../../tools/recipe ./magit-patch-changelog
	! ( $(EMACS) -Q --batch -L test/test-install/package-build-$(PKBUILD) \
	--eval "(require 'package-build)" \
	--eval "(require 'subr-x)" \
	--eval "(package-initialize)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-refresh-contents)" \
	--eval "(setq rcp (package-recipe-lookup \"magit-patch-changelog\"))" \
	--eval "(unless (file-exists-p package-build-archive-dir) \
	           (make-directory package-build-archive-dir))" \
	--eval "(let* ((my-repo \"$(TRAVIS_PULL_REQUEST_SLUG)\") \
	               (my-branch \"$(TRAVIS_PULL_REQUEST_BRANCH)\") \
	               (my-commit \"$(TRAVIS_PULL_REQUEST_SHA)\")) \
	           (oset rcp :repo my-repo) \
	           (oset rcp :branch my-branch) \
	           (oset rcp :commit my-commit))" \
	--eval "(package-build--package rcp (package-build--checkout rcp))" \
	--eval "(package-install-file (car (file-expand-wildcards (concat package-build-archive-dir \"magit-patch-changelog*.el\"))))" 2>&1 | egrep -ia "error: |fatal" )

$(CASK):
	cask install

.PHONY: test-compile
test-compile: $(CASK)
	sh -ex tools/package-lint.sh $(SRC)
	! (cask eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: test-unit
test-unit: $(CASK)
	cask exec ert-runner -L ./lisp -L . -L test test/magit-patch-changelog-tests.el

.PHONY: test
test: test-compile test-unit test-int

.PHONY: test-int
test-int: $(CASK)
	cask exec ecukes --reporter magnars

.PHONY: clean
clean:
	cask clean-elc
	rm -rf test/test-install

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	cask package

.PHONY: install
install: test-compile dist
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/magit-patch-changelog*.el\")))"

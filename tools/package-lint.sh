#!/bin/sh -ex

EMACS="${EMACS:=emacs}"
BASENAME=$(basename "$1")

!( cask emacs -Q --batch \
           --visit "$1" \
           --eval "(checkdoc-eval-current-buffer)" \
           --eval "(princ (with-current-buffer checkdoc-diagnostic-buffer \
                                               (buffer-string)))" \
           2>&1 | egrep -a "^$BASENAME:" | egrep -v "Messages should start" | grep "." )

cask emacs -Q --batch \
           -l package-lint \
           --eval "(package-initialize)" \
           --eval "(push (quote (\"melpa\" . \"http://melpa.org/packages/\")) \
                         package-archives)" \
           --eval "(package-refresh-contents)" \
           --eval "(defconst package-lint--sane-prefixes \
                     (rx \
                      string-start \
                      (or \
                       \"org-dblock-write:\" \
                       \"string-trim-left\" \
                       \"org-babel-execute:\" \
                       \"org-babel-prep-session:\" \
                       \"org-babel-variable-assignments:\" \
                       \"org-babel-default-header-args:\" \
                       \"pcomplete/\")))" \
           -f package-lint-batch-and-exit "$1"

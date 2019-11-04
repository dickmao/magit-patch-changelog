;;; env.el --- env.el -*- lexical-binding: t; coding: utf-8 -*-

(custom-set-variables
 '(magit-commit-ask-to-stage nil))

(require 'f)

(defsubst support-path ()
  (f-dirname load-file-name))

(defsubst root-path ()
  (f-parent (f-parent (support-path))))

(add-to-list 'load-path (concat (root-path) "/lisp"))
(add-to-list 'load-path (concat (root-path) "/test"))

(require 'magit-patch-changelog-testing)
(require 'magit-patch-changelog)


(defun cleanup ()
  )

(defvar test-directory)

(Setup
  (push "GIT_AUTHOR_NAME=A U Thor" process-environment)
  (push "GIT_AUTHOR_EMAIL=a.u.thor@example.com" process-environment)
  ;; cannot get global-magit-file-mode to work, maybe non-noninteractive only
  (global-set-key "\C-xg" 'magit-status))

(Before
 (setq test-directory (file-name-as-directory (make-temp-file "magit-" t)))
 (setq default-directory test-directory)
 (magit-git "init" ".")
 (f-touch "./file")
 (magit-git "add" "./file")
 (apply #'magit-git (split-string "commit -m init --allow-empty"))
 (should (magit-file-tracked-p "file")))

(After
 (delete-directory test-directory t)
 (setq default-directory (root-path))
 )

(Teardown
 (setq default-directory (root-path))
 (cleanup)
)

(Fail
 (if noninteractive
     (with-demoted-errors "demote: %s"
       (Teardown))
   (backtrace)
   (keyboard-quit))) ;; useful to prevent emacs from quitting

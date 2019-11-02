;;; magit-patch-changelog.el --- Git format-patch for CONTRIBUTING -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 The Authors of magit-patch-changelog.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0.1.0
;; Keywords: git tools vc
;; URL: https://github.com/dickmao/magit-patch-changelog
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nnhackernews.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate a patch suitable according to emacs-mirror/CONTRIBUTE.

;;; Code:

(require 'magit)

(defun magit-patch-changelog-add-log-insert (buffer file defun)
  "As `magit-commit-add-log-insert', and set text properties to xref diffs.

Write to BUFFER the ChangeLog entry \"* FILE (DEFUN):\"."
  (with-current-buffer (magit-get-mode-buffer 'magit-diff-mode)
    (when (stringp defun)
      (put-text-property 0 (length defun)
                         'magit-patch-changelog-loc (cons (current-buffer) (point))
                         defun)))
  (with-current-buffer buffer
    (undo-boundary)
    (goto-char (point-max))
    (while (re-search-backward (concat "^" comment-start) nil t))
    (save-restriction
      (narrow-to-region (point-min) (point))
      (cond ((re-search-backward (format "* %s\\(?: (\\([^)]+\\))\\)?: " file)
                                 nil t)
             (when (equal (match-string 1) defun)
               (setq defun nil))
             (re-search-forward ": "))
            (t
             (when (re-search-backward "^[\\*(].+\n" nil t)
               (goto-char (match-end 0)))
             (while (re-search-forward "^[^\\*\n].*\n" nil t))
             (let ((changelog-header (format "* %s " file)))
               (put-text-property 0 (length changelog-header)
                                  'magit-patch-changelog-header t
                                  changelog-header)
               (if defun
                   (progn (insert (format "%s(%s): \n" changelog-header defun))
                          (setq defun nil))
                 (insert (format "%s: \n" changelog-header))))
             (backward-char)
             (unless (looking-at "\n[\n\\']")
               (insert ?\n)
               (backward-char))))
      (when defun
        (forward-line)
        (let ((limit (save-excursion
                       (and (re-search-forward "^\\*" nil t)
                            (point)))))
          (unless (or (looking-back (format "(%s): " defun)
                                    (line-beginning-position))
                      (re-search-forward (format "^(%s): " defun) limit t))
            (while (re-search-forward "^[^\\*\n].*\n" limit t))
            (insert (format "(%s): \n" defun))
            (backward-char)))))))

(easy-mmode-defmap magit-patch-changelog-mode-map
                   '(("\M-." . magit-patch-changelog-xref)
                     ([M-down] . magit-patch-changelog-agg-down)
                     ([M-up] . magit-patch-changelog-agg-up))
                   "Keymap for the `magit-patch-changelog-mode'."
                   :group 'magit-patch)

(defsubst magit-patch-changelog--single-property-change (x direction limit)
  "`previous-single-property-change' is off-by-one coming and going.

Position point before character differing in 'magit-patch-changelog-loc of X
in direction DIRECTION up to LIMIT."
  (let ((result
         (funcall (if (< direction 0)
                      #'previous-single-property-change
                    #'next-single-property-change)
                  (if (< direction 0) (min (1+ x) (point-max)) x)
                  'magit-patch-changelog-loc
                  nil limit)))
    (if (and result (< direction 0))
        (max (1- result) (point-min) limit)
      result)))

(defun magit-patch-changelog--goto-ref (direction &optional limit)
  "Move point to next ChangeLog ref in DIRECTION up to LIMIT."
  (unless limit
    (setq limit (funcall (if (< direction 0)
                             #'previous-single-property-change
                           #'next-single-property-change)
                         (point) 'magit-patch-changelog-header)))
  (cl-block nil
    (let* ((orig (point))
           (on-ref-func (lambda (x) (get-text-property
                                     x 'magit-patch-changelog-loc)))
           (on-ref (funcall on-ref-func (point)))
           (change-p (lambda (x)
                       (and x (not (eq x limit)))))
           (next-change-func (lambda (x)
                               (magit-patch-changelog--single-property-change
                                x direction limit))))
      (when on-ref
        (let ((nspc (funcall next-change-func (point))))
          (when (funcall change-p nspc)
            (goto-char nspc)
            (when (funcall on-ref-func nspc)
              (cl-return nspc)))))

      (let ((nspc (funcall next-change-func (point))))
        (if (funcall change-p nspc)
            (goto-char nspc)
          (goto-char orig)
          nil)))))

(defun magit-patch-changelog--fixline (&optional triggering)
  "Patch up ChangeLog entry on current line.  Move point to TRIGGERING ref.

Returns nil if deleted line, t otherwise."
  (cl-block nil
    (save-excursion
      (beginning-of-line)
      (let* ((header-start (text-property-any
                            (point) (line-end-position)
                            'magit-patch-changelog-header t))
             (header-end (and header-start
                              (text-property-any
                               header-start (line-end-position)
                               'magit-patch-changelog-header nil)))
             (changelog-header (and header-start header-end
                                    (buffer-substring header-start header-end)))
             (line-end (line-end-position))
             (line-beg (line-beginning-position))
             changelog-refs
             next
             commentary)
        (save-excursion
          (unless (bobp) (backward-char))
          (while (setq next (magit-patch-changelog--goto-ref 1 line-end))
            (push (buffer-substring
                   next
                   (or (next-single-property-change
                        next 'magit-patch-changelog-loc)
                       (point-max)))
                  changelog-refs)))
        (save-excursion
          (end-of-line)
          (unless (eobp) (forward-char))
          (let ((begin (previous-single-property-change
                        (point) 'magit-patch-changelog-loc
                        nil line-beg)))
            (setq commentary (string-trim-left
                              (buffer-substring begin line-end)
                              "[(,): ]+"))))
        (setq changelog-refs (nreverse changelog-refs))
        (kill-region line-beg (min (1+ line-end) (point-max)))
        (when changelog-header
          (insert changelog-header))
        (when changelog-refs
          (insert (format "(%s): " (mapconcat #'identity changelog-refs ", "))))
        (unless (zerop (length commentary))
          (insert commentary))
        (if (bolp)
            (cl-return nil)
          (insert "\n"))))
    (when triggering
      (when-let ((goto (text-property-any (line-beginning-position)
                                          (line-end-position)
                                          'magit-patch-changelog-loc
                                          (get-text-property
                                           0 'magit-patch-changelog-loc
                                           triggering))))
        (goto-char goto)))
    t))

(defsubst magit-patch-changelog-agg-up ()
  "Slurp ref upwards.

Move (foo, >b< ar) to (bar, foo).
Move (>f< oo, bar) to (foo)\n(bar)."
  (interactive)
  (magit-patch-changelog--agg -1))

(defsubst magit-patch-changelog-agg-down ()
  "Barf ref downwards.

Move (>f< oo, bar) to (bar, foo).
Move (foo, >b< ar) to (foo)\n(bar)."
  (interactive)
  (magit-patch-changelog--agg 1))

(defun magit-patch-changelog--agg (direction)
  "DIRECTION is -1 for up, and +1 for down."
  (if (get-text-property (point) 'magit-patch-changelog-loc)
      (let ((changelog-header-start (text-property-any (line-beginning-position)
                                                       (line-end-position)
                                                       'magit-patch-changelog-header
                                                       t))
            (changelog-ref (thing-at-point 'symbol))
            (bounds (bounds-of-thing-at-point 'symbol))
            (next-line-p (if (< direction 0)
                             (lambda (x) (< x (line-beginning-position)))
                           (lambda (x) (> x (line-end-position)))))
            (insert-func (lambda (x) (if (< direction 0)
                                         (progn (forward-char)
                                                (insert (format " %s " x)))
                                       (insert (format " %s " x))))))
        (apply #'kill-region (list (car bounds) (cdr bounds)))
        (let ((next (save-excursion
                      (magit-patch-changelog--goto-ref direction))))
          (cond ((not next)
                 (cond ((and (< direction 0) changelog-header-start)
                        (goto-char (text-property-any changelog-header-start
                                                      (line-end-position)
                                                      'magit-patch-changelog-header
                                                      nil))
                        (funcall insert-func changelog-ref)
                        (magit-patch-changelog--fixline changelog-ref))
                       ((not (magit-patch-changelog--fixline))
                        (beginning-of-line)
                        (insert "\n")
                        (backward-char (if (< direction 0) 2 1))
                        (funcall insert-func changelog-ref)
                        (magit-patch-changelog--fixline changelog-ref))
                       (t
                        (if (< direction 0)
                            (progn
                              (beginning-of-line)
                              (insert "\n")
                              (backward-char 2))
                          (end-of-line)
                          (insert "\n"))
                        (funcall insert-func changelog-ref)
                        (magit-patch-changelog--fixline changelog-ref))))
                ((funcall next-line-p next)
                 (set-mark (point))
                 (goto-char next)
                 (funcall insert-func changelog-ref)
                 (let ((goback (prog1 (mark t) (pop-mark))))
                   (set-mark (point))
                   (goto-char goback))
                 (magit-patch-changelog--fixline)
                 (goto-char (mark t))
                 (pop-mark)
                 (magit-patch-changelog--fixline changelog-ref))
                (t
                 (goto-char (magit-patch-changelog--single-property-change
                             next direction (if (< direction 0)
                                                (line-beginning-position)
                                              (line-end-position))))
                 (funcall insert-func changelog-ref)
                 (magit-patch-changelog--fixline changelog-ref)))))
    (message "No ChangeLog data at point")))

(defun magit-patch-changelog-xref ()
  "Jump to diff referenced by text property `magit-patch-changelog-loc'."
  (interactive)
  (if-let ((loc (get-text-property (point) 'magit-patch-changelog-loc)))
      (cl-destructuring-bind (buf . pos) loc
        (let ((magit-display-buffer-noselect t))
          (save-window-excursion
            (save-restriction
              (with-current-buffer buf
                (goto-char pos))))))
    (message "No ChangeLog data at point")))

(define-derived-mode magit-patch-changelog-mode text-mode "ChangeLog Edit"
  "Major mode manipulating parenthesized ChangeLog function references.

\\{magit-patch-changelog-mode-map}")

;;; _
(provide 'magit-patch-changelog)
;;; magit-patch-changelog.el ends here

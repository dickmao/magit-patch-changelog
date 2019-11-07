;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(foobar '("a" "b"))

(require 'dash)

(defun foobar (&rest args)
  (mapconcat #'identity (-flatten args) " "))

(defun magit-commit-add-log-insert (buffer file defun)
  (with-current-buffer buffer
    (if defun
        (progn (insert (format "\n* %s (%s): \n" file defun))
               (setq defun nil))
      (insert (format "\n* %s: \n" file)))
    (backward-char)
    (unless (looking-at "\n[\n\\']")
      (insert ?\n)
      (backward-char))))

(defsubst magit-patch--forward-to-hunk ()
  (cl-loop until (or (magit-section-match 'hunk)
                     (condition-case nil
                         (magit-section-forward)
                       (user-error t)))
           finally return (magit-section-match 'hunk)))

(dolist (defun '("faggot-fag-fag" "vittles" "bagpipe-joe-123") (magit-patch-changelog--fixline))
  (put-text-property 0 (length defun)
                     'magit-patch-changelog-loc (cons (current-buffer) (length defun))
                     defun)
  (insert defun " "))

(let ((header "* this is a header "))
  (put-text-property 0 (length header) 'magit-patch-changelog-header t header)
  (insert header))

* this is a header this is a comment
(vittles, faggot-fag-fag, bagpipe-joe-123, bagpipe-joe-123): this is a comment
wljs dflj sdf sldf jaf booya


(transient--parse-child 'magit-patch-create
                        '("e" "Create patches for Emacs" magit-patch-create-emacs))
(print-out (get 'magit-patch-create 'transient--layout))

(with-current-buffer (magit-get-mode-buffer 'magit-process-mode)
  (magit-patch-changelog--single-property-change 'font-lock-face (point) 1 (point-max)))



(defun gnus-instantiate-server-buffer (name)
  (let ((buffer (generate-new-buffer (format " *gnus-thread %s*" name))))
    (nnheader-prep-server-buffer buffer)
    buffer))

(defmacro gnus-get-unread-articles-pass-preceding (f args)
  "Tack preceding return value to ARGS before applying F."
  `(apply ,f (nconc ,args (list (and (boundp 'gnus-run-thread--subresult)
                                     gnus-run-thread--subresult)))))

(defun gnus-thread-body (thread-name mtx working fns)
  (with-mutex mtx
    (nnheader-message 9 "gnus-thread-body: start %s" thread-name)
    (let (gnus-run-thread--subresult
          current-fn
          (nntp-server-buffer working))
      (condition-case err
          (dolist (fn fns)
            (setq current-fn fn)
            (setq gnus-run-thread--subresult (funcall fn)))
        (error (nnheader-message
                4 "gnus-thread-body: '%s' in %S"
                (error-message-string err) current-fn))))
    (kill-buffer working)
    (nnheader-message 9 "gnus-thread-body: finish %s" thread-name)))

(defun gnus-run-thread (mtx thread-group &rest fns)
  "MTX, if non-nil, is the mutex for the new thread.
THREAD-GROUP is string useful for naming working buffer and threads.
All FNS must finish before MTX is released."
  (when fns
    (let ((thread-name
           (concat thread-group "-"
                   (let* ((max-len 160)
                          (full-name (pp-to-string (car fns)))
                          (short-name (cl-subseq
                                       full-name 0
                                       (min max-len
                                            (length full-name)))))
                     (if (> (length full-name) (length short-name))
                         (concat short-name "...")
                       short-name)))))
      (make-thread (apply-partially
                    #'gnus-thread-body
                    thread-name mtx
                    (gnus-instantiate-server-buffer thread-group)
                    fns)
                   thread-name))))

(defvar gnus-mutex-get-unread-articles (make-mutex "gnus-mutex-get-unread-articles")
  "Updating or displaying state of unread articles are critical sections.")

(with-current-buffer "magit-diff: emacs"
  (let (previous)
    (dotimes (i 50)
      (setq previous (magit-patch-changelog-next-defun previous))
      (with-current-buffer "scratch.el" (insert (format "%s\n" previous))))))

(let ((default-directory "/home/dick/emacs"))
  (magit-get-mode-buffer 'magit-process-mode)
  )

(plist-get
 (let ((default-directory "/home/dick/emacs"))
   (with-current-buffer (magit-process-buffer t)
     (goto-char (point-max))
     (let (side-effect)
       (magit-section--backward-find
        (lambda ()
          (let ((section (magit-current-section)))
            (and (eq (oref section type) 'process)
                 (let* ((what (buffer-substring
                               (oref section start)
                               (or (oref section content)
                                   (oref section end))))
                        (commit (cl-second
                                 (split-string
                                  what
                                  (format "\\s-*%c\\s-*" magit-ellipsis)))))
                   (let (deactivate-mark)
                     (when (string-match-p "^commit" commit)
                       (setq side-effect
                             `(:content ,(buffer-substring-no-properties
                                          (or (oref section content)
                                              (oref section end))
                                          (oref section end))
                                        :ng      ,(eq 'magit-process-ng
                                                      (get-text-property
                                                       (oref section start)
                                                       'font-lock-face)))))))))))
       side-effect)))
 :content)


(cl-second (split-string "git … commit --" (format "[ %c]+" magit-ellipsis)))
(cl-second nil)

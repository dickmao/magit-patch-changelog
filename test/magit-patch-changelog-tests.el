(require 'ert)
(require 'magit-patch-changelog)

(defsubst line-matches (control)
  (string= (buffer-substring (line-beginning-position) (line-end-position))
           control))

(defmacro with-test-buffer (&rest body)
  `(with-temp-buffer
     (let ((header "* this is a header "))
       (put-text-property 0 (length header) 'magit-patch-changelog-header t header)
       (insert header))
     (dolist (func '("foo-foo-bar" "baz-bar-baz" "foo-baz-foo"))
       (put-text-property 0 (length func)
                          'magit-patch-changelog-loc
                          (cons (current-buffer)
                                (cl-loop with sum = 0
                                         for i from 0 below (length func)
                                         do (setq sum (+ sum (aref func i)))
                                         finally return sum))
                          func)
       (insert func "\n")
       (backward-char)
       (magit-patch-changelog--fixline)
       (end-of-line)
       (insert "this is a comment")
       (forward-line 1))
     ,@body))

(ert-deftest agg-limit ()
  (with-test-buffer
   (re-search-backward "foo-foo-bar")
   (magit-patch-changelog-agg-up)
   (should (line-matches "* this is a header (foo-foo-bar): this is a comment"))
   (should (eq (line-beginning-position) (point-min)))
   (re-search-forward "foo-baz-foo")
   (backward-char)
   (magit-patch-changelog-agg-down)
   (should (line-matches "(foo-baz-foo): this is a comment"))
   (should (eq (line-end-position) (1- (point-max))))
   (magit-patch-changelog-agg-up)
   (should (line-matches "(baz-bar-baz, foo-baz-foo): this is a comment"))
   (magit-patch-changelog-agg-down)
   (should (line-matches "(foo-baz-foo): "))))

(ert-deftest agg-header ()
  (with-test-buffer
    (re-search-backward "foo-foo-bar")
    (magit-patch-changelog-agg-down)
    (should (line-matches "(foo-foo-bar, baz-bar-baz): this is a comment"))
    (forward-line -1)
    (should (line-matches "* this is a header this is a comment"))))

(ert-deftest agg-intervening ()
  (with-test-buffer
    (re-search-backward "baz-bar-baz")
    (end-of-line)
    (insert "\n" "blah blah")
    (re-search-forward "foo-baz-foo")
    (backward-char)
    (magit-patch-changelog-agg-up)
    (should (line-matches "(baz-bar-baz, foo-baz-foo): this is a comment"))))

(setq debug-on-error t)

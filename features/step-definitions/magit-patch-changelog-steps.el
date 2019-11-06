(When "I wait for messages to hack \\(.*\\)$"
  (lambda (message)
    (magit-patch-changelog-test-wait-for
     (lambda ()
       (let* ((raw (symbol-value (intern message)))
              (upto-command-key
               (s-replace "\\\"" "\""
                          (substring raw 0 (cl-search "\\[" raw)))))
         ;; `substitute-command-keys' seems to depend on window sizing
         ;; which is compromised by the fmakunbound of `split-window-sensibly'
         (cl-some (lambda (x) (string-match-p (format "^%s" upto-command-key) x))
                  (-map 's-trim ecukes-message-log))))
     nil 2000 300)))

(When "I wait for messages to say \\(.*\\)$"
  (lambda (message)
    (magit-patch-changelog-test-wait-for
     (lambda ()
       (let* ((message (s-replace "\\\"" "\""
                                  (substitute-command-keys
                                   (symbol-value (intern message))))))
         (-contains? (-map 's-trim ecukes-message-log) message)))
     nil 2000 300)))

(When "I wait for messages to say \"\\(.*\\)\"$"
  (lambda (message)
    (magit-patch-changelog-test-wait-for
     (lambda ()
       (let* ((message (s-replace "\\\"" "\""
                                  (substitute-command-keys message))))
         (-contains? (-map 's-trim ecukes-message-log) message)))
     nil 2000 300)))

(When "^eval \"\\(.*\\)\"$"
  (lambda (command)
    (eval (car (read-from-string command)))))

(When "^magit-command \"\\(.*\\)\"$"
  (lambda (command)
    (let ((args (split-string command)))
      (apply (symbol-function (intern (car args))) (cdr args)))))

(When "^magit-git \"\\(.*\\)\"$"
  (lambda (command)
    (let ((args (split-string command)))
      (apply #'magit-git args))))

(When "^I dump process buffer$"
  (lambda ()
    (with-current-buffer (magit-process-buffer)
      (message (buffer-string)))))

(When "^I dump \"\\(.*\\)\" buffer$"
  (lambda (magit-mode)
    (with-current-buffer (magit-mode-get-buffer (intern magit-mode))
      (message "%s" (buffer-string)))))

(When "^I dump current buffer$"
  (lambda ()
    (message "%s" (buffer-string))))

(When "^test stuff$"
  (lambda ()
    t))

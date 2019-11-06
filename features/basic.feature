@cc
Scenario: C-c C-c
  And magit-command "magit-branch-and-checkout dev master"
  And eval "(with-temp-file "file" (insert (make-temp-name "content")))"
  And magit-git "commit -am "1" --allow-empty"
  And eval "(with-temp-file "file" (insert (make-temp-name "content")))"
  And magit-git "commit -am "2" --allow-empty"
  And magit-git "log --abbrev-commit --pretty=oneline"
  And I dump process buffer
  And I press "C-x g"
  And I press "W"
  And I press "c"
  And I press "e"
  And I dump current buffer
  And I wait for messages to say git-commit-usage-message
  And I press "C-c C-c"
  And eval "(should (f-exists? "0001-Squashed-commit-of-the-following.patch"))"
  And eval "(should-not (default-value 'git-commit-post-finish-hook))"

Scenario: C-c C-k
  And magit-command "magit-branch-and-checkout dev master"
  And eval "(with-temp-file "file" (insert (make-temp-name "content")))"
  And magit-git "commit -am "1" --allow-empty"
  And eval "(with-temp-file "file" (insert (make-temp-name "content")))"
  And magit-git "commit -am "2" --allow-empty"
  And I press "C-x g"
  And I press "W"
  And I press "c"
  And I press "e"
  And I wait for messages to say git-commit-usage-message
  And eval "(message "a niggling timer needs to get flushed")"
  And eval "(accept-process-output nil 0.1)"
  And I press "C-c C-k"
  And eval "(should-not (f-exists? "0001-Squashed-commit-of-the-following.patch"))"
  Then eval "(should-not (default-value 'git-commit-post-finish-hook))"

Scenario: kill-buffer
  And magit-command "magit-branch-and-checkout dev master"
  And eval "(with-temp-file "file" (insert (make-temp-name "content")))"
  And magit-git "commit -am "1" --allow-empty"
  And eval "(with-temp-file "file" (insert (make-temp-name "content")))"
  And magit-git "commit -am "2" --allow-empty"
  And I press "C-x g"
  And I press "W"
  And I press "c"
  And I press "e"
  And I wait for messages to say git-commit-usage-message
  And eval "(message "a niggling timer needs to get flushed")"
  And eval "(accept-process-output nil 0.1)"
  And I press "C-x k"
  And eval "(should-not (f-exists? "0001-Squashed-commit-of-the-following.patch"))"
  Then eval "(should-not (default-value 'git-commit-post-finish-hook))"

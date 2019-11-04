Scenario: kill-buffer
  And magit-command "magit-branch-and-checkout dev master"
  And eval "(magit-status-setup-buffer default-directory)"
  And I dump "magit-status-mode" buffer

Scenario: test
  And magit-command "magit-branch-and-checkout dev master"
  And eval "(magit-status-setup-buffer default-directory)"
  And eval "(should (magit-file-tracked-p "file"))"
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
  And I press "C-c C-c"
  And eval "(should (f-exists? "0001-Squashed-commit-of-the-following.patch"))"
  And eval "(should-not git-commit-post-finish-hook)"

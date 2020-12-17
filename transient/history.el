((magit-commit nil
	       ("--all"))
 (magit-push
  ("--force-with-lease" "--force")
  nil)
 (magit-status-jump nil))

(setq jacob-org-trello-packages
      '((org-trello :fetcher github
                    :repo "org-trello/org-trello"
                    :files ("org-trello*.el"))))

(defun jacob-org-trello/init-org-trello ()
  (use-package org-trello)

  ;; Override location of .trello
  (defconst org-trello--config-dir (format "%s%s/" "~/.spacemacs.d/" ".trello")
    "Default trello directory for the configuration files.")

  (defconst org-trello--config-file (expand-file-name (format "%s/%s" org-trello--config-dir org-trello--config-filename))
    "Absolute path to org-trello's config file.")

  (custom-set-variables '(org-trello-files '("/lw/trello/current.org" "lw/trello/icebox.org"))))

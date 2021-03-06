(general-define-key
 :states 'normal
 :prefix "SPC"

 "TAB" #'my/alternate-buffer

 "ff" #'helm-find-files
 "fo" #'ff-find-other-file
 "Ts" #'helm-themes

 "fs" #'evil-write-all
 "fD" #'my/delete-current-buffer-file
 "fR" #'my/rename-current-buffer-file

 "fed" #'my/find-dotfile
 "fek" #'my/find-dotfile-keys

 "bb" #'helm-mini
 "bn" #'next-buffer
 "bp" #'previous-buffer
 "bd" #'kill-this-buffer
 "bD" #'my/kill-other-buffers

 "wo" #'other-frame

 "gS" #'magit-status
 "gs" #'my/magit-status-fullscreen

 "wm" #'my/toggle-maximize-buffer
 "w|" #'my/maximize-vertically
 "w_" #'my/maximize-horizontally

 "ws" #'split-window-below
 "wS" #'split-window-below-and-focus
 "w-" #'split-window-below
 "wv" #'split-window-right
 "wV" #'split-window-right-and-focus
 "ww" #'other-window
 "wx" #'kill-buffer-and-window
 "wd" #'delete-window
 "wD" #'delete-other-windows

 "wh" #'evil-window-left
 "wj" #'evil-window-down
 "wk" #'evil-window-up
 "wl" #'evil-window-right

 "wH" #'evil-window-move-far-left
 "wJ" #'evil-window-move-very-bottom
 "wK" #'evil-window-move-very-top
 "wL" #'evil-window-move-far-right

 "w<left>"    #'evil-window-left
 "w<down>"    #'evil-window-down
 "w<up>"      #'evil-window-up
 "w<right>"   #'evil-window-right

 "w<S-left>"  #'evil-window-move-far-left
 "w<S-down>"  #'evil-window-move-very-bottom
 "w<S-up>"    #'evil-window-move-very-top
 "w<S-right>" #'evil-window-move-far-right

 ;; "cd"  #'xref-find-definitions
 ;; "cr"  #'xref-find-references
 "cd"  #'lsp-find-definition
 "cr"  #'lsp-find-references
 "cs"  #'helm-lsp-workspace-symbol

 ;; format
 "m=b" #'lsp-format-buffer
 "m=r" #'lsp-format-region
 "mmp" #'prettier-js

 ;; goto
 "mgt" #'lsp-find-type-definition
 "mgM" #'lsp-ui-imenu

 ;; help
 "mhh" #'lsp-describe-thing-at-point

 ;; jump
 ;; backend
 "mbd" #'lsp-describe-session
 "mbr" #'lsp-workspace-restart
 "mbs" #'lsp-workspace-shutdown

 ;; log buffer
 "mlb" #'magit-log-buffer-file

 ;; refactor
 "mrr" #'lsp-rename

 ;; toggles
 "mTd" #'lsp-ui-doc-mode
 "mTs" #'lsp-ui-sideline-mode
 "mTl" #'lsp-lens-mode

 ;; folders
 "mFs" #'lsp-workspace-folders-switch
 "mFr" #'lsp-workspace-folders-remove
 "mFa" #'lsp-workspace-folders-add

 ;; semantic code navigation from ccls
 "nh"  #'my/navigate-left
 "nj"  #'my/navigate-down
 "nk"  #'my/navigate-up
 "nl"  #'my/navigate-right

 ;; global org-mode
 "oc"  #'org-capture
 "oa"  #'org-agenda
 "ooc" #'my/jlewallen-open-capture
 "oon" #'my/jlewallen-open-notes
 "ood" #'my/jlewallen-open-desk
 )

(general-define-key
 :keymaps 'org-mode-map
 :states 'normal
 :prefix "SPC"
 "ojn" #'org-journal-open-next-entry
 "ojp" #'org-journal-open-previous-entry
 "obe" #'org-babel-execute-src-block
 "oba" #'org-babel-execute-buffer
 "obr" #'org-table-recalculate-buffer-tables
 "ooa" #'org-archive-subtree-default
 "ooc" #'org-toggle-checkbox
 "oor" #'org-refile
 "ooo" #'org-clock-out
 "oot" #'org-todo
 "oof" #'org-capture-finalize
 "ook" #'org-capture-kill
 "oha" #'hide-sublevels
 "on"  #'my/time-stamp
 )

(general-define-key
 :keymaps 'ledger-mode-map
 :states 'normal
 :prefix "SPC"
 "mc" #'ledger-toggle-current
 "mC" #'ledger-mode-clean-buffer
 "mD" #'my/ledger-duplicate-current-transaction
 )

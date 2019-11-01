;; ---------------------------------------------------------------------------------------
;;
;; C-h b     Show all bindings available in a buffer
;;
;;

(setq inhibit-startup-message t)

;; ---------------------------------------------------------------------------------------
;; package loading

(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

;; ---------------------------------------------------------------------------------------
;; general configuration

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(setq user-full-name "Jacob Lewallen"
      user-mail-address "jlewallen@gmail.com"
      calendar-location-name "Los Angeles"
      calendar-latitude 0.0
      calendar-longitude -0.0)

(setq-default tab-width 4
              js2-basic-offset 4
              js-indent-level 4
              css-indent-offset 4
              sgml-basic-offset 4
              web-mode-markup-indent-offset 4
              web-mode-css-indent-offset 4
              web-mode-code-indent-offset 4
              web-mode-attr-indent-offset 4
              backup-inhibited t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-window-scroll-bars (minibuffer-window) nil nil)
(setq create-lockfiles nil)
(setq ring-bell-function 'ignore)
(setq recentf-max-saved-items 100)

(fset 'yes-or-no-p 'y-or-n-p)

;; ---------------------------------------------------------------------------------------
;; appearance

(defun my/setup-frame (&optional frame)
  "Configure look of FRAME.

If FRAME is nil, configure current frame. If non-nil, make FRAME
current."
  (when frame (select-frame frame))
  (when (window-system)
    (set-face-attribute 'default nil :height 100 :family "Ubuntu Mono")))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/setup-frame)
  (my/setup-frame))

;; ---------------------------------------------------------------------------------------
;; themes

(defun my/load-themes ()
  (load-file (expand-file-name "themes.el" (file-name-directory load-file-name)))
  (setq custom-safe-themes t)
  (dolist (pkg jl-themes-packages)
    (package-install pkg)))

(defun my/fixup-fringe-coloring (&rest args)
  (set-face-attribute 'fringe nil :background nil))

(my/load-themes)

(advice-add 'load-theme :after 'my/fixup-fringe-coloring)

(load-theme 'nimbus t)

;; ---------------------------------------------------------------------------------------
;; evil-mode

(defun my/evil-mode-config ()
  (evil-mode 1))

(use-package evil :config (my/evil-mode-config))

(use-package evil-surround
  :after (evil)
  :config (global-evil-surround-mode 1))

;; ---------------------------------------------------------------------------------------
;; projectile

(defun my/projectile-config ())

(use-package projectile
  :config (my/projectile-config))

;; ---------------------------------------------------------------------------------------
;; helm

(defun my/helm-config ()
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  (define-key helm-map (kbd "C-h") #'evil-delete-backward-word)
  (define-key helm-map (kbd "C-w") #'evil-delete-backward-word)
  (define-key helm-map (kbd "ESC") #'helm-keyboard-quit)
  (add-hook 'helm-after-initialize-hook
            (lambda()
              (define-key helm-buffer-map (kbd "ESC") 'helm-keyboard-quit)
              (define-key helm-find-files-map (kbd "ESC") 'helm-keyboard-quit)
              (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)))

  (setq helm-display-function 'pop-to-buffer)
  (helm-mode 1))

(use-package helm
  :config (my/helm-config))

(use-package helm-themes :after helm)
(use-package helm-projectile :after helm)
(use-package helm-company :after helm)
(use-package helm-lsp :after helm)
(use-package helm-xref :after helm)

;; ---------------------------------------------------------------------------------------
;; miscellaneous

;; (setq golden-ratio-auto-scale t)
(use-package golden-ratio
  :config (golden-ratio-mode 1))

(defun my/shackle-config ())

(use-package shackle
  :config (my/shackle-config))

;; ---------------------------------------------------------------------------------------
;; general

(defun my/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window)))
     nil t)))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun my/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

;; from https://gist.github.com/3402786
(defun my/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun my/maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun my/maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-up) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-down) (error nil))
      (delete-window))))

(defun my/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))

(defun my/navigate-left ()
  (interactive)
  (ccls-navigate "L"))

(defun my/navigate-right ()
  (interactive)
  (ccls-navigate "R"))

(defun my/navigate-down ()
  (interactive)
  (ccls-navigate "D"))

(defun my/navigate-up ()
  (interactive)
  (ccls-navigate "U"))

(defun my/general-config ()
  (general-define-key
   :states 'normal
   :prefix "SPC"

   "TAB" #'my/alternate-buffer

   "ff" #'helm-find-files
   "Ts" #'helm-themes

   "fs" #'evil-write-all

   "fed" #'my/find-dotfile

   "bb" #'helm-mini
   "bn" #'next-buffer
   "bp" #'previous-buffer
   "bd" #'kill-this-buffer
   "bD" #'my/kill-other-buffers

   "wo" #'other-frame

   "gs" #'magit-status

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

   "mgd" #'lsp-find-definition
   "mgr" #'lsp-find-references
   "mgs" #'helm-lsp-workspace-symbol

   ;; format
   "m=b" #'lsp-format-buffer
   "m=r" #'lsp-format-region

   ;; goto
   "mgt" #'lsp-find-type-definition
   "mgM" #'lsp-ui-imenu

   ;; help
   "mhh" #'lsp-describe-thing-at-point

   ;; jump
   ;; backend
   "mbd" #'lsp-describe-session
   "mbr" #'lsp-restart-workspace
   "mbs" #'lsp-shutdown-workspace

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

   "nh"  #'my/navigate-left
   "nj"  #'my/navigate-down
   "nk"  #'my/navigate-up
   "nl"  #'my/navigate-right
   ))

(use-package general
  :after (evil helm helm-lsp helm-themes lsp-mode)
  :config (my/general-config))

;; ---------------------------------------------------------------------------------------
;; which-key

(defun my/which-key-config ()
  (which-key-mode))

(use-package which-key
  :after (evil)
  :config (my/which-key-config))

;; ---------------------------------------------------------------------------------------
;; org

(defun my/org-config ()
  (setq org-enable-org-journal-support t)
  (setq org-journal-dir "~/dropbox/notes/journal")
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
  (setq org-journal-carryover-items nil)
  (setq org-directory "~/dropbox/notes")
  (setq org-default-notes-file (concat org-directory "/capture.org"))

  (setq org-agenda-files (list "~/dropbox/notes/journal"
                               "~/dropbox/notes/cal"
                               "~/dropbox/notes")))

(use-package org
  :config (my/org-config))

;; ---------------------------------------------------------------------------------------
;; magit

(defun my/magit-config ())

(use-package magit
  :config (my/magit-config))

(use-package evil-magit
  :after magit
  :config (my/magit-config))

;; ---------------------------------------------------------------------------------------
;; programming modes

;; https://github.com/MaskRay/ccls/wiki/Project-Setup
;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
;; installed llvm/clangd into my path
;; https://releases.llvm.org/download.html
;; https://github.com/golang/tools/blob/master/gopls/doc/user.md
;; GO111MODULE=on go get golang.org/x/tools/gopls@latest
;; pip install python-language-server
;; npm install -g javascript-typescript-langserver

(use-package smartparens :defer)

(defun my/lsp-config ()
  (require 'lsp-clients)

  ;; (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))

  ;; prefer using lsp-ui (flycheck) over flymake.
  (setq lsp-prefer-flymake nil)

  ;; disabled completely
  (setq lsp-enable-file-watchers nil)

  (setq lsp-enable-snippet nil)

  ;; enable lsp-mode for various languages
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'js2-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp))

(defun my/ccls-config ()
  (setq ccls-executable "ccls")

  ;; (setq ccls-sem-highlight-method 'font-lock)
  ;; (setq ccls-sem-highlight-method 'overlay)
  (ccls-use-default-rainbow-sem-highlight)

  (add-hook 'lsp-after-open-hook #'ccls-code-lens-mode))

(use-package ccls
  :config (my/ccls-config))

(use-package lsp-mode
  :ensure t
  :after ccls
  :config (my/lsp-config))

(use-package lsp-ui
  :requires lsp-mode flycheck
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company
  :config
  (setq company-idle-delay 0.3)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends)

  ;; disable client-side cache because the lsp server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

(use-package python-mode)

(defun my/cc-mode-config ()
  (c-add-style "my-c-style"
               '("stroustrup"
                 (indent-tabs-mode . nil)
                 (c-basic-offset . 4)
                 (c-offsets-alist . ((innamespace . [0])
                                     (inline-open . 0)
                                     (brace-list-open . 0)
                                     (statement-case-open . +)))))

  (defun my/c++-mode-hook ()
    (c-set-style "my-c-style")
    (auto-fill-mode)
    (c-set-offset 'inextern-lang 0)
    (c-toggle-auto-hungry-state 1))

  (defun my/c-mode-hook ()
    (c-set-style "my-c-style")
    (auto-fill-mode)
    (c-set-offset 'inextern-lang 0)
    (c-toggle-auto-hungry-state 1))

  (add-hook 'c++-mode-hook 'my/c++-mode-hook)
  (add-hook 'c-mode-hook 'my/c-mode-hook)

  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

(use-package cc-mode :config (my/cc-mode-config))
(use-package cmake-ide :after cc-mode)

(defun my/go-mode-config ()
  (require 'go-guru)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package go-guru
  :defer)

(use-package go-mode
  :config (my/go-mode-config))

(use-package typescript-mode)

(defun my/js2-mode-config ())

(use-package js2-mode
  :config (my/js2-mode-config))

(use-package vue-mode)
(use-package ledger-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package protobuf-mode)

(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(zenburn-theme zen-and-art-theme yaml-mode white-sand-theme which-key vue-mode use-package underwater-theme ujelly-theme typescript-mode twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smartparens seti-theme reverse-theme rebecca-theme railscasts-theme python-mode purple-haze-theme protobuf-mode professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme nimbus-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme magit madhat2r-theme lush-theme lsp-ui light-soap-theme ledger-mode kaolin-themes json-mode js2-mode jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme helm-xref helm-themes helm-projectile helm-lsp helm-company hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme golden-ratio go-guru general gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme evil-surround espresso-theme dracula-theme doom-themes django-theme darktooth-theme darkokai-theme darkmine-theme darkburn-theme darcula-theme dakrone-theme cyberpunk-theme company-lsp color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-ide clues-theme cherry-blossom-theme ccls busybee-theme bubbleberry-theme birds-of-paradise-plus-theme base16-theme badwolf-theme auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

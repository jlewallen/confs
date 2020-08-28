;; ---------------------------------------------------------------------------------------
;;
;; C-h b     Show all bindings available in a buffer
;;
;;

;; ---------------------------------------------------------------------------------------
;; package loading

(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(if (and (fboundp 'native-comp-available-p)
		 (native-comp-available-p))
	(message "Native compilation is available")
  (message "Native complation is *not* available"))

(if (functionp 'json-serialize)
	(message "Native JSON is available")
  (message "Native JSON is *not* available"))

(setq comp-deferred-compilation t)

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

(defun my/path-file-here (file)
  (expand-file-name file (file-name-directory (or load-file-name buffer-file-name))))

(defun my/load-file-here (file)
  (load-file (my/path-file-here file)))

;; ---------------------------------------------------------------------------------------
;; general configuration

(setq gc-cons-threshold 402653184)
(setq gc-cons-percentage 0.6)
(setq file-name-handler-alist nil)
(setq site-run-file nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq ring-bell-function 'ignore)
(setq tramp-default-method "ssh")
(setq large-file-warning-threshold nil)
(setq recentf-max-saved-items 100)
(setq delete-trailing-lines t)
(setq user-full-name "Jacob Lewallen")
(setq user-mail-address "jlewallen@gmail.com")
(setq calendar-location-name "Los Angeles")
(setq calendar-latitude 34.052234)
(setq calendar-longitude -118.243685)

(setq-default tab-width 4)
(setq-default js2-basic-offset 4)
(setq-default js-indent-level 4)
(setq-default css-indent-offset 4)
(setq-default sgml-basic-offset 4)
(setq-default web-mode-markup-indent-offset 4)
(setq-default web-mode-css-indent-offset 4)
(setq-default web-mode-code-indent-offset 4)
(setq-default web-mode-attr-indent-offset 4)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-window-scroll-bars (minibuffer-window) nil nil)

(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(my/load-file-here "funcs.el")
(my/load-file-here "add-node-modules-path.el")

;; ---------------------------------------------------------------------------------------
;; appearance

(defun my/face-ubuntu-mono ()
  (set-face-attribute 'default nil
					  :family "Ubuntu Mono"
					  :height (cond
							   ((and (eq (display-pixel-width) 2560) (eq (display-pixel-height) 1440)) 120) ; thinkpad
							   ((and (eq (display-pixel-width) 5760) (eq (display-pixel-height) 1600)) 110) ; desktops
							   (t 110))))

(defun my/face-source-code-pro ()
  (set-face-attribute 'default nil
					  :family "Source Code Pro"
					  :height (cond
							   ((and (eq (display-pixel-width) 2560) (eq (display-pixel-height) 1440)) 110) ; thinkpad
							   ((and (eq (display-pixel-width) 5760) (eq (display-pixel-height) 1600)) 100) ; desktops
							   (t 100))))

(defun my/face-iosevka ()
  (set-face-attribute 'default nil
					  :family "Iosevka"
					  :height (cond
							   ((and (eq (display-pixel-width) 2560) (eq (display-pixel-height) 1440)) 115) ; thinkpad
							   ((and (eq (display-pixel-width) 5760) (eq (display-pixel-height) 1600)) 110) ; desktops
							   (t 100))))

(defun my/setup-frame (&optional frame)
  "Configure look of FRAME.

If FRAME is nil, configure current frame. If non-nil, make FRAME
current."
  (when frame (select-frame frame))
  (when (window-system)
	(message "setting faces, resolution: %d %d" (display-pixel-width) (display-pixel-height))
	(my/face-iosevka)))

(define-key special-event-map [config-changed-event] 'ignore)

(if (daemonp)
	(add-hook 'after-make-frame-functions #'my/setup-frame)
  (my/setup-frame))

;; ---------------------------------------------------------------------------------------
;; themes

(defun my/load-themes ()
  (my/load-file-here "themes.el")
  (setq custom-safe-themes t)
  (dolist (pkg jl-themes-packages)
	(package-install pkg))
  (add-to-list 'custom-theme-load-path (my/path-file-here "themes"))
  (load-theme 'horizon t))

(defun my/fixup-fringe-coloring (&rest args)
  (set-face-attribute 'fringe nil :background nil))

(my/load-themes)

(advice-add 'load-theme :after 'my/fixup-fringe-coloring)

(load-theme 'doom-laserwave t)

;; ---------------------------------------------------------------------------------------
;; evil-mode

(defun my/evil-mode-initialize ()
  (setq evil-want-C-u-scroll t))

(defun my/evil-mode-config ()
  (evil-mode 1))

(use-package evil
  :init (my/evil-mode-initialize)
  :config (my/evil-mode-config))

(use-package evil-surround
  :after (evil)
  :config (global-evil-surround-mode 1))

;; ---------------------------------------------------------------------------------------
;; projectile

(defun my-projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
	(and root (cons 'transient root))))

(defun my/projectile-config ()
  (projectile-mode t)

  (with-eval-after-load 'project
	(add-to-list 'project-find-functions 'my-projectile-project-find-function))
  )

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
  (add-hook 'helm-after-initialize-hook
			(lambda()
			  (define-key helm-buffer-map (kbd "ESC") #'helm-keyboard-quit)
			  (define-key helm-find-files-map (kbd "ESC") #'helm-keyboard-quit)
			  (define-key helm-map (kbd "ESC") #'helm-keyboard-quit)))

  (setq helm-always-two-windows t)
  (setq helm-split-window-inside-p t)
  (setq helm-display-buffer-default-height 23)
  (setq helm-candidate-number-limit 1000)

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

(defun my/golden-ratio ()
  ;; (setq golden-ratio-auto-scale t)
  (golden-ratio-mode 1))

(use-package golden-ratio
  :config (my/golden-ratio))

(defun my/shackle-config ())

(use-package shackle
  :config (my/shackle-config))

;; ---------------------------------------------------------------------------------------
;; general

(defun my/general-config ()
  (my/load-file-here "keys.el"))

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

(defun my/gnuplot-config ())

(use-package gnuplot
  :config (my/gnuplot-config))

(defun my/evil-org-mode ()
  (add-to-list 'load-path "~/.emacs.d/evil-org-mode")
  (require 'evil-org)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar heading shift todo))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(defun my/org-bullets ()
  (add-to-list 'load-path "~/.emacs.d/org-bullets")
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun my/org-config ()
  (setq org-directory "~/dropbox/notes")
  (setq org-default-notes-file (concat org-directory "/capture.org"))
  (setq org-confirm-babel-evaluate nil)
  (setq org-archive-location "~/dropbox/notes/archive/archive.org::datetree/* Archived")

  (setq org-agenda-files (list "~/dropbox/notes/journal"
							   "~/dropbox/notes"))

  (setq org-capture-templates
		'(("t" "TODO" entry (file+headline org-default-notes-file "Tasks")
		   "* TODO %?\n  %T\n  %i\n  %a")
		  ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
		   "* %? :IDEA: \n%T")
		  ("s" "Scratch" entry (file+headline org-default-notes-file "Scratch")
		   "* %? :IDEA: \n%T\n  %i\n  %a")
		  ("m" "Meeting" entry (file+datetree org-default-notes-file)
		   "* MEETING with %? :MEETING:\n%T" :clock-in t :clock-resume t)
		  ("j" "Journal" entry (file+datetree "~/dropbox/notes/journal.org")
		   "* %?\nEntered on %U\n  %i\n  %a")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
	 (gnuplot . t)))
  (my/evil-org-mode)
  (my/org-bullets))

(use-package org
  :config (my/org-config))

(use-package org-journal
  :ensure t
  :after org
  :custom
  (org-journal-dir "~/dropbox/notes/journal/")
  (org-journal-date-prefix "#+FILETAGS: journal\n\n")
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-date-format "%A, %d %B %Y"))

;; ---------------------------------------------------------------------------------------
;; magit

(defun my/magit-config ()
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit
  :config (my/magit-config))

(use-package evil-magit
  :after magit
  :config (my/magit-config))

(use-package git-timemachine
  :after evil-magit)

; (use-package git-gutter-fringe+
;   :after evil-magit
;   :config
;   (global-git-gutter+-mode)
;   (git-gutter-fr+-minimal))

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
;; npm install -g typescript-language-server

(use-package smartparens
  :config (smartparens-global-mode))

(defun my/lsp-config ()
  ;(require 'lsp-clients)

  (setq lsp-clients-clangd-executable "/home/jlewallen/tools/clang+llvm-10.0.0-x86_64-linux-gnu-ubuntu-18.04/bin/clangd")
  (setq lsp-clients-clangd-args '("--compile-commands-dir=build"
								  "--pch-storage=memory"
								  "-j=4" "--background-index" "--log=error"))

  ;; prefer using lsp-ui (flycheck) over flymake.
  (setq lsp-prefer-flymake nil)

  ;; disabled completely
  (setq lsp-enable-file-watchers nil)

  (setq lsp-enable-snippet nil)
  (setq lsp-print-performance t)

  ; debugging
  ; (setq lsp-log-io nil)
  ; (setq lsp-trace nil)
  ; (setq lsp-response-timeout 20)
  )

(defun my/ccls-config ()
  (setq ccls-executable "~/tools/ccls/Release/ccls")

  ;; (setq ccls-sem-highlight-method 'font-lock)
  ;; (setq ccls-sem-highlight-method 'overlay)
  (ccls-use-default-rainbow-sem-highlight)

  (add-hook 'lsp-after-open-hook #'ccls-code-lens-mode))

(use-package ccls
  :config (my/ccls-config))

(use-package lsp-mode
  :ensure t
  :after ccls
  :hook ((vue-mode . lsp)
		 (typescript-mode . lsp)
		 (rust-mode . lsp)
		 (go-mode . lsp)
		 (js2-mode . lsp)
		 (js-mode . lsp)
		 (c++-mode . lsp)
		 (python-mode . lsp))
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

;(use-package company-lsp
;  :requires company
;  :config
;  (push 'company-lsp company-backends)

  ;; disable client-side cache because the lsp server does a better job.
;  (setq company-transformers nil
;		company-lsp-async t
;		company-lsp-cache-candidates nil))

(defun my/python-config ())

(use-package python-mode
  :config (my/python-config))

(defun my/python-black-config ()
  (add-hook 'python-mode-hook #'python-black-on-save-mode))

(use-package python-black
  :demand t
  :after python-mode
  :config (my/python-black-config))

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
(use-package cmake-mode :after cc-mode)

(use-package terraform-mode)

(use-package modern-cpp-font-lock
  :after cc-mode
  :config
  (modern-c++-font-lock-global-mode t))

(defun my/go-mode-config ()
  (require 'go-guru)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package go-guru
  :defer)

(use-package go-mode
  :config (my/go-mode-config))

(use-package typescript-mode
  :hook (typescript-mode . add-node-modules-path))

(defun my/js2-mode-config ())

(use-package js2-mode
  :hook ((js2-mode . add-node-modules-path)
		 (js-mode . add-node-modules-path))
  :config (my/js2-mode-config))

(defun my/vue-mode-config ()
  (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (add-hook 'js-mode-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-submode-decoration-level 0)
	;(add-hook 'mmm-mode-hook (lambda () (set-face-background 'mmm-default-submode-face nil)))
  )

(use-package vue-mode
  :config (my/vue-mode-config))

(use-package ledger-mode
  :config
  (setq ledger-post-amount-alignment-column 100))

(use-package yaml-mode)
(use-package json-mode)
(use-package protobuf-mode)
(use-package groovy-mode)

(defun my/prettier-js-config ())

(use-package prettier-js
  :after lsp-mode
  :hook ((js2-mode . prettier-js-mode)
		 (js-mode . prettier-js-mode)
		 (typescript-mode . prettier-js-mode)
		 (vue-mode . prettier-js-mode))
  :config (my/prettier-js-config))

(server-start)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-term-color-vector
   [unspecified "#1d2021" "#d72638" "#88b92d" "#f19d1a" "#1e8bac" "#be4264" "#1e8bac" "#d5d5d5"] t)
 '(beacon-color "#F8BBD0")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#689d6a")
 '(cua-normal-cursor-color "#7c6f64")
 '(cua-overwrite-cursor-color "#b57614")
 '(cua-read-only-cursor-color "#98971a")
 '(cursor-type 'bar)
 '(ensime-sem-high-faces
   '((var :foreground "#9876aa" :underline
		  (:style wave :color "yellow"))
	 (val :foreground "#9876aa")
	 (varField :slant italic)
	 (valField :foreground "#9876aa" :slant italic)
	 (functionCall :foreground "#a9b7c6")
	 (implicitConversion :underline
						 (:color "#808080"))
	 (implicitParams :underline
					 (:color "#808080"))
	 (operator :foreground "#cc7832")
	 (param :foreground "#a9b7c6")
	 (class :foreground "#4e807d")
	 (trait :foreground "#4e807d" :slant italic)
	 (object :foreground "#6897bb" :slant italic)
	 (package :foreground "#cc7832")
	 (deprecated :strike-through "#a9b7c6")))
 '(evil-emacs-state-cursor '("#D50000" hbar) t)
 '(evil-insert-state-cursor '("#D50000" bar) t)
 '(evil-normal-state-cursor '("#F57F17" box) t)
 '(evil-visual-state-cursor '("#66BB6A" box) t)
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#f1c40f")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(frame-brackground-mode 'dark)
 '(fringe-mode 6 nil (fringe))
 '(helm-completion-style 'emacs)
 '(highlight-changes-colors '("#d3869b" "#8f3f71"))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   '("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315"))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors '(("#F8BBD0" . 0) ("#FAFAFA" . 100)))
 '(hl-bg-colors
   '("#e29a3f" "#df6835" "#cf5130" "#f598a7" "#c2608f" "#5b919b" "#82cc73" "#c6c148"))
 '(hl-fg-colors
   '("#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7"))
 '(hl-paren-background-colors '("#2492db" "#95a5a6" nil))
 '(hl-paren-colors '("#ecf0f1" "#ecf0f1" "#c0392b"))
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#c792ea"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#c3e88d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#37474F"))
 '(line-spacing 0.2)
 '(linum-format 'dynamic)
 '(lsp-ui-doc-border "#665c54")
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(main-line-separator-style 'chamfer)
 '(nrepl-message-colors
   '("#9d0006" "#af3a03" "#b57614" "#747400" "#c6c148" "#004858" "#689d6a" "#d3869b" "#8f3f71"))
 '(objed-cursor-color "#ff5370")
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-date-prefix "#+FILETAGS: journal

")
 '(org-journal-dir "~/dropbox/notes/journal/")
 '(org-journal-file-format "%Y%m%d.org")
 '(package-selected-packages
   '(org-babel-gnuplot python-black terraform-mode cmake-mode evil-org-mode evil-org poet-theme zerodark-theme warm-night-theme zenburn-theme zen-and-art-theme yaml-mode white-sand-theme which-key vue-mode use-package underwater-theme ujelly-theme typescript-mode twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smartparens seti-theme reverse-theme rebecca-theme railscasts-theme python-mode purple-haze-theme protobuf-mode professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme nimbus-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme magit madhat2r-theme lush-theme lsp-ui light-soap-theme ledger-mode kaolin-themes json-mode js2-mode jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme helm-xref helm-themes helm-projectile helm-lsp helm-company hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme golden-ratio go-guru general gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme evil-surround espresso-theme dracula-theme doom-themes django-theme darktooth-theme darkokai-theme darkmine-theme darkburn-theme darcula-theme dakrone-theme cyberpunk-theme company-lsp color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-ide clues-theme cherry-blossom-theme ccls busybee-theme bubbleberry-theme birds-of-paradise-plus-theme base16-theme badwolf-theme auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme))
 '(pdf-view-midnight-colors (cons "#c5c8c6" "#1d1f21"))
 '(pos-tip-background-color "#ffffffffffff")
 '(pos-tip-foreground-color "#78909C")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(rustic-ansi-faces
   ["#263238" "#ff5370" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(smartrep-mode-line-active-bg (solarized-color-blend "#98971a" "#ebdbb2" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(tabbar-background-color "#ffffffffffff")
 '(term-default-bg-color "#fbf1c7")
 '(term-default-fg-color "#7c6f64")
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((30 . "#e74c3c")
	 (60 . "#c0392b")
	 (90 . "#e67e22")
	 (120 . "#d35400")
	 (150 . "#f1c40f")
	 (180 . "#d98c10")
	 (210 . "#2ecc71")
	 (240 . "#27ae60")
	 (270 . "#1abc9c")
	 (300 . "#16a085")
	 (330 . "#2492db")
	 (360 . "#0a74b9")))
 '(vc-annotate-very-old-color "#0a74b9")
 '(weechat-color-list
   '(unspecified "#fbf1c7" "#ebdbb2" "#750000" "#9d0006" "#747400" "#98971a" "#8a5100" "#b57614" "#004858" "#076678" "#9f4d64" "#d3869b" "#2e7d33" "#689d6a" "#7c6f64" "#3c3836"))
 '(when
	  (or
	   (not
		(boundp 'ansi-term-color-vector))
	   (not
		(facep
		 (aref ansi-term-color-vector 0)))))
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#ebdbb2" "#9d0006" "#98971a" "#b57614" "#076678" "#d3869b" "#689d6a" "#32302f"])
 '(xterm-color-names-bright
   ["#fbf1c7" "#af3a03" "#a89984" "#3c3836" "#7c6f64" "#8f3f71" "#665c54" "#282828"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

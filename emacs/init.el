;;; init.el --- Personal configuration -*- lexical-binding: t -*-

(set-face-attribute 'default nil
                    :font "Iosevka Term SS07"
                    :height 135
                    :weight 'regular)
(pcase system-type
  ('windows-nt
   (set-face-attribute 'variable-pitch nil
                       :font "Segoe UI"
                       :height 135
                       :weight 'regular))
  ('gnu/linux
   (set-face-attribute 'variable-pitch nil
                       :font "sans-serif"
                       :height 135
                       :weight 'regular)))
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka Term SS07"
                    :height 135
                    :weight 'regular)

(setq default-directory (concat (getenv "HOME") "/"))

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

(setq custom-file (concat user-emacs-directory "custom.el"))

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(indent-tabs-mode -1)
(setq-default tab-width 4)
(setq-default evil-shift-width 4)

(defmacro linux-specific! (body)
  (pcase system-type
     ('gnu/linux body)
     (_ ())))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package diminish)

(use-package general :config (general-evil-setup))

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(global-visual-line-mode t)
(diminish 'visual-line-mode)

(use-package gcmh
  :custom
  ;; From doom emacs' early-init.el
  (gcmh-idle-delay 'auto) ; default is 15s
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  :config (gcmh-mode 1)
  :diminish gcmh-mode)

(general-create-definer jawadcode/leader-keys
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "M-SPC")

;; Miscellaneous keybinds
(jawadcode/leader-keys
  "SPC" '(find-file :wk "Find file")
  "f"   '(:ignore t :wk "File")
  "f r" '(counsel-recentf :wk "Find recent files")
  "f c" '((lambda () (interactive) (find-file "~/.config/emacs/init.org")) :wk "Open emacs config")
  ";"   '(comment-line :wk "Comment lines")
  ;; Help keybinds
  "h" '(:ignore t :wk "Help")
  "h f" '(describe-function :wk "Describe function")
  "h v" '(describe-variable :wk "Describe variable")
  "h r" '((lambda () (interactive) (load-file user-init-file) (load-file user-init-file)) :wk "Reload config")
  ;; Toggle keybinds
  "t"   '(:ignore t :wk "Toggle")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t v" '(visual-line-mode :wk "Toggle visual-line-mode"))

(linux-specific!
 (use-package sudo-edit
   :config
   (jawadcode/leader-keys
     "s" '(:ignore t :wk "Sudo Edit")
     "s f" '(sudo-edit-find-file :wk "Sudo find file")
     "s e" '(sudo-edit :wk "Sudo edit file"))))

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :init
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1)
  (jawadcode/leader-keys
    "w"   '(:ignore t :wk "Windows")

    ;; Window splits
    "w x" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New horizontal window")
    "w m" '(evil-window-vnew :wk "New vertical window")
    "w h" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")

    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")))

;; Extra evil stuff
(use-package evil-collection
  :after evil
  :custom (evil-collection-mode-list '(dashboard dired ibuffer))
  :config (evil-collection-init)
  :diminish evil-collection-unimpaired-mode)

(use-package evil-anzu :after evil)

(use-package evil-tutor)

(use-package which-key
  :init (which-key-mode 1)
  :custom
  (which-key-add-column-padding 3)
  (which-key-idle-delay 0.1)
  :diminish which-key-mode)

(use-package treemacs-all-the-icons :defer t :commands treemacs-all-the-icons)

(use-package treemacs
  :config
  (jawadcode/leader-keys
    "t t" '((lambda () (treemacs)) :wk "Toggle treemacs"))
  (treemacs-load-all-the-icons-with-workaround-font "Hermit"))

(use-package treemacs-evil :after (treemacs evil))

(use-package treemacs-projectile :after (treemacs projectile))

(use-package treemacs-icons-dired)

(use-package treemacs-tab-bar :after treemacs)

(use-package projectile
  :config
  (projectile-mode 1)
  (jawadcode/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))
  :diminish projectile-mode)

(linux-specific!
 (use-package pdf-tools
   :mode ("\\.pdf\\'" . pdf-view-mode)
   :config
   (setq-default pdf-view-display-size 'fit-width)
   (setq pdf-view-use-scaling t
	 pdf-view-use-imagemagick nil)
   (add-hook 'pdf-view-mode-hook
	     (lambda ()
	   (setq-local evil-normal-state-cursor (list nil))))
   (evil-make-overriding-map pdf-view-mode-map 'normal)))

(use-package all-the-icons
  :if (display-graphic-p))

;; This enables all-the-icons in the dired file manager
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package dashboard
  :after (all-the-icons projectile)
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items))
  (setq dashboard-items '((recents   . 6)
                          (projects  . 6)
                          (bookmarks . 6)))
  :config
  (dashboard-setup-startup-hook))

(use-package solaire-mode :config (solaire-global-mode +1))

(window-divider-mode)

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t)
  (load-theme 'doom-material-dark t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package nerd-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package centaur-tabs
  :after (all-the-icons)
  :config
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-bar 'over)
  (centaur-tabs-mode t)
  :hook (dashboard-mode . centaur-tabs-local-mode)
  :bind
  ("C-<tab>"   . centaur-tabs-backward)
  ("C-S-<tab>" . centaur-tabs-forward))

(setq org-indent-mode nil)

;; Org-mode keybinds
(jawadcode/leader-keys
  "m"   '(:ignore t :wk "Org")
  "m a" '(org-agenda :wk "Org agenda")
  "m e" '(org-export-dispatch :wk "Org export dispatch")
  "m i" '(org-toggle-item :wk "Org toggle item")
  "m t" '(org-todo :wk "Org todo")
  "m B" '(org-babel-tangle :wk "Org babel tangle")
  "m T" '(org-todo-list :wk "Org todo list"))

;; Org mode table keybinds
(jawadcode/leader-keys
  "m b"   '(:ignore t :wk "Tables")
  "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

;; Org mode datetime keybinds
(jawadcode/leader-keys
  "m d"   '(:ignore t :wk "Date/deadline")
  "m d t" '(org-time-stamp :wk "Org time stamp"))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-tempo)

(use-package ivy
  ;; :bind
  ;; (("C-c C-r" . ivy-resume)
  ;;  ("C-x B"   . ivy-switch-buffer-other-window))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode)
  (jawadcode/leader-keys
    "i"   '(:ignore t :wk "Ivy")
    "i r" '(ivy-resume :wk "Resume previous Ivy completion")
    "i b" '(ivy-switch-buffer-other-window :wk "Switch to another buffer in another window"))
  :diminish ivy-mode)

(use-package counsel
  :after ivy
  :config (counsel-mode)
  :diminish counsel-mode)

;; Adds bling to our ivy completions
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1)
  :custom
  ;; I'll be honest, idk what this does
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                              'ivy-rich-switch-buffer-transform))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package company
  :init (setq company-tooltip-align-annotations t)
  :config
  (define-key company-active-map (kbd "C-n") nil) ; Select next
  (define-key company-active-map (kbd "C-p") nil) ; Select previous
  (define-key company-active-map (kbd "RET") nil) ; Complete selection
  (define-key company-active-map (kbd "M-j") #'company-select-next)
  (define-key company-active-map (kbd "M-k") #'company-select-previous)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (global-company-mode)
  (diminish 'company-capf-mode)
  :diminish company-mode)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :diminish company-box-mode)

(use-package smartparens-mode
  :straight smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package poly-org)

(use-package tree-sitter
  :after tree-sitter-langs
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package lsp-mode
  ;; :hook ((rust-mode          . lsp)
  ;;        (c-mode             . lsp)
  ;;        (c++-mode           . lsp)
  ;;        (meson-mode         . lsp)
  ;;        (conf-toml-mode     . lsp)
  ;;        (html-mode          . lsp)
  ;;        (css-mode           . lsp)
  ;;        (javascript-mode    . lsp)
  ;;        (typescript-mode    . lsp)
  ;;        (lsp-mode           . lsp-enable-which-key-integration))
  :hook ((prog-mode . lsp)
         (lsp-mode  . lsp-enable-which-key-integration))
  :config
  (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map)
  (setq lsp-inlay-hint-enable t)
  :commands lsp
  :diminish flymake-mode)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package rust-mode :commands rust-mode)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package lsp-haskell
  :hook ((haskell-mode          . lsp)
         (haskell-literate-mode . lsp)
         (haskell-mode          . (lambda () (setq-local evil-shift-width 2)))))

(use-package lean4-mode
  :straight (lean4-mode
             :host github
             :repo "leanprover/lean4-mode"
             :files ("*.el" "data"))
  :commands lean4-mode)

(linux-specific!
 (use-package idris2-mode
   :straight (idris2-mode
	  :host github
	  :repo "idris-community/idris2-mode")
   :commands idris2-mode))

(use-package meson-mode :commands meson-mode)

(use-package cmake-mode :commands cmake-mode)

(linux-specific!
 (progn
  (use-package lsp-nix
    :straight lsp-mode
    :custom (lsp-nix-nil-formatter ["nixpkgs-fmt"]))
  (use-package nix-mode
    :hook ((nix-mode . lsp-deferred)
           (nix-mode . (lambda ()
                         (setq-local tab-width 2)
                         (setq-local evil-shift-width 2)))))))

(use-package typescript-mode)

(use-package svelte-mode
  :hook ((svelte-mode . lsp)
         (svelte-mode . (lambda () (tree-sitter-hl-mode -1)))))

(use-package latex
  :straight auctex
  :defer t
  :custom (bibtex-dialect 'biblatex)
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (TeX-mode . prettify-symbols-mode)
  :init
  (setq-default TeX-master t)
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-save-query nil
        TeX-engine 'xetex
        TeX-PDF-mode t)
  :config
  ;; Source: https://tex.stackexchange.com/a/86119/81279
  (setq font-latex-match-reference-keywords
        '(;; BibLaTeX
          ("printbibliography" "[{")
          ("addbibresource" "[{")
          ;; Standard commands.
          ("cite" "[{")
          ("citep" "[{")
          ("citet" "[{")
          ("Cite" "[{")
          ("parencite" "[{")
          ("Parencite" "[{")
          ("footcite" "[{")
          ("footcitetext" "[{")
          ;; Style-specific commands.
          ("textcite" "[{")
          ("Textcite" "[{")
          ("smartcite" "[{")
          ("Smartcite" "[{")
          ("cite*" "[{")
          ("parencite*" "[{")
          ("supercite" "[{")
          ;; Qualified citation lists.
          ("cites" "[{")
          ("Cites" "[{")
          ("parencites" "[{")
          ("Parencites" "[{")
          ("footcites" "[{")
          ("footcitetexts" "[{")
          ("smartcites" "[{")
          ("Smartcites" "[{")
          ("textcites" "[{")
          ("Textcites" "[{")
          ("supercites" "[{")
          ;; Style-independent commands.
          ("autocite" "[{")
          ("Autocite" "[{")
          ("autocite*" "[{")
          ("Autocite*" "[{")
          ("autocites" "[{")
          ("Autocites" "[{")
          ;; Text commands.
          ("citeauthor" "[{")
          ("Citeauthor" "[{")
          ("citetitle" "[{")
          ("citetitle*" "[{")
          ("citeyear" "[{")
          ("citedate" "[{")
          ("citeurl" "[{")
          ;; Special commands.
          ("fullcite" "[{")
          ;; Cleveref.
          ("cref" "{")
          ("Cref" "{")
          ("cpageref" "{")
          ("Cpageref" "{")
          ("cpagerefrange" "{")
          ("Cpagerefrange" "{")
          ("crefrange" "{")
          ("Crefrange" "{")
          ("labelcref" "{")))
  (setq font-latex-match-textual-keywords
        '(;; BibLaTeX
          ("parentext" "{")
          ("brackettext" "{")
          ("hybridblockquote" "[{")
          ;; Auxiliary commands.
          ("textelp" "{")
          ("textelp*" "{")
          ("textins" "{")
          ("textins*" "{")
          ;; Subcaption.
          ("subcaption" "[{")))
  (setq font-latex-match-variable-keywords
      '(;; Amsmath.
        ("numberwithin" "{")
        ;; Enumitem.
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")
        ("crefname" "{")))

  (pcase system-type
    ('windows-nt
     (add-to-list 'TeX-view-program-list '("Okular" ("okular --noraise --unique file:%o" (mode-io-correlate "#src:%n%a"))))
     (add-to-list 'TeX-view-program-selection '(output-pdf "Okular")))
    ('gnu/linux
     (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
     (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)))

  (add-hook 'tex-mode-local-vars-hook #'lsp)
  (add-hook 'latex-mode-local-vars-hook #'lsp)

  (require 'tex-fold)
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode)
  (require 'preview)
  (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup))

(use-package auctex-latexmk
  :after latex
  :hook (LaTeX-mode . (lambda () (setq TeX-command-default "LatexMk")))
  :init (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config (auctex-latexmk-setup))
(use-package evil-tex
  :after latex
  :hook (LaTeX-mode . evil-tex-mode))
(use-package cdlatex
  :after latex
  :hook ((LaTeX-mode . cdlatex-mode)
	     (org-mode   . org-cdlatex-mode))
  :config (setq cdlatex-use-dollar-to-ensure-math nil))

(use-package company-auctex
  :after latex
  :config (company-auctex-init))

(use-package company-reftex
  :after latex
  :config
  (add-hook 'TeX-mode-hook
		(lambda ()
		  (setq-local company-backends
			  (append
				'(company-reftex-labels company-reftex-citations)
                                company-backends)))))

(use-package company-math
  :after latex
  :config
  (add-hook 'TeX-mode-hook
		(lambda ()
		  (setq-local company-backends
			  (append
				'(company-math-symbols-latex company-math-symbols-unicode company-latex-commands)
				company-backends)))))

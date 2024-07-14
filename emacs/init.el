;;; init.el --- Personal configuration -*- lexical-binding: t -*-

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(set-face-font 'default
        (font-spec :family "Iosevka Term SS07"
            :size 13.5
            :weight 'normal
            :width 'normal
            :slant 'normal))
(set-face-font 'fixed-pitch
        (font-spec :family "Iosevka Term SS07"
            :size 13.5
            :weight 'normal
            :width 'normal
            :slant 'normal))
(set-face-font 'variable-pitch
        (font-spec :family "Roboto"
            :size 13.5
            :weight 'normal
            :width 'normal))
(set-fontset-font t
        'emoji
        (font-spec :family "Noto Color Emoji"
              :size 13.5
              :weight 'normal
              :width 'normal
              :slant 'normal))

(scroll-bar-mode -1)

(keymap-global-set "C-+"            #'text-scale-increase)
(keymap-global-set "C--"            #'text-scale-decrease)
(keymap-global-set "C-<wheel-up>"   #'text-scale-increase)
(keymap-global-set "C-<wheel-down>" #'text-scale-decrease)

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

(use-package f)

(setq default-directory (f-slash (getenv "HOME")))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq custom-file (f-join user-emacs-directory "custom.el"))

(use-package delight
  :config
  (delight '((eldoc-mode nil "eldoc")
             (abbrev-mode nil "abbrev")
             (flycheck-mode nil "flycheck"))))

(use-package general :config (general-evil-setup))

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(global-visual-line-mode t)
(use-package emacs
  :delight (visual-line-mode))

(use-package gcmh
  :custom
  ;; From doom emacs' early-init.el
  (gcmh-idle-delay 'auto) ; default is 15s
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  :config (gcmh-mode 1)
  :delight)

(general-create-definer jawadcode/leader-keys
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "M-SPC")

(jawadcode/leader-keys
  ;; Buffer keybinds
  "b"   '(:ignore t :wk "Buffer")
  "b k" #'kill-buffer
  ;; File keybinds
  "f"   '(:ignore t :wk "File")
  "f f" #'find-file
  "f r" #'counsel-recentf
  "f c" '((lambda ()
            (interactive)
            (find-file (f-join user-emacs-directory "init.org")))
          :wk "Open emacs config")
  ";"   #'comment-line
  ;; Help keybinds
  "h"   '(:ignore t :wk "Help")
  "h f" #'describe-function
  "h v" #'describe-variable)

(electric-pair-mode 1)

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1)
  (jawadcode/leader-keys
    "w"   '(:ignore t :wk "Window")
    ;; Window splits
    "w x" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new    :wk "New horizontal window")
    "w m" '(evil-window-vnew   :wk "New vertical window")
    "w h" '(evil-window-split  :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")))

;; Extra evil
(use-package evil-collection
  :after evil
  :custom (evil-collection-mode-list '(dashboard dired ibuffer))
  :config (evil-collection-init)
  :delight evil-collection-unimpaired-mode)

(use-package evil-anzu :after evil)

(use-package evil-tutor)

(use-package which-key
  :custom
  (which-key-add-column-padding 3)
  (which-key-idle-delay 0.1)
  :config (which-key-mode 1)
  :delight)

(use-package projectile
  :config
  (projectile-mode 1)
  (jawadcode/leader-keys "p" projectile-command-map)
  :delight '(:eval (concat " " (projectile-project-name))))

(use-package poly-org)

(use-package dashboard
  :if (< (length command-line-args) 2)
  :after (all-the-icons projectile)
  :init
  (setq initial-buffer-choice 'dashboard-open
        dashboard-startup-banner 'logo
        dashboard-icon-type 'all-the-icons
        dashboard-projects-backend 'projectile
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items)
        dashboard-items '((recents   . 6)
                          (projects  . 6)
                          (bookmarks . 6)))
  :config
  (dashboard-setup-startup-hook))

(use-package treemacs-all-the-icons
  :defer t
  :commands treemacs-all-the-icons)

(use-package treemacs
  :after projectile
  :config
  (treemacs-load-all-the-icons-with-workaround-font "Hermit")
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  :general (jawadcode/leader-keys "r" #'treemacs))

(use-package treemacs-evil :after (treemacs evil))

(use-package treemacs-projectile :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-tab-bar :after treemacs)

(linux-specific!
 (use-package pdf-tools
   :mode ("\\.pdf\\'" . pdf-view-mode)
   :config
   (setq-default pdf-view-display-size 'fit-width)
   (setq pdf-view-use-scaling t
         pdf-view-use-imagemagick nil)
   (evil-set-initial-state 'pdf-view-mode 'emacs)
   (add-hook
    'pdf-view-mode-hook
    #'(lambda ()
       (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))))

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append))

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

(use-package solaire-mode :config (solaire-global-mode 1))

(window-divider-mode)

(use-package standard-themes
  :custom
  ;; Read the doc string of each of those user options.  These are some
  ;; sample values.
  (standard-themes-bold-constructs t)
  (standard-themes-italic-constructs t)
  (standard-themes-disable-other-themes t)
  (standard-themes-mixed-fonts t)
  (standard-themes-variable-pitch-ui t)
  (standard-themes-prompts '(extrabold italic))
  ;; more complex alist to set weight, height, and optional
  ;; `variable-pitch' per heading level (t is for any level not
  ;; specified):
  (standard-themes-headings
   '((0 . (variable-pitch light 1.8))
     (1 . (variable-pitch light 1.7))
     (2 . (variable-pitch light 1.6))
     (3 . (variable-pitch semilight 1.5))
     (4 . (variable-pitch semilight 1.4))
     (5 . (variable-pitch 1.3))
     (6 . (variable-pitch 1.2))
     (7 . (variable-pitch 1.1))
     (agenda-date . (1.2))
     (agenda-structure . (variable-pitch light 1.7))
     (t . (variable-pitch 1.0))))
  :config (standard-themes-load-light))

(use-package centaur-tabs
  :after (all-the-icons)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-change-fonts (face-attribute 'variable-pitch :font) 135)
  (jawadcode/leader-keys
    "t" '(:ignore t :wk "Centaur Tabs")
    "t n" #'centaur-tabs-forward
    "t p" #'centaur-tabs-backward)
  (centaur-tabs-mode t)
  :hook (dashboard-mode . centaur-tabs-local-mode))

(use-package tree-sitter
  :after tree-sitter-langs
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :delight)

(use-package tree-sitter-langs)

(setq org-indent-mode nil)

;; Org-mode keybinds
(jawadcode/leader-keys
  "o"   '(:ignore t :wk "Org")
  "o a" #'org-agenda
  "o e" #'org-export-dispatch
  "o i" #'org-toggle-item
  "o t" #'org-todo
  "o T" #'org-todo-list
  "o g" #'org-babel-tangle
  "o d" #'org-time-stamp)

;; Org mode table keybinds
(jawadcode/leader-keys
  "o b"   '(:ignore t :wk "Tables")
  "o b h" #'org-table-insert-hline)

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(add-hook 'org-mode-hook #'(lambda () (require 'org-tempo)))

(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode)
  :delight)

(use-package counsel
  :after ivy
  :config (counsel-mode)
  :delight)

;; Adds bling to our ivy completions
(use-package ivy-rich
  :after ivy
  :custom
  ;; I'll be honest, idk what this does
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                              'ivy-rich-switch-buffer-transform)
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :config (all-the-icons-ivy-rich-mode 1))

(use-package company
    :init (setq company-tooltip-align-annotations t)
    :config
    (keymap-set company-active-map "C-n"   nil)
    (keymap-set company-active-map "C-p"   nil)
    (keymap-set company-active-map "RET"   nil)
    (keymap-set company-active-map "M-j"   #'company-select-next)
    (keymap-set company-active-map "M-k"   #'company-select-previous)
    (keymap-set company-active-map "<tab>" #'company-complete-selection)
    (global-company-mode)
    (delight 'company-capf-mode)
    :delight)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :delight)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :delight yas-minor-mode)

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package lsp-mode
  :custom (lsp-inlay-hint-enable t)
  :commands lsp
  :hook ((prog-mode . lsp)
         (lsp-mode  . lsp-enable-which-key-integration)
         (lsp-mode  . (lambda ()
                        (jawadcode/leader-keys "l" lsp-command-map))))
  :delight flymake-mode)

(use-package lsp-ui       :commands lsp-ui-mode)
(use-package lsp-ivy      :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(defun c-c++-indentation-hook ()
  (setq c-basic-offset tab-width)
  (setq-local evil-shift-width 4))

(add-hook 'c-mode-hook 'c-c++-indentation-hook)
(add-hook 'c++-mode-hook 'c-c++-indentation-hook)

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
         ;; Looks worse with ts, css and js isn't highlighted
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
            #'(lambda ()
               (setq-local company-backends
                           (append
                            '(company-reftex-labels company-reftex-citations)
                            company-backends)))))

(use-package company-math
  :after latex
  :config
  (add-hook 'TeX-mode-hook
            #'(lambda ()
               (setq-local company-backends
                           (append
                            '(company-math-symbols-latex company-math-symbols-unicode company-latex-commands)
                            company-backends)))))

;; Rest of Configuration -*- lexical-binding: t; -*-

;; === EVIL MODE + WHICH-KEY ===

(setq evil-want-keybinding nil)

(use-package evil
  :custom
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-shift-width 4)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-mode 1))

(use-package evil-collection
  :custom (evil-collection-mode-list '(dashboard dired ibuffer))
  :config (evil-collection-init))

(use-package evil-anzu)

(use-package which-key
  :custom
  (which-key-idle-delay 0.05)
  (which-key-add-column-padding 0)
  (which-key-show-docstrings t)
  (which-key-max-description-length 54)
  (which-key-allow-evil-operator t)
  :config (which-key-mode 1))

;; === PROGRAMS ===

(use-package all-the-icons)

(use-package dashboard
  :init (setq initial-buffer-choice 'dashboard-open)
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'project-el)
  (dashboard-center-content t)
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-items))
  (dashboard-items '((recents . 10)
                     (projects . 10)))
  :config
  (dashboard-setup-startup-hook))

(use-package centaur-tabs
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-cycle-scope 'tabs)
  :hook (dashboard-mode . centaur-tabs-local-mode)
  :bind ( :map centaur-tabs-mode-map
          ("<leader> t n" . centaur-tabs-forward)
          ("<leader> t p" . centaur-tabs-backward))
  :config
  (centaur-tabs-change-fonts "Roboto" 140)
  (centaur-tabs-mode 1))

(use-package treemacs-all-the-icons
  :commands treemacs-all-the-icons)

(use-package treemacs
  :config
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  :bind ("<leader> r" . treemacs))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-tab-bar :after treemacs)

(use-package solaire-mode
  :config
  (solaire-global-mode 1))

;; === KEY BINDINGS ===

(keymap-global-set "C-+"            #'text-scale-increase)
(keymap-global-set "C--"            #'text-scale-decrease)
(keymap-global-set "C-<wheel-up>"   #'text-scale-increase)
(keymap-global-set "C-<wheel-down>" #'text-scale-decrease)

(defvar-keymap jawad/buffer-map
  :doc "My buffer keymap"
  "k" #'kill-buffer)

(defvar-keymap jawad/window-map
  :doc "My window keymap"
  "x" #'evil-window-delete
  "n" #'evil-window-new
  "m" #'evil-window-vnew
  "h" #'evil-window-split
  "v" #'evil-window-vsplit)

(use-package f)

(setq config-dir (f-dirname
                  (file-truename
                   (f-join user-emacs-directory "init.el"))))

(defun open-configs ()
  "Open config files."
  (interactive)
  (find-file (f-join config-dir "early-init.el"))
  (find-file (f-join config-dir "init.el")))

(defvar-keymap jawad/file-map
  :doc "My file keymap"
  "f" #'consult-fd
  "r" #'consult-recent-file
  "g" #'consult-ripgrep
  "c" #'open-configs)

(defvar-keymap jawad/global-map
  :doc "My global keymap"
  "f" jawad/file-map
  "b" jawad/buffer-map
  "w" jawad/window-map
  "p" project-prefix-map
  "h" help-map)

(evil-define-key 'normal 'global (kbd "<leader>") jawad/global-map)

;; === MINIBUFFER CONFIGURATION ===

(use-package vertico
  :demand t
  :custom
  (vertico-cycle 1)
  (vertico-resize nil)
  :config (vertico-mode 1)
  :bind ( :map vertico-map
          ("M-j" . vertico-next)
          ("M-k" . vertico-previous)))

(use-package marginalia
  :demand t
  :config (marginalia-mode 1))

(use-package orderless
  :demand t
  :config (setq completion-styles '(orderless basic)))

(use-package consult
  :demand t
  :config (setq completion-in-region-function 'consult-completion-in-region))

;; === EDITOR CONFIGURATION ===

(use-package tree-sitter
  :after tree-sitter-langs
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :delight)

(use-package tree-sitter-langs)

(setq default-directory (f-slash (getenv "HOME")))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq custom-file (f-join user-emacs-directory "custom.el"))

(setq-default indent-tabs-mode -1)
(setq-default tab-width 4)

(electric-pair-mode t)

(use-package ligature
  :config
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
  (global-ligature-mode 1))

;; === COMPLETIONS & SNIPPETS ===

(use-package company
  :init (setq company-tooltip-align-annotations t)
  :bind ( :map company-active-map
          ("M-j"   . #'company-select-next)
          ("M-k"   . #'company-select-previous)
          ("<tab>" . #'company-complete-selection))
  :config (global-company-mode))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :commands yas-minor-mode)

(use-package yasnippet-snippets
  :after (yasnippet))

;; === DIRENV (FOR NIX) ===

(when (eq system-type 'gnu/linux)
  (use-package inheritenv)

  (use-package envrc
    :hook (after-init . envrc-global-mode)
    :bind ( :map envrc-mode-map
            ("<leader> e" . envrc-command-map))))

;; === LSP-MODE (EGLOT IS A PAIN IN THE ARSE) ===

(use-package lsp-mode
	:custom
	(lsp-enable-which-key-integration t)
	(lsp-inlay-hint-enable t)
	:hook (lsp-mode . lsp-enable-which-key-integration)
	:config
	(defun lsp-booster--advice-json-parse (old-fn &rest args)
		"Try to parse bytecode instead of json."
		(or
		 (when (equal (following-char) ?#)
			 (let ((bytecode (read (current-buffer))))
				 (when (byte-code-function-p bytecode)
					 (funcall bytecode))))
		 (apply old-fn args)))
	(advice-add (if (progn (require 'json)
												 (fboundp 'json-parse-buffer))
									'json-parse-buffer
								'json-read)
							:around
							#'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
		"Prepend emacs-lsp-booster command to lsp CMD."
		(let ((orig-result (funcall old-fn cmd test?)))
			(if (and (not test?)                             ;; for check lsp-server-present?
							 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
							 lsp-use-plists
							 (not (functionp 'json-rpc-connection))  ;; native json-rpc
							 (executable-find "emacs-lsp-booster"))
					(progn
						(when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
							(setcar orig-result command-from-exec-path))
						(message "Using emacs-lsp-booster for %s!" orig-result)
						(cons "emacs-lsp-booster" orig-result))
				orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map)
	:commands (lsp lsp-deferred))

(use-package lsp-ui)
(use-package lsp-ivy)

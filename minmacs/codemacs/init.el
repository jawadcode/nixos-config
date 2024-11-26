;; Initialisation -*- lexical-binding: t; -*-

(setq emacs-dir (file-name-parent-directory user-emacs-directory))

(load (file-name-concat emacs-dir "common/core.el"))
(load (file-name-concat emacs-dir "common/rest.el"))

;; === LSP & LANGUAGE CONFIGURATIONS ===

(defun smol-tabs ()
  (setq tab-width 2)
  (setq-local evil-shift-width 2))

(add-hook 'emacs-lisp-mode-hook #'smol-tabs)

(use-package rust-mode
  :hook (rust-mode . lsp-deferred)
  :commands rust-mode)

(defun c/++-setup ()
  (setq c-basic-offset tab-width)
  (lsp-deferred))

(add-hook 'c-mode-hook   #'c/++-setup)
(add-hook 'c++-mode-hook #'c/++-setup)

(use-package tuareg
  :hook ((tuareg-mode . lsp-deferred)
         (tuareg-mode . prettify-symbols-mode)
         (tuareg-mode . (lambda () (setq tuareg-mode-name "🐫"))))
  :commands tuareg-mode)

(use-package haskell-mode
  :hook ((haskell-mode . lsp-deferred)
         (haskell-literate-mode . lsp-deferred)
         (haskell-mode . smol-tabs))
  :commands (haskell-mode haskell-literate-mode))

(use-package glsl-mode
  :hook (glsl-mode . lsp-deferred)
  :commands glsl-mode)

(use-package meson-mode
  :hook (meson-mode . lsp-deferred)
  :commands meson-mode)

(use-package cmake-mode
  :hook (cmake-mode . lsp-deferred)
  :commands cmake-mode)

(use-package typescript-mode
  :init (add-hook 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))
  :hook
  (js-mode . lsp-deferred)
  (javascript-mode . lsp-deferred)
  (typescript-mode . lsp-deferred)
  :commands (javascript-mode typescript-mode))

(use-package svelte-mode
  :hook (svelte-mode . (lambda ()
                         (tree-sitter-hl-mode -1)
                         (lsp-deferred)))
  :commands svelte-mode)

(when (eq system-type 'gnu/linux)
  (use-package idris2-mode
    :straight (idris2-mode
							 :host github
							 :repo "idris-community/idris2-mode")
    :hook (idris2-mode . lsp-deferred)
    :commands idris2-mode)
  (use-package nix-mode :mode "\\.nix\\'"))

(load (file-name-concat emacs-dir "common/load-env-vars.el"))

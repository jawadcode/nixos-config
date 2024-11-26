;; Initialisation -*- lexical-binding: t; -*-

(let ((emacs-dir (file-name-parent-directory user-emacs-directory)))
  (load (file-name-concat emacs-dir "common/core.el"))
  (load (file-name-concat emacs-dir "common/rest.el")))

;; === LANGUAGE CONFIGURATIONS ===

(use-package lean4-mode
  :straight (lean4-mode
             :host github
             :repo "leanprover/lean4-mode"
             :files ("*.el" "data"))
  :commands lean4-mode)

;; === LaTeX (with AUCTeX) ===

(use-package pdf-tools
	:mode ("\\.pdf\\'" . pdf-view-mode)
	:config
	(setq-default pdf-view-display-size 'fit-width)
	(setq pdf-view-use-scaling t
				pdf-view-use-imagemagick nil)
	(add-hook 'pdf-view-mode-hook
						(lambda ()
							(setq-local evil-normal-state-cursor (list nil))))
	(evil-make-overriding-map pdf-view-mode-map 'normal))

(use-package latex
  :straight auctex
  :defer t
  :custom (bibtex-dialect 'biblatex)
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . prettify-symbols-mode)
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
     (add-to-list 'TeX-view-program-selection '(output-pdf "SumatraPDF")))
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
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq-local company-backends
                          (append
                           '(company-reftex-labels company-reftex-citations)
                           company-backends)))))
(use-package company-math
  :after latex
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq-local company-backends
                          (append
                           '(company-math-symbols-latex company-math-symbols-unicode company-latex-commands)
                           company-backends)))))

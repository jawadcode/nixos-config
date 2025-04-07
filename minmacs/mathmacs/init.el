;; Initialisation -*- lexical-binding: t; -*-

(let ((emacs-dir (file-name-parent-directory user-emacs-directory)))
  (load (file-name-concat emacs-dir "common/core.el"))
  (load (file-name-concat emacs-dir "common/rest.el")))

;; === EVIL-SURROUND & EVIL-EMBRACE CONFIG (PRIMARILY FOR LATEX)

(use-package evil-surround
  :commands (global-evil-surround-mode evil-surround-edit evil-Surround-edit evil-surround-region)
  :config (global-evil-surround-mode 1))

;; Taken verbatim from https://github.com/doomemacs/doomemacs/blob/2bc052425ca45a41532be0648ebd976d1bd2e6c1/modules/editor/evil/config.el#L248
(defun evil-embrace-latex-mode-hook-h ()
  (dolist (pair '((?\' . ("`" . "\'"))
                  (?\" . ("``" . "\'\'"))))
    (delete (car pair) evil-embrace-evil-surround-keys)
    ;; Avoid `embrace-add-pair' because it would overwrite the default
    ;; rules, which we want for other modes
    (push (cons (car pair) (make-embrace-pair-struct
                            :key (car pair)
                            :left (cadr pair)
                            :right (cddr pair)
                            :left-regexp (regexp-quote (cadr pair))
                            :right-regexp (regexp-quote (cddr pair))))
          embrace--pairs-list))
  (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))

(use-package evil-embrace
  :commands embrace-add-pair embrace-add-pair-regexp
  :hook
  (LaTeX-mode . embrace-LaTeX-mode-hook)
  (LaTeX-mode . evil-embrace-latex-mode-hook-h)
  (org-mode   . embrace-org-mode-hook)
  :init (with-eval-after-load 'evil-surround (evil-embrace-enable-evil-surround-integration))
  :config (setq evil-embrace-show-help-p nil))

;; == LANGUAGE CONFIGURATIONS ==

(use-package lean4-mode
  :straight (lean4-mode
             :host github
             :repo "leanprover/lean4-mode"
             :files ("*.el" "data"))
  :commands lean4-mode)

;; === LaTeX (with AUCTeX) ===

;; Most of this config is taken directly from https://github.com/doomemacs/doomemacs/tree/2bc052425ca45a41532be0648ebd976d1bd2e6c1/modules/lang/latex

;;###autoload
(defun latex-fold-last-macro-a (&rest _)
  "Advice to auto-fold LaTeX macros after functions that
typically insert macros."
  ;; A simpler approach would be to just fold the whole line, but if point was
  ;; inside a macro that would kick it out. So instead we fold the last macro
  ;; before point, hoping its the one newly inserted.
  (TeX-fold-region (save-excursion
                     (search-backward "\\" (line-beginning-position) t)
                     (point))
                   (1+ (point))))

(use-package latex
  :straight auctex
  :init
  (setq TeX-parse-self t ; Enable parse on load
        TeX-auto-save t ; Enable parse on save
        ;; Use hidden dirs for AUCTeX files
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; Don't start the emacs server when correlating sources
        TeX-source-correlate-start-server nil
        ;; Auto-insert braces after sub/superscript in `LaTeX-math-mode'.
        TeX-electric-sub-and-superscript t
        ;; Save without asking before each compilation
        TeX-save-query nil
        ;; Use XeTeX as the LaTeX engine
        TeX-engine 'xetex)
  ;; Do not prompt for a master file
  (setq-default TeX-master t)

  :config
  ;; Fontification taken from https://tex.stackexchange.com/a/86119/81279.
  (setq font-latex-match-reference-keywords
        '(;; BibLaTeX.
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
        '(;; BibLaTeX brackets.
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

  (when (executable-find "evince")
    (add-to-list 'TeX-view-program-selection '(output-pdf "Evince")))

  ;; Set up `chktex'
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")

  (defun latex/compile ()
    (interactive)
    (TeX-save-document (TeX-master-file))
    (TeX-command TeX-command-default 'TeX-master-file -1))

  (evil-define-key 'normal latex-mode-map
    (kbd "<leader> t v") #'TeX-view
    (kbd "<leader> t c") #'latex/compile
    (kbd "<leader> t a") #'TeX-command-run-all
    (kbd "<leader> t m") #'TeX-command-master)
  (evil-define-key 'normal LaTeX-mode-map
    (kbd "<leader> t v") #'TeX-view
    (kbd "<leader> t c") #'latex/compile
    (kbd "<leader> t a") #'TeX-command-run-all
    (kbd "<leader> t m") #'TeX-command-master)

  ;; The standard LaTeXMk command uses `TeX-run-format' which doesn't trigger
  ;; `TeX-after-compilation-finished-functions', so we swap it out for
  ;; `TeX-run-TeX' which does.
  ;; TODO: Translate this to use the built-in macro `define-advice'
  (defadvice! latex-run-after-compilation-finished-functions-a (&rest args)
    :after #'TeX-TeX-sentinel
    (unless (TeX-error-report-has-errors-p)
      (run-hook-with-args 'TeX-after-compilation-finished-functions
                          (with-current-buffer TeX-command-buffer
                            (expand-file-name
                             (TeX-active-master (TeX-output-extension)))))))

  ;; Add the Table Of Contents entry to the sectioning hooks.
  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent 0)

  (require 'tex-fold)
  (add-hook 'TeX-mode-hook #'TeX-fold-mode)
  (defun latex-TeX-fold-buffer-h ()
    (run-with-idle-timer 0 nil 'TeX-fold-buffer))
  (add-hook 'TeX-mode-hook #'latex-TeX-fold-buffer-h)
  ;; Fold after all AUCTeX macro insertions
  (advice-add #'TeX-insert-macro :after #'latex-fold-last-macro-a)
  ;; Fold after CDLaTeX macro insertions
  (advice-add #'cdlatex-math-symbol :after #'latex-fold-last-macro-a)
  (advice-add #'cdlatex-math-modfiy :after #'latex-fold-last-macro-a)
  ;; Fold after snippets
  (defun latex-fold-snippet-contents-h ()
    (add-hook 'yas-after-exit-snippet-hook
              (lambda ()
                (when (and yas-snippet-beg yas-snippet-end)
                  (TeX-fold-region yas-snippet-beg yas-snippet-end)))
              :local))
  (add-hook 'TeX-fold-mode-hook #'latex-fold-snippet-contents-h)

	;; TODO: Rewrite this so that it doesn't prevent fixed-pitch items being
	;; updated by text-scale changes.
  ;; ;; Prevents folded items from being displayed as fixed-pitch
  ;; (defun latex-fold-set-variable-pitch-h ()
  ;;   (when mixed-pitch-mode
  ;;     ;; Adding to this list makes mixed-pitch clean the face remaps after us
  ;;     (add-to-list 'mixed-pitch-fixed-cookie
  ;;                  (face-remap-add-relative
  ;;                   'TeX-fold-folded-face
  ;;                   :family (face-attribute 'variable-pitch :family)
  ;;                   :height (face-attribute 'variable-pitch :height)))))
  ;; (add-hook 'mixed-pitch-mode-hook #'latex-fold-set-variable-pitch-h)

  (evil-define-key 'normal TeX-fold-mode-map
    (kbd "<leader> t f") #'TeX-fold-paragraph
    (kbd "<leader> t F") #'TeX-fold-clearout-paragraph
    (kbd "C-f")          #'TeX-fold-clearout-buffer)

  :hook
  (TeX-mode . (lambda ()
                (setq ispell-parser 'tex ; Tell emacs how to parse TeX files
                      ;; Don't auto-fill within math blocks
                      fill-nobreak-predicate (cons #'texmathp
                                                   fill-nobreak-predicate))))
  ((TeX-mode LaTeX-mode) . lsp-deferred))

(use-package cdlatex
  :hook
  (LaTeX-mode . cdlatex-mode)
  (org-mode . org-cdlatex-mode)
  :config
  ;; Use `\( ... \)' instead of `$ ... $'
  (setq cdlatex-use-dollar-to-ensure-math nil)
  :bind ( :map cdlatex-mode-map
          ;; Already handled by `yasnippet'
          ("TAB"   . nil)
          ;; Already handled by AUCTeX
          ("^"     . nil)
          ("_"     . nil)
          ("C-RET" . nil)))

(use-package adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adpative-wrap-extra-indent 0))

(use-package evil-tex :hook (LaTeX-mode . evil-tex-mode))

(use-package company-auctex)
(use-package company-reftex)
(use-package company-math
  :after (company-auctex company-reftex)
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq-local company-backends
                          (append
                           '(company-auctex-labels
                             company-auctex-bibs
                             (company-auctex-macros company-auctex-symbols company-auctex-environments)
                             company-reftex-labels
                             company-reftex-citations
                             company-math-symbols-unicode
                             company-latex-commands)
                           company-backends)))))


;; Core Configuration -*- lexical-binding: t; -*-

;; === USER INTERFACE STUFF ===

(set-face-font 'default
							 (font-spec :family "Iosevka Term SS07"
													:size 18
													:weight 'normal
													:width 'normal
													:slant 'normal))
(set-face-font 'fixed-pitch
							 (font-spec :family "Iosevka Term SS07"
													:size 18
													:weight 'normal
													:width 'normal
													:slant 'normal))
(set-face-font 'variable-pitch
							 (font-spec :family "Roboto"
													:size 18
													:weight 'normal
													:width 'normal))

(if (eq system-type 'windows-nt)
		(when (member "Noto Emoji" (font-family-list))
			(set-fontset-font t
												'emoji
												(font-spec :family "Noto Emoji"
																	 :size 18
																	 :weight 'normal
																	 :width 'normal
																	 :slant 'normal)))
	(when (member "Noto Color Emoji" (font-family-list))
		(set-fontset-font t
											'emoji
											(font-spec :family "Noto Color Emoji"
																 :size 18
																 :weight 'normal
																 :width 'normal
																 :slant 'normal))))

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'image-mode-hook #'(lambda () (blink-cursor-mode -1)))

(window-divider-mode)
(global-hl-line-mode)

;; === INITIALISE STRAIGHT.EL ===

(setq straight-base-dir (file-name-parent-directory user-emacs-directory))
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

(setq use-package-always-ensure t
      use-package-compute-statistics t)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; === CORE PACKAGES ===

(use-package monokai-theme
	:demand t
	:config (load-theme 'monokai t))

(use-package mood-line
  :config (mood-line-mode))

(use-package gcmh
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  :config (gcmh-mode 1))


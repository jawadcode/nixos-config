;; Core Configuration -*- lexical-binding: t; -*-

;; === USER INTERFACE STUFF ===

(defun set-font ()
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
                            :width 'normal)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'set-font)
  (set-font))

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
(add-hook 'image-mode-hook (lambda () (blink-cursor-mode -1)))

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

;; (use-package monokai-theme
;;   :demand t
;;   :config (load-theme 'monokai t))

(use-package standard-themes
	:config (standard-themes-load-theme 'standard-light))

(use-package mood-line
  :config (mood-line-mode))

(use-package gcmh
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  :config (gcmh-mode 1))

;; === MIXED PITCH ===

(use-package mixed-pitch :hook (text-mode . mixed-pitch-mode))

;; Taken from https://github.com/doomemacs/doomemacs/blob/2bc052425ca45a41532be0648ebd976d1bd2e6c1/lisp/doom-lib.el#L971
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

;; Early Initialisation -*- lexcal-binding: t; -*-

(setq package-enable-at-startup nil
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      native-comp-async-report-warnings-errors 'silent
      byte-compile-warnings nil
      inhibit-startup-screen t
      warning-minimum-level :error
      gc-cons-threshold most-positive-fixnum)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(set-language-environment "UTF-8")
(setq default-input-method nil) ; Undoes `set-language-environment`'s changes
;; Windows uses UTF-16 for its clipboard, so setting UTF-8 in that case would
;; break things
(unless (eq system-type 'windows-nt)
  (setq selection-coding-system 'utf-8))

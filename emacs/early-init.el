;;; early-init.el --- Early initialisation -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum
	  read-process-output-max (* 1024 1024)
	  package-enable-at-startup nil
	  native-comp-async-report-warnings-errors 'silent
	  byte-compile-warnings nil
	  inhibit-startup-screen t
	  warning-minimum-level :error)

(set-language-environment "UTF-8")
(setq default-input-method nil) ; Undoes `set-language-environment`'s changes
;; Windows uses UTF-16 for its clipboard, so let emacs do its own thing here
(if (not (eq system-type 'windows-nt))
	(setq default-input-method 'utf-8))

(setenv "LSP_USE_PLISTS" "true")

(setq menu-bar-mode      nil
	  tool-bar-mode      nil
	  column-number-mode t)

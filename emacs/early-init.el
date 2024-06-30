;;; early-init.el --- Early initialisation -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 1024 1024)
      package-enable-at-startup nil
      native-comp-async-report-warnings-errors 'silent
      byte-compile-warnings nil
      inhibit-startup-screen t
      warning-minimum-level :error)

(setenv "LSP_USE_PLISTS" "true")

(setq ;; menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      column-number-mode t)

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


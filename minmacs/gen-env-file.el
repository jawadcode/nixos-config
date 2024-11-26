;; Gen Environment File -*- lexical-binding: t; -*-

(defun gen-env-file (path)
  "Save envvars to a file at PATH"
    (let ((dirname (file-name-directory path)))
      (make-directory dirname t))
    (with-temp-file path
      (setq-local coding-system-for-write 'utf-8-unix)
      (insert
       ";; -*- mode: emacs-lisp -*-\n"
       ";; This file was automatically generated and will be overwritten.\n")
      (insert (pp-to-string process-environment))))

(gen-env-file (file-name-concat (getenv "USERPROFILE") "env.el"))

; Setting up the environment -*- lexical-binding: t; -*-

(setq env-file-path (file-name-concat (getenv "HOME") "env.el"))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name
        (seq-some (lambda (x) (if (file-exists-p x) x nil))
                  (list "C:/Program Files/PowerShell/7/pwsh.exe")))

  (defun load-env-file (file)
    "Read and set envvars from FILE."
    (if (null (file-exists-p file))
        (signal 'file-error
                (list "No envvar file exists." file
                      "Run `emacs --script ~/.emacs.d/scripts/gen-env-file.el`."))
      (with-temp-buffer
        (insert-file-contents file)
        (when-let (env (read (current-buffer)))
          (let ((tz (getenv-internal "TZ")))
            (setq-default
             process-environment
             (append env (default-value 'process-environment))
             exec-path
             (append (split-string (getenv "PATH") path-separator t)
                     (list exec-directory))
             shell-file-name
             (or (getenv "SHELL")
                 (default-value 'shell-file-name)))
            (when-let (newtz (getenv-internal "TZ"))
              (unless (equal tz newtz)
                (set-time-zone-rule newtz))))
          env))))

  (load-env-file env-file-path))

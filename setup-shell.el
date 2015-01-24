;; bash completion
;; TODO: is there a helm interface for shell-completion?
(require 'bash-completion)
(bash-completion-setup)

;; tab-completion for shell-command
(require 'shell-command)
(shell-command-completion-mode)

;; C-d to kill buffer if process is dead.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

;; TODO should this kill/bury dired buffer so when I bury shell dired isn't on top?
(defun dired-shell-jump ()
  "Open an `shell' that corresponds to current directory."
  (interactive)
  (let ((current-dir (dired-current-directory)))
    (process-send-string 
     (shell)
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))

(defun shell-jump ()
  "Open an `shell' and cd into directory of current buffer."
  (interactive)
  (if (eq major-mode 'shell-mode)
      ;; TODO better idea would be to cd directory of the buffer I jumped from.
      ;; It should be somewhere on some stack.
      (process-send-string (shell) "cd ..\n")
    (let* ((file buffer-file-name)
           (dir (if file (file-name-directory file) default-directory)))
      (other-window 1)
      (process-send-string 
       (shell)
       (if (file-remote-p dir)
           (let ((v (tramp-dissect-file-name dir t)))
             (format "ssh %s@%s\n"
                     (aref v 1) (aref v 2)))
         (format "cd '%s'\n" dir))))))

;; TODO force it to pop shell in other-window
(defun helm-ff-switch-to-shell (_candidate)
  "Switch to shell and cd to `helm-ff-default-directory'."
  (let ((cd-shell #'(lambda ()
                       (comint-kill-input)
                       (goto-char (point-max))
                       (insert
                        (format "cd '%s'" helm-ff-default-directory))
                       (comint-send-input)
                       (goto-char (point-max)))))
    (if (get-buffer "*shell*")
        (helm-switch-to-buffer "*shell*")
      (call-interactively 'shell))
    (funcall cd-shell)))

(defun helm-ff-run-switch-to-shell ()
  "Run switch to shell action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-ff-switch-to-shell)))

(provide 'setup-shell)

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

(provide 'setup-shell)

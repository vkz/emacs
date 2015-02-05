;; bash completion
;; TODO: is there a helm interface for shell-completion?
(require 'bash-completion)
(bash-completion-setup)

;; tab-completion for shell-command
(require 'shell-command)
(shell-command-completion-mode)

;; TODO seems like this would make redundant add-hook every time i enter
;; shell-mode. Works only because 'add-hook checks for redundant functions.
(add-hook 'shell-mode-hook
          '(lambda ()
             (add-hook 'comint-preoutput-filter-functions
                       '(lambda (output)
                          (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)) nil t)))

;; C-d to kill buffer if process is dead.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(defun shell-format-send-cd-string (dirstr)
  "Switch to *shell* and send 'cd dirstr' command."
    (shell)
    (comint-kill-input)
    (goto-char (point-max))
    (insert
     (if (file-remote-p dirstr)
         (let ((v (tramp-dissefct-file-name dirstr t)))
           (format "ssh %s@%s"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'" dirstr)))
    (comint-send-input))

(defun dired-shell-jump ()
  "Open an `shell' that corresponds to current directory."
  (interactive)
  (let ((dir (dired-current-directory))
        (win (selected-window)))
    (while (and (string= major-mode 'dired-mode)
                (equal (selected-window) win))
      (quit-window))
    (select-window win)
    (shell-format-send-cd-string dir)))

(defun shell-jump (&optional here)
  "Open an `shell' and cd into directory of current buffer. When
invoked from *shell* buffer, cd to the directory of the buffer in
the other window."
  (interactive)
  (let* ((file (if (eq major-mode 'shell-mode)
                   (progn (other-window 1) buffer-file-name)
                 buffer-file-name))
         (dir (if file (file-name-directory file) default-directory)))
    (unless here (other-window 1))
    (shell-format-send-cd-string dir)))

(defun helm-ff-switch-to-shell (_candidate)
  "Switch to shell and cd to `helm-ff-default-directory'."
  (let ((cd-shell #'(lambda ()
                      (shell-format-send-cd-string helm-ff-default-directory))))
    (unless (eq major-mode 'shell-mode)
      (if (get-buffer "*shell*")
          (switch-to-buffer-other-window "*shell*")
      (other-window 1)
      (call-interactively 'shell)
      (sleep-for 0.2)))
    (funcall cd-shell)))

(defun helm-ff-shell-jump ()
  "Run switch to shell action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-ff-switch-to-shell)))

(provide 'setup-shell)

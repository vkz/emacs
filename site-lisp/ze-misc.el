;;; ze-misc.el --- Misc defuns -*- lexical-binding: t; -*-
;;; Borrowed from BBatsov's prelude-core.el

(require 'dash)

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

;;;###autoload
(defun my-start-or-switch-to (function buffer-name &optional here)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (let ((switch-to (or (and here #'pop-to-buffer-same-window)
                       #'switch-to-buffer-other-window)))
    (if (get-buffer buffer-name)
        ;; NOTE not sure what last arg NORECORD t achieves
        (funcall switch-to buffer-name t)
      (unless here (other-window 1))
      (funcall function))))

;;;###autoload
(defun start-or-switch-to-shell (&optional here)
  (interactive)
  (my-start-or-switch-to 'shell "*shell*" here))

;;;###autoload
(defun start-or-switch-to-nodejs ()
  (interactive)
  (my-start-or-switch-to 'nodejs-repl "*nodejs*"))

;;;###autoload
(defun prelude-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

;;;###autoload
(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

;;;###autoload
(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

;;;###autoload
(defun prelude-duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(require 'windmove)

;;;###autoload
(defun prelude-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (let ((leftmost (or (windmove-find-other-window 'left) (selected-window))))
    (select-window leftmost)))

;;;###autoload
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(defun prelude-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

;;;###autoload
(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

;;;###autoload
(defun kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation"
  (interactive)
  (back-to-indentation)
  (kill-line))

;;;###autoload
(defun kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

;;;###autoload
(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

;;;###autoload
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;;###autoload
(defun region-as-string ()
  (buffer-substring (region-beginning)
                    (region-end)))

;;;###autoload
(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward))

;;;###autoload
(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward))

;;;###autoload
(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (progn
      (comment-or-uncomment-region (save-excursion (beginning-of-line) (point))
                                   (save-excursion (end-of-line) (point)))
      (next-line))))

;;;###autoload
(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer-other-window (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))

;;;###autoload
(defun font-lock-comment-annotations (mode)
  (font-lock-add-keywords
   mode
   '(("\\<\\(TODO\\)" 1 'font-lock-todo-annotation t)
     ("\\<\\(HACK\\)" 1 'font-lock-todo-annotation t)
     ("\\<\\(STUDY\\)" 1 'font-lock-study-annotation t)
     ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-annotation t)
     ("\\<\\(NOTE\\)" 1 'font-lock-note-annotation t))))

;;;###autoload
(defun comment-annotations-in-modes (modes)
  "Highlight a bunch of well known comment annotations."
  (mapc 'font-lock-comment-annotations
        modes))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

;;;###autoload
(defun quick-switch-buffer ()
  "Quickly swap current buffer with the one its hiding."
  (interactive)
  (let (buf (car (helm-buffer-list)))
    (pop-to-buffer-same-window buf)))

;;;###autoload
(defun i-meant-other-window ()
  "Oh, I meant find file or buffer in the other window."
  (interactive)
  (let ((buf (current-buffer)))
    (bury-buffer)
    (switch-to-buffer-other-window buf)))

;;;###autoload
(defun helm-mini/projectile-switch ()
  (interactive)
  (let* ((vkz-session helm-buffer)
         (switch-to
          (or
           (and (string-equal vkz-session "*helm mini*") 'helm-projectile)
           (and (string-equal vkz-session "*helm projectile*") 'helm-mini))))
    (helm-run-after-quit switch-to)))


;;;###autoload
(defun er/line-wise-select-advice ()
  "Expand region to whole lines whenever as long as the initial
mark wasn't set by `expand-region'."
  (defadvice er/expand-region (around line-wise-select activate)
    (if (or (not (region-active-p))
            (eq last-command 'er/expand-region))
        ad-do-it
      (if (< (point) (mark))
          (let ((beg (point)))
            (goto-char (mark))
            (end-of-line)
            (forward-char 1)
            (push-mark)
            (goto-char beg)
            (beginning-of-line))
        (let ((end (point)))
          (goto-char (mark))
          (beginning-of-line)
          (push-mark)
          (goto-char end)
          (end-of-line)
          (forward-char 1))))))

;;;###autoload
(defun ze-mark-paragraph ()
  (interactive)
  (mark-paragraph)
  (exchange-point-and-mark))

;;;###autoload
(defun ze-smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation.

Kill the whole line with function `kill-whole-line' and then move
`back-to-indentation'."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

;;;###autoload
(defun ze-smart-backward-kill-line ()
  "Kill line backwards and re-indent."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

;;;###autoload
(defun ze-smart-open-line ()
  "Insert empty line after the current line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;;;###autoload
(defun ze-back-to-indentation-or-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(define-minor-mode ze-auto-fill-comments-mode
  "Minor mode to auto-fill comments only."
  :lighter nil
  :keymap nil
  (cond
   (ze-auto-fill-comments-mode
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))
   (:else
    (kill-local-variable 'comment-auto-fill-only-comments)
    (auto-fill-mode -1))))

;;;###autoload
(defun ze-insert-current-date (iso)
  "Insert the current date at point.

When ISO is non-nil, insert the date in ISO 8601 format.
Otherwise insert the date as Mar 04, 2014."
  (interactive "P")
  (insert (format-time-string (if iso "%F" "%b %d, %Y"))))

;;;###autoload
(defun ze-bury-buffer-then-switch ()
  (interactive)
  (bury-buffer)
  (other-window 1))

(provide 'ze-misc)

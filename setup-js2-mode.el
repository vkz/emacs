;;; setup-js2-mode.el --- tweak js2 settings -*- lexical-binding: t; -*-

(setq-default js2-allow-rhino-new-expr-initializer nil)
(setq-default js2-global-externs '("module" "require" "assert" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "describe" "it"))
;; TODO: would increasing this decrease the bloody self-insert delay in js2-mode?
(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-mirror-mode nil)
(setq-default js2-include-rhino-externs nil)
(setq-default js2-include-gears-externs nil)
(setq-default js2-auto-indent-p t)
(setq-default js2-concat-multiline-strings 'eol)

;; TODO: should I toggle these?
(setq-default js2-enter-indents-newline nil)
(setq-default js2-indent-on-enter-key nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-rebind-eol-bol-keys nil)

;; TODO: Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
;; (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

;; TODO: remap mnemonics, some of them are horrendous on Dvorak
;; TODO: actually learn them
;; TODO: `guide-key' doesn't pick up mnemonics
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-r")

;; Set up wrapping of pairs, with the possiblity of semicolons thrown into the mix

(defun js2r--setup-wrapping-pair (open close)
  (define-key js2-mode-map (read-kbd-macro open) (λ (js2r--self-insert-wrapping open close)))
  (unless (s-equals? open close)
    (define-key js2-mode-map (read-kbd-macro close) (λ (js2r--self-insert-closing open close)))))

(define-key js2-mode-map (kbd ";")
  (λ (if (looking-at ";")
         (forward-char)
       (funcall 'self-insert-command 1))))

;; TODO weirdly it's in Magnar's fork of `js2-mode' but not in the official
;; package
(defsubst js2-mode-inside-comment-or-string ()
  "Return non-nil if inside a comment or string."
  (or
   (let ((comment-start
          (save-excursion
            (goto-char (point-at-bol))
            (if (re-search-forward "//" (point-at-eol) t)
                (match-beginning 0)))))
     (and comment-start
          (<= comment-start (point))))
   (let ((parse-state (save-excursion
                        (syntax-ppss (point)))))
     (or (nth 3 parse-state)
         (nth 4 parse-state)))))

(defun js2r--self-insert-wrapping (open close)
  (cond
   ((use-region-p)
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert close)
        (goto-char beg)
        (insert open))))

   ((and (s-equals? open close)
         (looking-back (regexp-quote open))
         (looking-at (regexp-quote close)))
    (forward-char (length close)))

   ((js2-mode-inside-comment-or-string)
    (funcall 'self-insert-command 1))

   (:else
    (let ((end (js2r--something-to-close-statement)))
      (insert open close end)
      (backward-char (+ (length close) (length end)))
      (js2r--remove-all-this-cruft-on-backward-delete)))))

(defun js2r--remove-all-this-cruft-on-backward-delete ()
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "DEL") 'undo-tree-undo)
     (define-key map (kbd "C-h") 'undo-tree-undo)
     map) nil))

(defun js2r--self-insert-closing (open close)
  (if (and (looking-back (regexp-quote open))
           (looking-at (regexp-quote close)))
      (forward-char (length close))
    (funcall 'self-insert-command 1)))

(defun js2r--does-not-need-semi ()
  (save-excursion
    (back-to-indentation)
    (or (looking-at "if ")
        (looking-at "function ")
        (looking-at "for ")
        (looking-at "while ")
        (looking-at "try ")
        (looking-at "} else "))))

(defun js2r--comma-unless (delimiter)
  (if (looking-at (concat "[\n\t\r ]*" (regexp-quote delimiter)))
      ""
    ","))

(defun js2r--something-to-close-statement ()
  (cond
   ((and (js2-block-node-p (js2-node-at-point)) (looking-at " *}")) ";")
   ;; ((not (eolp)) "")
   ((js2-array-node-p (js2-node-at-point)) (js2r--comma-unless "]"))
   ((js2-object-node-p (js2-node-at-point)) (js2r--comma-unless "}"))
   ((js2-object-prop-node-p (js2-node-at-point)) (js2r--comma-unless "}"))
   ((js2-call-node-p (js2-node-at-point)) (js2r--comma-unless ")"))
   ((js2r--does-not-need-semi) "")
   ;; NOTE is that more reasonable?
   ((not (eolp)) "")
   (:else ";")))

(js2r--setup-wrapping-pair "(" ")")
(js2r--setup-wrapping-pair "{" "}")
(js2r--setup-wrapping-pair "[" "]")
(js2r--setup-wrapping-pair "\"" "\"")
(js2r--setup-wrapping-pair "'" "'")

;; prevent js2-mode from stealing keyes
(define-key js2-mode-map (kbd "TAB") nil)
(define-key js2-mode-map (kbd "C-c C-e") nil)

;; ;; navigating by paragraph in js code is ineffecient
(define-key js2-mode-map (kbd "M-e") 'end-of-defun)
(define-key js2-mode-map (kbd "M-a") 'beginning-of-defun)
(define-key js2-mode-map (kbd "C-M-e") 'end-of-buffer)
(define-key js2-mode-map (kbd "C-M-a") 'beginning-of-buffer)

;; Use lambda for anonymous functions
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192")
                        nil)))))

;; Use right arrow for return in one-line functions
(font-lock-add-keywords
 'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u2190")
                        nil)))))

;; set indent offset to 2
(custom-set-default 'js2-basic-offset 2)

(require 'json)

;; jslime
(autoload 'jslime-mode "jslime")
(add-hook 'js2-mode-hook 'jslime-mode)
(add-hook 'js2-mode-hook 'show-smartparens-mode)

(provide 'setup-js2-mode)

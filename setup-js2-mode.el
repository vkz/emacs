;;; setup-js2-mode.el --- tweak js2 settings -*- lexical-binding: t; -*-

(setq-default js2-allow-rhino-new-expr-initializer nil)
(setq-default js2-global-externs '("module" "require" "assert" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
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

;; --- skipped ---
;; a lot of stuff here

;; TODO: Replacement for `smartparens-mode' with js-aware
;; wrapping-pair functions. Try living without it then with it, then
;; decide what to keep.

;; TODO: Replacement for `Tern' which I don't really use. Its
;; inference capabilities are rudimentary and work as often as
;; don't. For jumping consider a combination of TAGs, imenu and helm
;; interface to grep.

;; prevent js2-mode from stealing keyes
(define-key js2-mode-map (kbd "TAB") nil)
(define-key js2-mode-map (kbd "C-c C-e") nil)

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


(provide 'setup-js2-mode)

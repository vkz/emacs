;; should I do it at key-translation-map level?
(define-key global-map (kbd "C-t") 'Control-X-prefix)

(when is-mac
  (setq mac-command-modifier 'meta))

(provide 'key-bindings)

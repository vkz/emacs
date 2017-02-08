;; TODO clj needs cleaning up
;; TODO better clj/cider bindings
;; TODO clj/cider lispy bindings
(use-package align-cljlet
  :ensure t
  :after clojure-mode
  :init
  (add-hook 'clojure-mode-hook (lambda () (require 'align-cljlet))))

(use-package cider
  :ensure t
  :diminish cider-mode
  :init
  (progn
    (setq cider-stacktrace-default-filters '(tooling dup)
          cider-repl-pop-to-buffer-on-connect nil
          cider-repl-use-pretty-printing t
          cider-prompt-save-file-on-load nil
          cider-repl-use-clojure-font-lock t
          nrepl-hide-special-buffers t
          cider-repl-popup-stacktraces t
          cider-auto-select-error-buffer t)
    (setq cider-cljs-lein-repl
          "(do (require 'figwheel-sidecar.repl-api)
               (figwheel-sidecar.repl-api/start-figwheel!)
               (figwheel-sidecar.repl-api/cljs-repl))")
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
    (add-hook 'cider-repl-mode-hook 'lispy-mode))
  :config
  (progn
    ;; add support for golden-ratio
    (with-eval-after-load 'golden-ratio
      (push 'cider-popup-buffer-quit-function golden-ratio-extra-commands))

    ;; TODO this assumes anything but (ns user) in repl, which I don't like. We
    ;; should check current ns and do the right thing here.
    (defun cider-repl-reset ()
      "Companion function for `compontent' and `reloaded.repl'.
Assuming reloaded.repl/set-init! is set approapriately in (ns
user) will send (user/reset) to the repl."
      (interactive)
      (save-some-buffers)
      (with-current-buffer (cider-current-repl-buffer)
        (goto-char (point-max))
        ;; TODO not sure why this doesn't work but throws an exception
        (insert "(let [cns (ns-name *ns*)] (ns user) (reset) (in-ns cns))")
        (cider-repl-return)))

    (defun cider-figwheel-repl ()
      (interactive)
      (save-some-buffers)
      (with-current-buffer (cider-current-repl-buffer)
        (goto-char (point-max))
        (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
        (cider-repl-return)))

    (bind-keys :map cider-mode-map
               ("C-c r" . cider-repl-reset)
               ("C-c M-f" . cider-figwheel-repl))
    
    (defun spacemacs//cider-eval-in-repl-no-focus (form)
      "Insert FORM in the REPL buffer and eval it."
      (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
        (setq form (replace-match "" t t form)))
      (with-current-buffer (cider-current-repl-buffer)
        (let ((pt-max (point-max)))
          (goto-char pt-max)
          (insert form)
          (indent-region pt-max (point))
          (cider-repl-return))))

    (defun spacemacs/cider-send-last-sexp-to-repl ()
      "Send last sexp to REPL and evaluate it without changing
the focus."
      (interactive)
      (spacemacs//cider-eval-in-repl-no-focus (cider-last-sexp)))

    (defun spacemacs/cider-send-last-sexp-to-repl-focus ()
      "Send last sexp to REPL and evaluate it and switch to the REPL in
`insert state'."
      (interactive)
      (cider-insert-last-sexp-in-repl t)
      (evil-insert-state))

    (defun spacemacs/cider-send-region-to-repl (start end)
      "Send region to REPL and evaluate it without changing
the focus."
      (interactive "r")
      (spacemacs//cider-eval-in-repl-no-focus
       (buffer-substring-no-properties start end)))

    (defun spacemacs/cider-send-region-to-repl-focus (start end)
      "Send region to REPL and evaluate it and switch to the REPL in
`insert state'."
      (interactive "r")
      (cider-insert-in-repl
       (buffer-substring-no-properties start end) t))

    (defun spacemacs/cider-send-function-to-repl ()
      "Send current function to REPL and evaluate it without changing
the focus."
      (interactive)
      (spacemacs//cider-eval-in-repl-no-focus (cider-defun-at-point)))

    (defun spacemacs/cider-send-function-to-repl-focus ()
      "Send current function to REPL and evaluate it and switch to the REPL in
`insert state'."
      (interactive)
      (cider-insert-defun-in-repl t))

    (defun spacemacs/cider-send-ns-form-to-repl ()
      "Send buffer's ns form to REPL and evaluate it without changing
the focus."
      (interactive)
      (spacemacs//cider-eval-in-repl-no-focus (cider-ns-form)))

    (defun spacemacs/cider-send-ns-form-to-repl-focus ()
      "Send ns form to REPL and evaluate it and switch to the REPL in
`insert state'."
      (interactive)
      (cider-insert-ns-form-in-repl t))

    (defun spacemacs/cider-send-buffer-in-repl-and-focus ()
      "Send the current buffer in the REPL and switch to the REPL in
`insert state'."
      (interactive)
      (cider-load-buffer)
      (cider-switch-to-repl-buffer))

    (defun spacemacs/cider-test-run-focused-test ()
      (interactive)
      (cider-load-buffer)
      (spacemacs//cider-eval-in-repl-no-focus (cider-test-run-test)))

    (defun spacemacs/cider-test-run-all-tests ()
      (interactive)
      (cider-load-buffer)
      (spacemacs//cider-eval-in-repl-no-focus (cider-test-run-tests nil)))

    (defun spacemacs/cider-test-rerun-tests ()
      (interactive)
      (cider-load-buffer)
      (spacemacs//cider-eval-in-repl-no-focus (cider-test-rerun-tests)))

    (defun spacemacs/cider-display-error-buffer (&optional arg)
      "Displays the *cider-error* buffer in the current window.
If called with a prefix argument, uses the other-window instead."
      (interactive "P")
      (let ((buffer (get-buffer cider-error-buffer)))
        (when buffer
          (funcall (if (equal arg '(4))
                       'switch-to-buffer-other-window
                     'switch-to-buffer)
                   buffer))))

    (defun spacemacs/cider-toggle-repl-pretty-printing ()
      (interactive)
      (setq cider-repl-use-pretty-printing
            (if cider-repl-use-pretty-printing nil t))
      (message "Cider REPL pretty printing: %s"
               (if cider-repl-use-pretty-printing "ON" "OFF")))

    (defun spacemacs/cider-toggle-repl-font-locking ()
      (interactive)
      (setq cider-repl-use-clojure-font-lock
            (if cider-repl-use-pretty-printing nil t))
      (message "Cider REPL clojure-mode font-lock: %s"
               (if cider-repl-use-clojure-font-lock "ON" "OFF")))

    ;; open cider-doc directly and close it with q
    (setq cider-prompt-for-symbol nil)

    ;; TODO bindings
    ;; (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
    ;;   (spacemacs/set-leader-keys-for-major-mode m
    ;;                                             "hh" 'cider-doc
    ;;                                             "hg" 'cider-grimoire
    ;;                                             "hj" 'cider-javadoc

    ;;                                             "eb" 'cider-eval-buffer
    ;;                                             "ee" 'cider-eval-last-sexp
    ;;                                             "ef" 'cider-eval-defun-at-point
    ;;                                             "er" 'cider-eval-region
    ;;                                             "ew" 'cider-eval-last-sexp-and-replace

    ;;                                             "fb" 'cider-format-buffer

    ;;                                             "gb" 'cider-jump-back
    ;;                                             "ge" 'cider-jump-to-compilation-error
    ;;                                             "gg" 'cider-find-var
    ;;                                             "gr" 'cider-jump-to-resource

    ;;                                             "sb" 'cider-load-buffer
    ;;                                             "sB" 'spacemacs/cider-send-buffer-in-repl-and-focus
    ;;                                             "sc" 'cider-connect
    ;;                                             "se" 'spacemacs/cider-send-last-sexp-to-repl
    ;;                                             "sE" 'spacemacs/cider-send-last-sexp-to-repl-focus
    ;;                                             "sf" 'spacemacs/cider-send-function-to-repl
    ;;                                             "sF" 'spacemacs/cider-send-function-to-repl-focus
    ;;                                             "si" 'cider-jack-in
    ;;                                             "sI" 'cider-jack-in-clojurescript
    ;;                                             "sn" 'spacemacs/cider-send-ns-form-to-repl
    ;;                                             "sN" 'spacemacs/cider-send-ns-form-to-repl-focus
    ;;                                             "sq" 'cider-quit
    ;;                                             "sr" 'spacemacs/cider-send-region-to-repl
    ;;                                             "sR" 'spacemacs/cider-send-region-to-repl-focus
    ;;                                             "ss" 'cider-switch-to-repl-buffer
    ;;                                             "sx" 'cider-refresh

    ;;                                             "Tf" 'spacemacs/cider-toggle-repl-font-locking
    ;;                                             "Tp" 'spacemacs/cider-toggle-repl-pretty-printing

    ;;                                             "ta" 'spacemacs/cider-test-run-all-tests
    ;;                                             "tr" 'spacemacs/cider-test-rerun-tests
    ;;                                             "tt" 'spacemacs/cider-test-run-focused-test

    ;;                                             "db" 'cider-debug-defun-at-point
    ;;                                             "de" 'spacemacs/cider-display-error-buffer
    ;;                                             "di" 'cider-inspect))

    ;; (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
    ;;                                           "hh" 'cider-doc
    ;;                                           "hg" 'cider-grimoire
    ;;                                           "hj" 'cider-javadoc

    ;;                                           "ee" 'cider-eval-last-sexp
    ;;                                           "ef" 'cider-eval-defun-at-point
    ;;                                           "er" 'cider-eval-region
    ;;                                           "ew" 'cider-eval-last-sexp-and-replace

    ;;                                           "gb" 'cider-jump-back
    ;;                                           "ge" 'cider-jump-to-compilation-error
    ;;                                           "gg" 'cider-find-var
    ;;                                           "gr" 'cider-jump-to-resource

    ;;                                           "sc" 'cider-repl-clear-buffer
    ;;                                           "sn" 'cider-repl-set-ns
    ;;                                           "sq" 'cider-quit
    ;;                                           "ss" 'cider-switch-to-last-clojure-buffer
    ;;                                           "sx" 'cider-refresh

    ;;                                           "Tf" 'spacemacs/cider-toggle-repl-font-locking
    ;;                                           "Tp" 'spacemacs/cider-toggle-repl-pretty-printing

    ;;                                           "de" 'spacemacs/cider-display-error-buffer
    ;;                                           "di" 'cider-inspect)

    ;; (evil-define-key 'normal cider-repl-mode-map
    ;;                  "C-j" 'cider-repl-next-input
    ;;                  "C-k" 'cider-repl-previous-input)

    ))

(with-eval-after-load 'eval-sexp-fu
  (require 'cider-eval-sexp-fu))

(use-package clj-refactor
  :ensure t
  :diminish clj-refactor-mode
  :init
  (setq cljr-magic-require-namespaces
        '(("io"    . "clojure.java.io")
          ("set"   . "clojure.set")
          ("str"   . "clojure.string")
          ("walk"  . "clojure.walk")
          ("zip"   . "clojure.zip")
          ("time"  . "clj-time.core")
          ("log"   . "clojure.tools.logging")
          ("json"  . "cheshire.core")))
  (setq cljr-expectations-test-declaration "[expectations :refer :all]")
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (progn
    (cljr-add-keybindings-with-prefix "C-c C-r")

    ;; (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
    ;;                                           "r?"  'cljr-describe-refactoring
    ;;                                           "rap" 'cljr-add-project-dependency
    ;;                                           "ras" 'cljr-add-stubs
    ;;                                           "rcc" 'cljr-cycle-coll
    ;;                                           "rci" 'cljr-cycle-if
    ;;                                           "rcp" 'cljr-cycle-privacy
    ;;                                           "rdk" 'cljr-destructure-keys
    ;;                                           "rel" 'cljr-expand-let
    ;;                                           "rfu" 'cljr-find-usages
    ;;                                           "rhd" 'cljr-hotload-dependency
    ;;                                           "ril" 'cljr-introduce-let
    ;;                                           "rml" 'cljr-move-to-let
    ;;                                           "rpc" 'cljr-project-clean
    ;;                                           "rrl" 'cljr-remove-let
    ;;                                           "rsp" 'cljr-sort-project-dependencies
    ;;                                           "rsc" 'cljr-show-changelog
    ;;                                           "rtf" 'cljr-thread-first-all
    ;;                                           "rth" 'cljr-thread
    ;;                                           "rtl" 'cljr-thread-last-all
    ;;                                           "rua" 'cljr-unwind-all
    ;;                                           "rup" 'cljr-update-project-dependencies
    ;;                                           "ruw" 'cljr-unwind)
    ))

;; Stolen from Magnars
;; https://github.com/magnars/.emacs.d/blob/master/defuns/clj-defuns.el
(require 's)

(defun clj--src-file-name-from-test (name)
  (s-with name
    (s-replace "/test/" "/src/")
    (s-replace "_test.clj" ".clj")))

(defun clj--test-file-name-from-src (name)
  (s-with name
    (s-replace "/src/" "/test/")
    (s-replace ".clj" "_test.clj")))

(defun clj--src-file-name-from-cards (name)
  (s-with name
    (s-replace "/devcards/" "/src/")
    (s-replace "_cards.clj" ".clj")))

(defun clj--cards-file-name-from-src (name)
  (s-with name
    (s-replace "/src/" "/devcards/")
    (s-replace ".clj" "_cards.clj")))

(defun clj--is-test? (name)
  (string-match-p "/test/" name))

(defun clj--is-card? (name)
  (string-match-p "/devcards/" name))

(defun clj--is-ui? (name)
  (string-match-p "/ui/" name))

(defun clj-other-file-name ()
  (let ((name (buffer-file-name)))
    (cond
     ((clj--is-test? name) (clj--src-file-name-from-test name))
     ((clj--is-card? name) (clj--src-file-name-from-cards name))
     ((clj--is-ui? name) (clj--cards-file-name-from-src name))
     (:else (clj--test-file-name-from-src name)))))

(defun clj-find-alternative-name (file)
  (cond
   ((s-ends-with? ".cljs" file)
    (s-replace ".cljs" ".cljc" file))
   ((s-ends-with? ".clj" file)
    (s-replace ".clj" ".cljc" file))
   ((s-ends-with? ".cljc" file)
    (s-replace ".cljc" ".clj" file))))

(defun clj-jump-to-other-file (arg)
  (interactive "P")
  (let* ((file (clj-other-file-name))
         (alternative-file (clj-find-alternative-name file)))
    (cond
     ((file-exists-p file) (find-file file))
     ((file-exists-p alternative-file) (find-file alternative-file))
     (arg (find-file file)
          (save-buffer))
     (t (ido-find-file-in-dir (file-name-directory file))))))

(defun clj-jump-to-other-file-other-window (arg)
  (interactive "P")
  (let ((file (clj-other-file-name)))
    (if (or (file-exists-p file) arg)
        (find-file-other-window file)
      (error "%s not found." file))))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.boot\\'" . clojure-mode))
  :init
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))
  :config
  (add-hook 'clojure-mode-hook 'lispy-mode)
  (bind-keys :map clojure-mode-map
             ("C-:" . nil)
             ;; NOTE to create a missing "other" file like _test.clj run these
             ;; with a prefix argument.
             ;; TODO seems silly, fix this!
             ("C-c <tab>" . clj-jump-to-other-file-other-window)
             ("C-c <S-tab>" . clj-jump-to-other-file))
  (require 'datomic-snippets)
  ;; TODO (define-clojure-indent &rest KVS) instead of put-clojure-indent
  ;; (define-clojure-indent
  ;;   (d/a 1)
  ;;   (d/button 1)
  ;;   ...
  ;;   (d/svg 1))
  ;; (put-clojure-indent 'GET 'defun)
  ;; (put-clojure-indent 'POST 'defun)
  ;; (put-clojure-indent 'DELETE 'defun)
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

;; TODO steal clj-autotest from Magnars
;; https://github.com/magnars/.emacs.d/blob/master/site-lisp/clj-autotest.el
;; 
;; Good: it pretty much provides everything lein-autoexpect plugin offers
;; withouth the overhead of running another JVM instance.
;; 
;; Bad: code bitrotted e.g. nrepl-sync-request:eval it relies on uses different
;; calling convention now, so it's out of sync with cider code.
;;
;; Meanwhile: just add [lein-expectations ""] [lein-autoexpect ""] plugins to
;; project.clj and run `lein autoexpect' in terminal.

;; TODO there expectations-mode minor mode based on clojure test mode:
;; https://github.com/gar3thjon3s/expectations-mode
;; I think Magnars clj-autotest could be better.

(use-package symbol-focus
  :disabled t
  :load-path "site-lisp/"
  :after clojure-mode
  :bind (:map clojure-mode-map ("C-S-f" . sf/focus-at-point)))

(provide 'setup-clj)

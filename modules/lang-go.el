;;; go mode
;; go get -u github.com/nsf/gocode
;; go get -u golang.org/x/tools/...
;; go get -u github.com/rogpeppe/godef
(use-package go-mode
  :ensure t
  :init(progn
         (use-package go-eldoc
           :ensure t
           )
         )
  (use-package gotest
    :ensure t)
  :hook
  (
   (go-mode . lsp)
   (go-mode . (lambda ()
                (setq-default)
                (setq tab-width 4)
                (setq standard-indent 4)
                (setq indent-tabs-mode nil)
                ))
   )

  :config
  (setq exec-path (append exec-path '("~/Workspaces/development/go/bin")))
  ;; (setq lsp-gopls-use-placeholders nil)
  (exec-path-from-shell-copy-env "GOPATH")
  (define-key go-mode-map (kbd "C-c f") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-c t") 'go-test-current-test)
  (define-key go-mode-map (kbd "C-c p") 'go-test-current-project)
  (define-key go-mode-map (kbd "C-c b") 'go-test-current-benchmark)
  (define-key go-mode-map (kbd "C-c r") 'go-run)

  ;; (add-to-list 'lsp-file-watch-ignored "[/\\]vendor")
  )

(provide 'lang-go)

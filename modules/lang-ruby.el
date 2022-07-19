;;; ruby mode
;; gem install solargraph
(use-package ruby-mode
  :ensure t
  :hook
  (
   (ruby-mode . lsp)
   (ruby-mode . (lambda ()
                (setq-default)
                (setq tab-width 4)
                (setq standard-indent 4)
                (setq indent-tabs-mode nil)
                ))
   )
  )

(provide 'lang-ruby)

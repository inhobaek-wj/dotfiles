;;; yaml mode
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

;;; dockerfile mode
(use-package dockerfile-mode
  :ensure t
  :mode "/Dockerfile.*\\'")


;;; markdown mode
;; need to install pandoc first.
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc --from markdown --to html --ascii")
  )

;;; jenkinsfile mode
(use-package jenkinsfile-mode
  :ensure t
  :mode "/Jenkinsfile.*\\'")


;; (use-package pkgbuild-mode
;;   :ensure t
;;   :mode "/PKGBUILD\\'")

(provide 'text-file-mode)

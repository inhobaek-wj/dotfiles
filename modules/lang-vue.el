;; requirement npm i -g vue-language-server
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (require 'lsp-mode)
  (add-hook 'vue-mode-hook #'lsp))

;; disable background color in vue mode.
(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))

(provide 'lang-vue)

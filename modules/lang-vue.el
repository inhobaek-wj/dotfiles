;; requirement npm i -g vue-language-server
(defun my-vue-mode-hook ()
  (setq tab-width 2
        js-indent-align-list-continuation nil
        indent-tabs-mode nil))

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (require 'lsp-mode)
  (add-hook 'vue-mode-hook #'lsp)
  (add-hook 'vue-mode-hook  'my-vue-mode-hook)
  )

;; disable background color in vue mode.
(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))

(provide 'lang-vue)

;;; dart mode.
(use-package lsp-dart :ensure t)
(use-package dart-mode
  :ensure t
  :init
  (require 'lsp-dart)
  (setq lsp-dart--server-command "dart language-server")
  :hook (dart-mode . lsp)
  :custom
  (dart-format-on-save t)
  )

;;; flutter
(use-package flutter
  :after dart-mode
  :ensure t
  :init
  (setq flutter-sdk-path "~/Workspaces/development/flutter/")
  :bind
  (:map dart-mode-map
        ("C-M-x" . #'flutter-run-or-hot-reload)))

(provide 'lang-flutter)

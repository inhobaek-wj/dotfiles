
;;; dart mode.
;;; need to install dart package. sudo pacman -Syu dart
;;; when install flutter, it has dart-sdk internally.
(use-package dart-mode
  :ensure t
  :init
  (setq lsp-dart-sdk-dir "~/Workspaces/development/flutter/bin/cache/dart-sdk/"
        lsp-dart-suggest-from-unimported-libraries nil
        )
  ;; (setq lsp-dart--server-command "pub global activate dart_language_server")
  :hook
  ((dart-mode . lsp))
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

;; ;; Optional
;; (use-package flutter-l10n-flycheck
;;   :after flutter
;;   :ensure t
;;   :config
;;   (flutter-l10n-flycheck-setup))

(provide 'lang-flutter)

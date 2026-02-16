;;; lang-react.el --- React/TypeScript support via tree-sitter -*- lexical-binding: t; -*-

;;; Tree-sitter grammar sources
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

;;; Auto-install grammars if missing
(dolist (lang '(typescript tsx))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

;;; TypeScript
(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook
  ((typescript-ts-mode . lsp)
   (typescript-ts-mode . prettier-js-mode)
   (typescript-ts-mode . (lambda ()
                           (setq tab-width 2)
                           (setq indent-tabs-mode nil)))))

;;; TSX (React)
(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :hook
  ((tsx-ts-mode . lsp)
   (tsx-ts-mode . prettier-js-mode)
   (tsx-ts-mode . (lambda ()
                    (setq tab-width 2)
                    (setq indent-tabs-mode nil)))))

(provide 'lang-react)
;;; lang-react.el ends here

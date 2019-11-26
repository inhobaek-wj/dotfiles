;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; emacs-server
(require 'server)
(server-start)

;;; Korean
(set-language-environment "Korean")
(global-set-key (kbd "S-SPC") 'toggle-korean-input-method)

;;; date expression in English
(setq system-time-locale "C")

;;; encoding.
(set-language-environment   'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system  'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;; (unless (eq system-type 'windows-nt)
;;   (set-selection-coding-system 'utf-8))

(prefer-coding-system 'utf-8)

;;; emacs가 init.el에 추가하는 설정 방지
;;; (custom-set-variables ...
;;; https://jamiecollinson.com/blog/my-emacs-config/
(setq custom-file (make-temp-file "emacs-custom"))

;;; packages
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-milkbox" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;; backup
(add-to-list 'backup-directory-alist '("." . "~/.emacs-saves"))
(setq delete-old-versions t
      kept-old-versions 2
      kept-new-versions 2
      version-control t)

;;; whitespace mode
(global-whitespace-mode t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((nil (:bold t :background "yellow"))))
 '(whitespace-tab ((nil (:bold t :background "linen"))))
 '(whitespace-trailing ((nil (:bold t :background "red1")))))

(add-hook
 'after-change-major-mode-hook
 '(lambda ()
    (setq whitespace-line-column nil
          whitespace-style '(face tabs trailing lines-tail tab-mark))))

;; (add-hook 'before-save-hook 'cleanup-buffer)

;;; disable tabs mode
(setq-default indent-tabs-mode nil)

;; startup-message 안 보기
(setq inhibit-startup-message t)
;; *scratch* 버퍼 깨끗하게 시작하기
;;(setq initial-scratch-message nil)
;; 선택 텍스트를 타이핑할 때, 삭제
(delete-selection-mode t)
;; word-wrap
(global-visual-line-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copy region if active
;; otherwise copy to end of current line
;;   * with prefix, copy N whole lines
(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun copy-whole-lines (arg)
  "Copy ARG lines to the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun copy-line (arg)
  "Copy to end of line, or ARG lines."
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we
   are not at the end of the line, then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts
   comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (not (region-active-p))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  (interactive)
  (tabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; syntax highlighting on
(global-font-lock-mode t)
;; 커서가 있는 라인 하이라이트
(global-hl-line-mode t)
;; turn on line number
(global-linum-mode t)

(global-auto-revert-mode 1)

;; menu bar off
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;; theme
;;; https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :ensure t
  :init
  ;;; org에서 커진 한글 폰트가 너무 안 예뻐서
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-light 'NO-CONFIRM)
  (defconst my/solarized-light-red "#FF6E64")
  (defconst my/solarized-light-green "#B4C342")
  (defconst my/solarized-light-orange "#F2804F")
  (defconst my/solarized-base2 "#EEE8D5")
  (defconst my/solarized-hl "#EEEED5")
  (progn
    (custom-theme-set-faces
     'solarized-light
     `(hl-line
       ((t (:background ,my/solarized-hl))))))
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  :config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom alias
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'sh 'shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global key binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; comment without region
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; M-x == C-x/C-m
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; ibuffer-mode
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") 'copy-whole-lines)

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; (global-set-key (kbd "M-o") 'ace-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :pin org
  :ensure org-plus-contrib
  :bind (
         ;; ("C-x C-m" . helm-M-x)
         ;; ("C-x b" . helm-mini)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package delight
  :ensure t)

;;; Window
(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-mode t))

;; (use-package ace-window
;;   :ensure t
;;   :config)

(use-package counsel
  :ensure t)

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 2)
  (setq which-key-max-description-length 40)
  (setq which-key-max-display-columns nil)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :commands flycheck-mode
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 5.0))

;;; pdf-tools
;; https://github.com/politza/pdf-tools
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install)
  )

;;; https://github.com/ralesi/ranger.el
(use-package ranger
  :ensure t
  :config
  ;; (ranger-override-dired-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programing related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; projectile
;;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode)
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile)
  ;; https://github.com/bbatsov/projectile/issues/1183
  (setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
  (setq projectile-globally-ignored-file-suffixes
        '(".psd" ".png" ".fbx" ".anim" ".mat" ".meta" ".prefab" ".asset"
          ".controller")))

;;; helm
;;; https://github.com/emacs-helm/helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :config
  ;; http://tuhdo.github.io/helm-intro.html 권고에 따라 키 바꿈 C-x C-c 실수에 동의
  (progn
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    (global-set-key (kbd "C-c h s") 'helm-do-ag)
    (global-set-key (kbd "C-c h o") 'helm-occur))
  (setq helm-split-window-inside-p t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-apropos-fuzzy-match t)
  (helm-autoresize-mode 1)

  :init(progn
         (require 'helm-config)
         (helm-mode 1)

         (use-package helm-ag
           :ensure t
           )

         (use-package helm-swoop
           :ensure t
           :bind
           (("M-i" . helm-swoop)
            ("M-I" . helm-swoop-back-to-last-point)
            ("C-c M-i" . helm-multi-swoop)
            ("C-x M-i" . helm-multi-swoop-all)
            :map
            helm-swoop-map
            ("M-i" . helm-multi-swoop-all-from-helm-swoop)
            ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop)))

         (use-package helm-projectile
           :ensure t
           :commands (helm-projectile)
           :config
           (helm-projectile-on)
           )
         )
  )

(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-complete company-mode)
  :bind ( ;;([remap dabbrev-expand] . company-complete)
         :map prog-mode-map
         ([tab] . company-indent-or-complete-common))
  :init (if (fboundp 'evil-declare-change-repeat)
            (mapc #'evil-declare-change-repeat
                  '(company-complete-common
                    company-select-next
                    company-select-previous
                    company-complete-selection
                    company-complete-number)))
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-statistics
    :ensure t
    :init
    (company-statistics-mode))
  (setq company-idle-delay 0)
  (setq company-show-numbers "on")
  (add-hook 'prog-mode-hook 'company-mode))


(use-package magit
  :ensure t
  ;; :config
  ;; (setq magit-git-executable "/usr/libexec/git-core")
  :bind ("C-x g" . magit-status)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programing language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Elpy, the Emacs Lisp Python Environment
;;; elpy https://github.com/jorgenschaefer/elpy
(use-package elpy
  :ensure t
  :config (elpy-enable)
  :init
  ;; For elpy
  ;; (setq elpy-rpc-python-command "python")
  ;; For interactive shell
  ;; (setq python-shell-interpreter "python")
  )

;;; javascript
;;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; Better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

;;; git clone https://github.com/ternjs/tern
;;; npm install
(add-to-list 'load-path "~/.emacs.d/elpa/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)

;;; First, install tern with above code.
(use-package company-tern
  :ensure t
  :init
  (add-to-list 'company-backends 'company-tern))
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;;; Install indium. If indium is not working, check sudo npm install -g indium
(unless (package-installed-p 'indium)
  (package-install 'indium))
(setq indium-chrome-executable "chrome")


;;; Web
(use-package web-mode
  :ensure t
  :init
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-css-indent-offset 4))

  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode)))

(use-package json-mode
  :ensure    t
  :config    (bind-keys :map json-mode-map
                        ("C-c i" . json-mode-beautify))
  :mode      ("\\.\\(json\\)$" . json-mode))

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'"
  :init
  (setq less-css-compile-at-save nil))

(use-package lsp-mode
  :init
  (setq lsp-prefer-flymake nil)
  :ensure t
  :config
  ;; disable weird indentation
  (setq lsp-enable-on-type-formatting nil)
  )


;;; java
;;; lsp-java
(use-package lsp-java
  :ensure t
  :after lsp
  :init (progn
          (require 'cc-mode)
          (use-package projectile :ensure t)
          (use-package yasnippet :ensure t)
          (use-package hydra :ensure t)
          (use-package company-lsp :ensure t)
          (use-package lsp-ui
            :ensure t
            :config
            (setq lsp-ui-doc-enable t
                  lsp-ui-sideline-enable t
                  lsp-ui-flycheck-enable t
                  )
            :bind (:map
                   lsp-ui-mode-map
                   ("C-c C-l" . lsp-ui-sideline-apply-code-actions)
                   ("C-c C-i" . lsp-ui-find-workspace-symbol)
                   )
            :after lsp-mode)
          (use-package dap-mode
            :ensure t
            :after lsp-mode
            :config
            (dap-mode t)
            (dap-ui-mode t))
          (use-package dap-java
            :after (lsp-java)
            )
          )
  :config
  (add-hook 'java-mode-hook 'lsp)
  (require 'lsp-java-boot)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx1G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:/home/jake/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar"
         "-Xbootclasspath/a:/home/jake/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar"
         ))
  )

;; (setq lsp-ui-sideline-update-mode 'point)


;;; clojure
(use-package cider
  :ensure t)


(use-package groovy-mode
  :ensure    t
  :defer     t
  :mode      ("\\.\\(groovy\\|gradle\\)$" . groovy-mode))

(add-hook 'groovy-mode-hook
          (lambda ()
            (c-set-offset 'label 4)))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "/Dockerfile\\'")

;; (use-package kotlin-mode
;;   :ensure t
;; :mode "\\.kt\\'")

;; (use-package js-doc
;;   :ensure t
;;   :bind (:map js2-mode-map
;;               ;("C-c C-@" . js-doc-describe-tag)
;;               ("C-c i"   . js-doc-insert-function-doc)
;; ("@" . js-doc-insert-tag)))

;; (use-package pkgbuild-mode
;;   :ensure t
;;   :mode "/PKGBUILD\\'")

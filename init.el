(defconst local-init-el-path "~/.emacs.d/init.el.local")
(when (file-exists-p local-init-el-path)
  (message (format "load local init el - %s" local-init-el-path))
  (load-file local-init-el-path))

(defun available-font? (font) (member font (font-family-list)))

;; startup-message 안 보기
(setq inhibit-startup-message t)
;; *scratch* 버퍼 깨끗하게 시작하기
;;(setq initial-scratch-message nil)
;; 선택 텍스트를 타이핑할 때, 삭제
(delete-selection-mode t)
;; word-wrap
(global-visual-line-mode t)
;; syntax highlighting on
(global-font-lock-mode t)
;; 커서가 있는 라인 하이라이트
(global-hl-line-mode t)
;; turn on line number
(global-linum-mode t)

(global-auto-revert-mode 1)


(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'sh 'shell)


;; menu bar off
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; key bind.
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

;; comment without region
(global-set-key (kbd "M-;") 'comment-dwim-line)

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



;; (global-set-key (kbd "M-o") 'ace-window)


;; emacs-server
(require 'server)
(server-start)


;; 한글
(set-language-environment "Korean")
;;; 날짜 표시를 영어로하려고
(setq system-time-locale "C")


;;;; encoding.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;;;; univesal coding system
(defvar universal-coding-system-env-list '("PYTHONIOENCODING")
  "List of environment variables \\[universal-coding-system-argument] should set")

(defadvice universal-coding-system-argument (around provide-env-handler activate)
  "Augments \\[universal-coding-system-argument] so it also sets environment variables

Naively sets all environment variables specified in
`universal-coding-system-env-list' to the literal string
representation of the argument `coding-system'.

No guarantees are made that the environment variables set by this advice support
the same coding systems as Emacs."
  (let ((process-environment (copy-alist process-environment)))
    (dolist (extra-env universal-coding-system-env-list)
      (setenv extra-env (symbol-name (ad-get-arg 0))))
    ad-do-it))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;;; emacs가 init.el에 추가하는 설정 방지
;;; (custom-set-variables ...
;;; https://jamiecollinson.com/blog/my-emacs-config/
(setq custom-file (make-temp-file "emacs-custom"))

;; packages
;; Set up package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
                                        ;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-milkbox" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; theme
;; https://github.com/bbatsov/solarized-emacs
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

;; backup
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

(global-set-key (kbd "C-c n") 'cleanup-buffer)

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

;; (add-hook
;;  'after-change-major-mode-hook
;;  '(lambda ()
;;     (if (derived-mode-p 'prog-mode)
;;         (setq whitespace-line-column
;;               100
;;               whitespace-style
;;               '(face tabs trailing lines-tail tab-mark)))))


;; disable tabs mode
(setq-default indent-tabs-mode nil)


;;;; Emacs extend ;;;;

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

;;;smart-mode-line
;;(setq sml/no-confirm-load-theme t)
;;(setq sml/show-eol t) ;; show end-of-line. ex) CRLF(dos)
;;(setq sml/theme 'respectful)
;;(sml/setup)
;;(add-to-list 'sml/replacer-regexp-list '("^c:/work/" ":Dev:") t)

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
  (require 'helm-config)
  ;; http://tuhdo.github.io/helm-intro.html 권고에 따라 키 바꿈 C-x C-c 실수에 동의
  (progn
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c")))
  (progn
    (global-set-key (kbd "C-c h s") 'helm-do-ag)
    (global-set-key (kbd "C-c h o") 'helm-occur))

  (setq helm-split-window-inside-p t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-apropos-fuzzy-match t)
  (helm-autoresize-mode 1)

  (helm-mode 1))

;;; https://github.com/syohex/emacs-helm-ag
;;; prerequsite
;;; window 에서 pacman -Ss silver
;;; window 에서 pacman -s mingw-w64-x86_64-ag
(use-package helm-ag
  :ensure t
  :config
  )

;;; https://github.com/ShingoFukuyama/helm-swoop
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

;;; https://github.com/bbatsov/helm-projectile
(use-package helm-projectile
  :ensure t
  :commands (helm-projectile)
  :after helm
  :config
  (helm-projectile-on)
  )


(use-package counsel
  :ensure t)

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode))


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


(use-package org
  :pin org
  :ensure org-plus-contrib
  :bind (
         ;; ("C-x C-m" . helm-M-x)
         ;; ("C-x b" . helm-mini)
         ))


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
(setq js-indent-level 2)
;;; git clone https://github.com/ternjs/tern
;;; npm install
(add-to-list 'load-path "~/.emacs.d/elpa/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(use-package tern-auto-complete
  :ensure t
  :init)

;;; First, install tern with above code.
(use-package company-tern
  :ensure t
  :init
  (add-to-list 'company-backends 'company-tern))

;;; Web
(use-package web-mode
  :ensure t
  :init
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2))

  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode)))

;;; java
;; lsp-java
(require 'cc-mode)

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(use-package projectile :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode
  :init
  (setq lsp-prefer-flymake nil)
  :ensure t
  )
(use-package hydra :ensure t)
(use-package company-lsp :ensure t)
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-ui-flycheck-enable t)
  :after lsp-mode)
(use-package lsp-java
  :ensure t
  :after lsp
  :config
  (add-hook 'java-mode-hook 'lsp)
  )

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))
(use-package dap-java :after (lsp-java))

(require 'lsp-java-boot)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq lsp-java-vmargs
      (list
       "-noverify"
       "-Xmx1G"
       "-XX:+UseG1GC"
       "-XX:+UseStringDeduplication"
       "-javaagent:/home/jake/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar"
       "-Xbootclasspath/a:/home/jake/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar"
       ))
;; (setq lsp-ui-sideline-update-mode 'point)
(setq lsp-enable-on-type-formatting nil)

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
(add-hook 'java-mode-hook 'flycheck-mode)


;; key binding
(require 'lsp-ui)
(define-key lsp-ui-mode-map (kbd "C-c C-l") 'lsp-ui-sideline-apply-code-actions)
(define-key lsp-ui-mode-map (kbd "C-c C-i") 'lsp-ui-find-workspace-symbol)

;;; clojure
(use-package cider
  :ensure t)

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

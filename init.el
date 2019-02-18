(defconst local-init-el-path "~/.emacs.d/init.el.local")
(when (file-exists-p local-init-el-path)
  (message (format "load local init el - %s" local-init-el-path))
  (load-file local-init-el-path))

(setq windows? (eq system-type 'windows-nt))
(setq mac? (eq system-type 'darwin))

(defun available-font? (font) (member font (font-family-list)))

;; startup-message 안 보기
(setq inhibit-startup-message t)
;; *scratch* 버퍼 깨끗하게 시작하기
;;(setq initial-scratch-message nil)
;; 선택 텍스트를 타이핑할 때, 삭제
(delete-selection-mode t)
;; 라인 넘버 보기
;; (global-linum-mode t)
;; 컬럼 넘버 보기
(setq column-number-mode t)
;; word-wrap
(global-visual-line-mode t)
;; syntax highlighting on
(global-font-lock-mode t)
;; 커서가 있는 라인 하이라이트
(global-hl-line-mode t)

(global-auto-revert-mode 1)

;; menu bar off
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;;; key bind.
;; M-x == C-x/C-m
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;;; ibuffer-mode
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; (global-set-key (kbd "M-o") 'ace-window)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'sh 'shell)
;;; emacs-server
(require 'server)
(server-start)


;; | 12345678 |   |
;; |----------+---|
;; | 일이삼사 |   |
(when mac?
  ;; font
  (when (available-font? "Consolas")
    (set-frame-font "Consolas-15" nil t)
    (set-fontset-font t 'hangul (font-spec :name "PCMyungjo-16"))
    (setq-default line-spacing 2))

  ;; keybinding
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; | 12345678 |   |
;; |----------+---|
;; | 일이삼사 |   |
(when windows?
  (when (available-font? "Consolas")
    (set-frame-font "Consolas-13" nil t)
    (set-fontset-font t 'hangul (font-spec :name "Batangche"))
    (setq-default line-spacing 3)))

;; 한글
(set-language-environment "Korean")
;; 날짜 표시를 영어로하려고
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


;;;; emacs가 init.el에 추가하는 설정 방지
;;; (custom-set-variables ...
;;; https://jamiecollinson.com/blog/my-emacs-config/
(setq custom-file (make-temp-file "emacs-custom"))

;;;; packages
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/")
;;              t)
;; (add-to-list 'package-archives
;;              '("org" . "http://orgmode.org/elpa/")
;;              t)

;;; Set up package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/")
             t)


(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;;; theme
;; https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :ensure t
  :init
  ;; org에서 커진 한글 폰트가 너무 안 예뻐서
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

;;;; shell
(when windows?
  (let* ((combine-path (lambda (dir dir-or-file)
                         (concat (file-name-as-directory dir) dir-or-file)))
         ;; (base-dir "C:/git-sdk-64")
         (base-dir "c:/msys64")
         (mingw64-bin-dir (funcall combine-path base-dir "mingw64/bin"))
         (msys2-bin-dir (funcall combine-path base-dir "usr/bin"))
         (bash-path (funcall combine-path msys2-bin-dir "bash.exe")))
    (add-to-list 'exec-path msys2-bin-dir)
    (add-to-list 'exec-path mingw64-bin-dir)
    (setq explicit-shell-file-name bash-path)
    ;; (setq shell-file-name bash-path)
    (setq shell-file-name "bash")
    (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
    (setenv "SHELL" bash-path)
    (setenv "PATH" (concat mingw64-bin-dir path-separator
                           (concat msys2-bin-dir path-separator
                                   (getenv "PATH"))))
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
    )
  )

;;;; backup
(add-to-list 'backup-directory-alist '("." . "~/.emacs-saves"))
(setq delete-old-versions t
      kept-old-versions 2
      kept-new-versions 2
      version-control t)

;;;; whitespace mode
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
    (if (derived-mode-p 'prog-mode)
        (setq whitespace-line-column
              100
              whitespace-style
              '(face tabs trailing lines-tail tab-mark)))))
        ;; (setq whitespace-line-column
        ;;       nil
        ;;       whitespace-style
        ;;       '(face tabs trailing tab-mark)))))

;;; disable tabs mode
(setq-default indent-tabs-mode nil)




;;;; Emacs extend ;;;;

(use-package delight
  :ensure t)

;;;; Window
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


;;;; helm
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

  ;; windows에서만 문제가 발생
  (when windows?
    ;; mingw64/mingw-w64-x86_64-ag 2.0.0.r1912.ccdbe93-1 사용시
    ;; 한글 검색이 안 된다. grep, rip 모두 잘 되는 걸로 봐서는 패키지를 의심
    ;; helm-ag 패키지로도 사용할 수 있는 ripgrep을 사용한다.
    ;; https://github.com/BurntSushi/ripgrep
    ;; macOS에서도 ag 대신 ripgrep을 사용할지는 고민 중.
    (setq helm-ag-base-command "rg -i --no-heading --vimgrep")

    ;; helm-do-ag 처럼 process로 한글 인자를 넘길 때, encoding 문제를 해결하기 위해
    ;; 내부 동작을 정확히 파악하지 못했다.
    ;;
    ;; cp949일 때
    ;; - (korean-iso-8bit-dos . korean-iso-8bit-unix)
    ;; - 출력은 깨지지만 입력은 process로 제대로 전달된다.
    ;; utf-8일 때
    ;; - (utf-8-dos . utf-8-unix)
    ;; - 입력은 깨지지만 출력은 제대로 된다.
    ;;
    ;; 둘을 조합했다.
    ;; 다른 건 utf-8로 잘 동작하니 helm-do-ag 실행할 때만 프로세스 인코딩을 변경한다
    (advice-add 'helm-do-ag
                :before (lambda (&rest _)
                          (setq default-process-coding-system
                                '(utf-8-dos . korean-iso-8bit-unix))))
    (advice-add 'helm-do-ag
                :after (lambda (&rest _)
                         (setq default-process-coding-system
                               '(utf-8-dos . utf-8-unix))))))

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


;;;; projectile
;;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
        '(".psd" ".png" ".fbx" ".anim" ".mat" ".meta" ".prefab" ".asset"
          ".controller")))

;;; https://github.com/bbatsov/helm-projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)

  ;; windows에서는 ag대신 ripgrep을 사용.
  ;; --ignore 옵션이 하드코딩돼서 ripgrep을 사용 못함
  ;; projectile 0.14.0
  (when windows?
    (advice-add 'helm-do-ag
                :before (lambda (&rest _)
                          (setq helm-ag-base-command
                                (replace-regexp-in-string
                                 "--ignore.*"
                                 ""
                                 helm-ag-base-command))))))


(use-package counsel
  :ensure t)

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-show-numbers "on"))

(use-package yasnippet :ensure t)

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
  :init (global-flycheck-mode))

;; (use-package flycheck-package
;;   :ensure t
;;   :init
;;   :config
;;   (flycheck-package-setup))


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


;;;; javascript
;; (setq js-indent-level 4)

;; (defun eslint-fix ()
;;   "Format the current file with ESLint."
;;   (interactive)
;;   (let ((eslint (or (shiren/use-eslint-from-node-modules) (executable-find "eslint"))))
;;     (if (file-executable-p eslint)
;;         (progn (call-process eslint nil "*ESLint Errors*" nil "--rule" "no-debugger:0" "--fix" buffer-file-name)
;;                (revert-buffer t t t))
;;       (message "ESLint not found."))))

;; (use-package js2-mode
;;   :ensure t
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;   (add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
;;   (add-hook 'js2-mode-hook
;;           '(lambda ()
;;              (js2-imenu-extras-mode)))
;;   :config
;;   (define-key js2-mode-map (kbd "M-.") nil)
;;   (define-key js2-mode-map (kbd "C-c C-j") nil)
;;   (setq js2-include-node-externs t)
;;   (setq js2-pretty-multiline-declarations nil)
;;   (add-hook 'js2-mode-hook (lambda ()
;;                              ;;(add-hook 'after-save-hook 'eslint-fix nil t)
;;                              (setq tab-width 2)
;;                              (setq-default js2-basic-offset 2)
;;                              (js2-imenu-extras-mode)))
;;   (setq-default js2-basic-offset 2
;;                 js1-bounce-indent-p nil)
;;   (setq-default js2-mode-show-parse-errors nil
;;                 js2-mode-show-strict-warnings nil))

;; (use-package lsp-javascript-typescript
;;   :ensure-system-package
;;   (javascript-typescript-langserver . "npm i -g javascript-typescript-langserver")
;;   :ensure t
;;   :init
;;   (defun my-company-transformer (candidates)
;;     (let ((completion-ignore-case t))
;;       (all-completions (company-grab-symbol) candidates)))

;;   (defun my-js-hook nil
;;     (make-local-variable 'company-transformers)
;;     (push 'my-company-transformer company-transformers))

;;   (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
;;   (add-hook 'js-mode-hook 'my-js-hook)
;;   (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
;;   (add-hook 'js2-mode-hook 'my-js-hook)
;;   (add-hook 'rjsx-mode-hook #'lsp-javascript-typescript-enable)
;;   (add-hook 'rjsx-mode-hook 'my-js-hook))



;; (use-package tern
;;   :ensure t
;;   :init
;;   (autoload 'tern-mode' "tern.el" nil t)
;;   (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;;   ;; (add-hook 'rjsx-mode-hook (lambda () (tern-mode t)))
;;   ;; :config
;;   ;; (define-key tern-mode-keymap (kbd "C-c C-r") nil)
;;   ;; ;; (define-key tern-mode-keymap (kbd "M-.") nil)
;;   ;; ;; (define-key tern-mode-keymap (kbd "M-,") nil)
;;   ;; (setq tern-command '("tern" "--no-port-file"))
;;   )

;; (use-package tern-auto-complete
;;   :ensure t
;;   :init)

;;; 위에 use-package 를 사용해서 package를 설치하고
;;; 아래 코드로 돌리니까 auto completion 이 되기 시작.
;;; 아래 코드 돌리기 전에 해당 디렉토리 안에서
;;; git clone https://github.com/ternjs/tern
;;; npm install
(add-to-list 'load-path "~/node_modules/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))


(use-package company-tern
  :ensure t
  :init
  (add-to-list 'company-backends 'company-tern))

(use-package js-doc
  :ensure t
  :bind
  (:map js2-mode-map
        ("\C-cd" . js-doc-insert-function-doc)
        ("@" . js-doc-insert-tag))
  ;; :config
  ;; (setq js-doc-mail-address "shirenbeat@gmail.com"
  ;;     js-doc-author (format "Sungho Kim <%s>" js-doc-mail-address)
  ;;     js-doc-url "shiren.github.io"
  ;;     js-doc-license "MIT")
)


;;;; Web
(use-package web-mode
  :ensure t
  :init
  ;; (defun my-web-mode-hook ()
  ;;   "Hooks for Web mode."
  ;;   (setq web-mode-markup-indent-offset 2)
  ;;   (setq web-mode-code-indent-offset 2)
  ;;   (setq web-mode-css-indent-offset 2))

  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode)))

;;;; java
(use-package meghanada
  :ensure t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              ;; meghanada-mode on
              (meghanada-mode t)
              ;; (flycheck-mode +1)
              (flycheck-mode t)
              ;; (setq c-basic-offset 2)
              ;;; use code format
              ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
              ))
  :config
  (when windows?
   (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
   (setq meghanada-maven-path "mvn.cmd")
  (unless
   (setq meghanada-java-path "java")
   (setq meghanada-maven-path "mvn"))))


;;;; clojure
(use-package cider
  :ensure t)


;;;; pdf-tools
;; 1.Open msys2 shell
;; 2.Update and install dependencies, skipping any you already have
;;   $ pacman -Syu
;;   $ pacman -S base-devel
;;   $ pacman -S mingw-w64-x86_64-toolchain
;;   $ pacman -S mingw-w64-x86_64-zlib
;;   $ pacman -S mingw-w64-x86_64-libpng
;;   $ pacman -S mingw-w64-x86_64-poppler
;;   $ pacman -S mingw-w64-x86_64-imagemagick
;; 3.M-x pdf-tools-install RET
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install)
  )

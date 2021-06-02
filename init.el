;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; emacs-server
(require 'server)
(server-start)

;;; date expression in English
(setq system-time-locale "C")

;;; encoding.
(set-language-environment "Korean")
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system  'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; (unless (eq system-type 'windows-nt)
;;   (set-selection-coding-system 'utf-8))


;;; emacs가 init.el에 추가하는 설정 방지
;;; (custom-set-variables ...
;;; https://jamiecollinson.com/blog/my-emacs-config/
(setq custom-file (make-temp-file "emacs-custom"))

;;; Bootstrap straight.el
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; packages
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-milkbox" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;; backup
(add-to-list 'backup-directory-alist '("." . "~/.config/.emacs-saves"))
(setq delete-old-versions t
      kept-old-versions 2
      kept-new-versions 2
      version-control t)

;;; font
(defun available-font? (font) (member font (font-family-list)))
(when (available-font? "Consolas")
  (set-frame-font "Consolas-15" nil t)
  (set-fontset-font t 'hangul (font-spec :name "PCMyungjo-16"))
  (setq-default line-spacing 2))

;;; whitespace mode
;; (global-whitespace-mode t)

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(whitespace-line ((nil (:bold t :background "yellow"))))
;;  '(whitespace-tab ((nil (:bold t :background "linen"))))
;;  '(whitespace-trailing ((nil (:bold t :background "red1")))))

;; (add-hook
;;  'after-change-major-mode-hook
;;  '(lambda ()
;;     (setq whitespace-line-column 150;;nil
;;           whitespace-style '(face tabs trailing lines-tail tab-mark))))

;; (add-hook 'before-save-hook 'cleanup-buffer)

;;; disable tabs mode
(setq-default indent-tabs-mode nil)

;; startup-message 안 보기
(setq inhibit-startup-message t)
;; *scratch* 버퍼 깨끗하게 시작하기
(setq initial-scratch-message nil)
;; 선택 텍스트를 타이핑할 때, 삭제
(delete-selection-mode t)
;; word-wrap
(global-visual-line-mode t)

;; beep sound off
(setq visible-bell 1)

;;; environment variables
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  )
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq global-line-num 0)
(setq my-github-url "https://github.com/inhobaek-wj")
(setq work-git-url "http://git.aiotholdings.com")
(setq redmine-url "http://redmine.aiotholdings.com")
(setq mattermost-url "http://mattermost.aiotholdings.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun goto-code-from-error ()
  (interactive)

  ;; TODO: check if this function is called from compilation buffer.
  ;; TODO: check current line is valid.
  ;; TODO: highlighting compilation buffer

  (setq current-line
        (string-trim
         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

  (setq path-and-num (split-string current-line "("))
  (setq class-path-list (split-string (pop path-and-num) "\\."))
  (nbutlast class-path-list 1)
  (setq class-path (mapconcat `identity class-path-list "."))

  (setq text-with-line-num (pop path-and-num))

  (setq global-line-num
        (string-to-number
         (when (string-match "[0-9]+" text-with-line-num)
           (match-string 0 text-with-line-num))
         )
        )

  (minibuffer-with-setup-hook
      (lambda () (insert class-path))
    (call-interactively #'projectile-find-file-other-window))

  )

(defun goto-global-linum ()
  (interactive)
  (goto-line global-line-num)
  )

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
  (indent-buffer)

  (if (eq major-mode 'go-mode)
      (gofmt)
    )
  )

(defun browse-url-to-my-github ()
  (interactive)
  (browse-url my-github-url)
  )

(defun browse-url-to-work-github ()
  (interactive)
  (browse-url work-git-url)
  )

(defun browse-url-to-my-local8100 ()
  (interactive)
  (browse-url my-local8100-url)
  )

(defun browse-url-to-redmine ()
  (interactive)
  (browse-url redmine-url)
  )

(defun browse-url-to-mattermost ()
  (interactive)
  (browse-url mattermost-url)
  )

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
  ;; (load-theme 'solarized-light 'NO-CONFIRM)
  (defconst my/solarized-light-red "#FF6E64")
  (defconst my/solarized-light-green "#B4C342")
  (defconst my/solarized-light-orange "#F2804F")
  (defconst my/solarized-base2 "#EEE8D5")
  (defconst my/solarized-hl "#EEEED5")
  ;; (progn
  ;;   (custom-theme-set-faces
  ;;    'solarized-light
  ;;    `(hl-line
  ;;      ((t (:background ,my/solarized-hl))))))
  )

(use-package color-theme-sanityinc-tomorrow
  :defer t)

(use-package heroku-theme
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom alias
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'sh 'shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global key binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; korean input
(global-set-key (kbd "S-SPC") 'toggle-korean-input-method)

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

;; for testing and debuging
(global-set-key (kbd "C-c d c") 'dap-java-run-test-class)
(global-set-key (kbd "C-c d m") 'dap-java-run-test-method)
(global-set-key (kbd "M-g l") 'goto-global-linum)
(global-set-key (kbd "M-g e") 'goto-code-from-error)

;; custom browse url
(global-set-key (kbd "C-c C-b m") 'browse-url-to-my-github)
(global-set-key (kbd "C-c C-b w") 'browse-url-to-work-github)
(global-set-key (kbd "C-c C-b l") 'browse-url-to-my-local8100)
(global-set-key (kbd "C-c C-b r") 'browse-url-to-redmine)
(global-set-key (kbd "C-c C-b t") 'browse-url-to-mattermost)

;; switch major mode
(global-set-key (kbd "C-c c v") 'vue-mode)
(global-set-key (kbd "C-c c w") 'web-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package org
;;   :pin org
;;   :ensure org-plus-contrib
;;   :bind (
;;          ;; ("C-x C-m" . helm-M-x)
;;          ;; ("C-x b" . helm-mini)
;;          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  ;; :bind
  ;; (("C-c n" . mc/mark-next-like-this)
  ;;  ("C-c p" . mc/mark-previous-like-this))
  )

(use-package autopair
  :ensure t
  :init
  (autopair-global-mode)
  )
;; (use-package smartparens
;;   :defer)

(use-package delight
  :ensure t)

;;; Window
(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-mode t))

(use-package ace-window
  :ensure t
  :init
  (setq aw-scope 'frame ; limit to single frame (useful when using exwm)
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window))

(use-package windmove
  :ensure t
  :demand
  :bind
  (("C-s-n" . windmove-down)
   ("C-s-p" . windmove-up)
   ("C-s-b" . windmove-left)
   ("C-s-f" . windmove-right)
   ("C-s-j" . windmove-down)
   ("C-s-k" . windmove-up)
   ("C-s-h" . windmove-left)
   ("C-s-l" . windmove-right))
  :config
  (windmove-default-keybindings))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programing related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-complete
  :ensure t)

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
        '(".psd" ".png" ".fbx" ".anim" ".mat" ".meta" ".prefab" ".asset" ".min.js"
          ".controller" ".jpg"))
  (setq projectile-globally-ignored-directories
        (append '(".DS_Store" ".git" ".svn" "out" "repl" "target" "dist" "lib" "node_modules" "libs" "deploy" "coverage" ".nuxt")
                projectile-globally-ignored-directories))
  (setq grep-find-ignored-directories (append '("dist" "deploy" "node_modules" "coverage" ".nuxt") grep-find-ignored-directories))
  :bind
  ("C-c p f" . projectile-find-file)
  ("C-c p 4 f" . projectile-find-file-other-window)
  ("C-c p b" . projectile-switch-to-buffer)
  ("C-c p 4 b" . projectile-switch-to-buffer-other-window)
  ("C-c p D" . projectile-dired)
  ("C-c p d" . projectile-find-dir)
  ("C-c p j" . projectile-find-tag)
  ("C-c p r" . projectile-replace)
  ("C-c p o" . projectile-multi-occur)
  ("C-c p s s" . counsel-projectile-ag)
  ("C-c C-g" . counsel-projectile-rg)
  ("C-c p I" . projectile-ibuffer)
  ("C-c p p" . projectile-switch-project)
  )

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
  :bind (:map prog-mode-map
              ([tab] . company-indent-or-complete-common))

  :hook
  (after-init . global-company-mode)
  (prog-mode . company-mode)

  :config
  (use-package company-statistics
    :ensure t
    :init
    (company-statistics-mode))
  (setq company-idle-delay 0.3)
  (setq company-show-numbers "on")
  (setq company-minimum-prefix-length 1)
  )

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  ;;; 이맥스가 기본적으로 제공하는 Git 백엔드를 켜두면 매우 느려진다. magit만 쓴다.
  (setq vc-handled-backends nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programing language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Elpy, the Emacs Lisp Python Environment
;;; elpy https://github.com/jorgenschaefer/elpy
(use-package elpy
  :ensure t
  :init (elpy-enable)
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
  (add-to-list 'auto-mode-alist '("\\.babelrc\\'" . js2-mode))
  (setq js2-basic-offset 2)

  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode) ;; better imenu.
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode)
                             (company-mode)))
  )

;;; git clone https://github.com/ternjs/tern
;;; npm install
;; (add-to-list 'load-path "~/.emacs.d/elpa/tern/emacs/")
;; (autoload 'tern-mode "tern.el" nil t)

;;; First, install tern with above code.
;; (use-package company-tern
;;   :ensure t
;;   :init (add-to-list 'company-backends 'company-tern)
;;   )

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
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-style-padding  0
          web-mode-script-padding 0)
    )

  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  )

(use-package json-mode
  :ensure    t
  :config    (bind-keys :map json-mode-map
                        ("C-c i" . json-mode-beautify))
  (setq json-indent-offset 2)
  :mode      ("\\.\\(json\\)$" . json-mode))

(use-package rainbow-mode
  :ensure t
  :hook (css-mode . rainbow-mode))

(use-package lsp-mode
  :init
  (setq lsp-prefer-flymake nil)
  :ensure t
  :commands lsp
  :config
  ;; disable weird indentation
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  )

(use-package yasnippet
  :ensure t
  :init (yas-reload-all)
  :hook (prog-mode . yas-minor-mode)
  )

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable t
        lsp-ui-flycheck-enable t
        ;; lsp-ui-sideline-update-mode 'point
        )
  :bind (:map
         lsp-ui-mode-map
         ("C-c C-l" . lsp-ui-sideline-apply-code-actions)
         ("C-c C-i" . lsp-ui-find-workspace-symbol)
         ("C-c C-g" . lsp-ui-doc-glance)
         )
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  )

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  )

;; (straight-use-package '(company-lsp :type git :host github :repo "tigersoldier/company-lsp"))
(use-package company-lsp
  :straight (company-lsp :type git :host github :repo "tigersoldier/company-lsp")
  :ensure t
  :init
  (setq company-lsp-cache-candidates 'auto
        company-lsp-filter-candidates t)
  (setq company-lsp-async t)
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-enable-recompletion t)
  )
;; (push 'company-lsp company-backends) 관련 내용은 아래 링크 참고
;; https://emacs.stackexchange.com/questions/51812/company-lsp-complains-about-void-function-lsp-client-completion-in-comments

;;; java
;;; lsp-java
(use-package lsp-java
  :ensure t
  :after lsp
  :init (progn
          (require 'cc-mode)
          (use-package projectile :ensure t)
          (use-package hydra :ensure t)
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
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx1G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         ;; "-javaagent:/Users/inho/.m2/repository/org/projectlombok/lombok/1.18.10/lombok.jar"
         ;; "-Xbootclasspath/a:/Users/inho/.m2/repository/org/projectlombok/lombok/1.18.10/lombok.jar"
         )
        )
  (setq lsp-java-server-install-dir "/Users/inho/.emacs.d/eclipse.jdt.ls/server/")

  (add-hook 'java-mode-hook 'lsp)
  (require 'lsp-java-boot)
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (add-hook 'java-mode-hook (lambda ()
                              (setq c-basic-offset 4)))
  )


;;; clojure
(use-package cider
  :ensure t)

(use-package groovy-mode
  :ensure    t
  :defer     t
  :mode      ("\\.\\(groovy\\|gradle\\)$" . groovy-mode)
  :hook(groovy-mode . (lambda ()
                        (c-set-offset 'label 4)))

  )

;; (use-package js-doc
;;   :ensure t
;;   :bind (:map js2-mode-map
;;               ;("C-c C-@" . js-doc-describe-tag)
;;               ("C-c i"   . js-doc-insert-function-doc)
;; ("@" . js-doc-insert-tag)))


;;; loading my  configuration
(add-to-list 'load-path "~/.emacs.d/modules/")

(require 'lang-vue)
(require 'lang-go)
(require 'lang-flutter)
(require 'text-file-mode)

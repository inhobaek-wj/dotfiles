;;; -*- lexical-binding: t -*-
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

;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
;; (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;;; emacs가 init.el에 추가하는 설정 방지
;;; (custom-set-variables ...
;;; https://jamiecollinson.com/blog/my-emacs-config/
(setq custom-file (make-temp-file "emacs-custom"))

;;; packages
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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
;; autocomplete paired
(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\{ . ?\})
        (?\' . ?\')
        (?\< . ?\>)
        (?\` . ?\`)
        )
      )

;;; environment variables
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; LIBRARY_PATH for native-comp is set in early-init.el
(when (eq system-type 'darwin) (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq global-line-num 0)

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
  (if (eq major-mode 'js2-mode)
      (lsp-format-buffer)
    )
  )

(defun browse-url-to-barae-karate-test-summary ()
  (interactive)
  (browse-url "file:///Users/inho/Workspaces/barea/barae21API/target/karate-reports/karate-summary.html")
  )

(defun browse-url-to-correct-korean ()
  (interactive)
  (browse-url "http://speller.cs.pusan.ac.kr/")
  )


;; move line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

;; move line down
(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(defun make-test-file ()
  "Find test file or make it"
  (interactive)

  (setq major-mode-to-string (format "%s" major-mode))

  (setq buffer-name-with-file-type (split-string (buffer-name) "\\."))
  (setq only-buffer-name (pop buffer-name-with-file-type))

  (if (eq major-mode 'go-mode)
      (minibuffer-with-setup-hook
          (lambda () (insert only-buffer-name "_test.go"))
        (call-interactively #'find-file-other-window))
    )

  (if (eq major-mode 'js2-mode)
      (minibuffer-with-setup-hook
          (lambda () (insert "__test__/" only-buffer-name ".test.js"))
        (call-interactively #'find-file-other-window))
    )

  (if (eq major-mode 'js-mode)
      (minibuffer-with-setup-hook
          (lambda () (insert "__test__/" only-buffer-name ".test.js"))
        (call-interactively #'find-file-other-window))
    )

  (if (eq major-mode 'css-mode)
      (minibuffer-with-setup-hook
          (lambda () (insert "__test__/" only-buffer-name ".test.js"))
        (call-interactively #'find-file-other-window))
    )

  (if (string-match "vue" major-mode-to-string)
      (minibuffer-with-setup-hook
          (lambda () (insert "__test__/" only-buffer-name ".test.js"))
        (call-interactively #'find-file-other-window))
    )
  )

(defun find-package-json-root ()
  "Find the nearest directory containing package.json"
  (let ((current-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (locate-dominating-file current-dir "package.json")))

(defun run-js-test-file ()
  "Run tests for the current file"
  (interactive)
  (let* ((package-root (find-package-json-root))
         (file-name (buffer-file-name))
         (relative-path (file-relative-name file-name package-root))
         (is-cypress (string-match-p "\\.cy\\.[jt]sx?$" file-name))
         (default-directory package-root)
         (test-command (if is-cypress
                           (format "npm run test:e2e -- --spec '%s'" relative-path)
                         (format "npm test -- %s" relative-path))))
    (compile test-command)))

(defun run-js-test-all ()
  "Run all tests in the project"
  (interactive)
  (let* ((default-directory (find-package-json-root))
         (file-name (buffer-file-name))
         (is-cypress (and file-name (string-match-p "\\.cy\\.[jt]sx?$" file-name)))
         (test-command (if is-cypress "npm run test:e2e" "npm test")))
    (compile test-command)))

(defun run-cypress-test-all ()
  "Run all Cypress e2e tests in the project"
  (interactive)
  (let ((default-directory (find-package-json-root)))
    (compile "npm run test:e2e")))

(defun toggle-test-only ()
  "Toggle .only() on the current describe or it block"
  (interactive)
  (save-excursion
    (let ((line-text (thing-at-point 'line t)))
      (beginning-of-line)
      (if (string-match "\\(describe\\|it\\|test\\)\\(\\.only\\)?(" line-text)
          (if (match-string 2 line-text)
              ;; Remove .only
              (progn
                (search-forward ".only" (line-end-position) t)
                (replace-match ""))
            ;; Add .only
            (progn
              (search-forward-regexp "\\(describe\\|it\\|test\\)(" (line-end-position) t)
              (goto-char (match-beginning 0))
              (search-forward-regexp "\\(describe\\|it\\|test\\)" (line-end-position) t)
              (insert ".only")))
        (message "Not on a describe/it/test line")))))

(defun run-current-test-block ()
  "Add .only to current test block, run tests, then remove .only when compilation finishes"
  (interactive)
  (let ((original-point (point))
        (target-buffer (current-buffer))
        (modified nil))
    (save-excursion
      ;; Find the current describe or it block
      (beginning-of-line)
      (unless (looking-at ".*\\(describe\\|it\\|test\\)(")
        (search-backward-regexp "^[[:space:]]*\\(describe\\|it\\|test\\)(" nil t))

      ;; Add .only if not already there
      (let ((line-text (thing-at-point 'line t)))
        (when (and (string-match "\\(describe\\|it\\|test\\)(" line-text)
                   (not (string-match "\\.only(" line-text)))
          (beginning-of-line)
          (search-forward-regexp "\\(describe\\|it\\|test\\)(" (line-end-position) t)
          (goto-char (match-beginning 0))
          (search-forward-regexp "\\(describe\\|it\\|test\\)" (line-end-position) t)
          (insert ".only")
          (setq modified t))))

    ;; Save and run tests
    (when modified
      (save-buffer)

      ;; Set up a one-time hook to remove .only when compilation finishes
      (let ((buf target-buffer)
            (pt original-point)
            (cleanup-fn nil))
        (setq cleanup-fn
              (lambda (buffer status)
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (save-excursion
                      (goto-char pt)
                      (beginning-of-line)
                      (unless (looking-at ".*\\(describe\\|it\\|test\\)\\.only(")
                        (search-backward-regexp "^[[:space:]]*\\(describe\\|it\\|test\\)\\.only(" nil t))
                      (when (search-forward ".only" (line-end-position) t)
                        (replace-match "")
                        (save-buffer)))))
                ;; Remove this hook after running once
                (remove-hook 'compilation-finish-functions cleanup-fn)))

        (add-hook 'compilation-finish-functions cleanup-fn)
        (run-js-test-file)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; syntax highlighting on
(global-font-lock-mode t)
;; 커서가 있는 라인 하이라이트
(global-hl-line-mode t)
;; turn on line number
;; (global-linum-mode t)
(global-display-line-numbers-mode t)

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
  :ensure t)

(use-package heroku-theme
  :ensure t)

(use-package material-theme
  :ensure t)

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

;; ibuffer-mode
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\C-w" 'backward-kill-word)

;; Smart kill-region that uses kill-ring-save in read-only buffers
(defun my-smart-kill-region (beg end &optional region)
  "Kill region, but use kill-ring-save in read-only buffers."
  (interactive (list (mark) (point) 'region))
  (if buffer-read-only
      (kill-ring-save beg end region)
    (kill-region beg end region)))

(global-set-key "\C-x\C-k" 'my-smart-kill-region)
(global-set-key "\C-c\C-k" 'my-smart-kill-region)

(global-set-key (kbd "C-x m") 'kmacro-keymap)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") 'copy-whole-lines)

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; for testing and debuging in Java
(global-set-key (kbd "C-c d c") 'dap-java-run-test-class)
(global-set-key (kbd "C-c d m") 'dap-java-run-test-method)
(global-set-key (kbd "M-g l") 'goto-global-linum)
(global-set-key (kbd "M-g e") 'goto-code-from-error)

;; custom browse url
(global-set-key (kbd "C-c e b") 'browse-url-to-barae-karate-test-summary)
(global-set-key (kbd "C-c e c") 'browse-url-to-correct-korean)

;; switch major mode
(global-set-key (kbd "C-c v v") 'vue-mode)
(global-set-key (kbd "C-c v w") 'web-mode)

;; move one line
(global-set-key [(control shift n)] 'move-line-down)
(global-set-key [(control shift p)] 'move-line-up)

;;
(global-set-key (kbd "C-c m t") 'make-test-file)

;; run tests
(global-set-key (kbd "C-c t f") 'run-js-test-file)
(global-set-key (kbd "C-c t a") 'run-js-test-all)
(global-set-key (kbd "C-c t c") 'run-cypress-test-all)
(global-set-key (kbd "C-c t t") 'run-current-test-block)
(global-set-key (kbd "C-c t o") 'toggle-test-only)


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
  :ensure t
  ;; :bind
  ;; (("C-c n" . mc/mark-next-like-this)
  ;;  ("C-c p" . mc/mark-previous-like-this))
  )

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

(use-package string-inflection
  :ensure t
  :init
  ;; C-q C-u is similar to the keybinding used by Vz Editor.
  (global-unset-key (kbd "C-q"))
  :bind
  ("C-q u" . string-inflection-underscore)
  ("C-q l" . string-inflection-lower-camelcase)
  ("C-q c" . string-inflection-camelcase)
  ("C-q p" . string-inflection-pascal-case)
  ("C-q k" . string-inflection-kebab-case)
  )

;; C-x b(helm-mini)와 같은 버퍼 목록을 Ibuffer에서도 보이게 설정.
;; helm이 숨기는 내부 버퍼(helm-boring-buffer-regexp-list)만 동일하게 제외한다.
(setq ibuffer-never-show-predicates
      '("\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf"))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))

;; 버퍼를 디렉터리별로 묶어서 보여준다.
;; ibuffer의 `directory' 필터와 같은 기준(파일의 디렉터리, 없으면
;; default-directory)을 써서 vterm/magit 버퍼도 같은 그룹에 들어간다.
(defun my-ibuffer-buffer-directory (buf)
  "Return BUF's directory: the visited file's directory, else `default-directory'.
The result is expanded so that \"~/\" and \"/Users/me/\" group together."
  (with-current-buffer buf
    (let ((dir (if-let* ((filename (ibuffer-buffer-file-name))
                         (dirname (file-name-directory filename)))
                   dirname
                 default-directory)))
      (and dir (expand-file-name dir)))))

(defun my-ibuffer-filter-groups-by-directory ()
  "Return an `ibuffer-filter-groups' value with one group per directory."
  (let (dirs)
    (dolist (buf (buffer-list))
      (let ((dir (my-ibuffer-buffer-directory buf)))
        (when (and dir (not (member dir dirs)))
          (push dir dirs))))
    (mapcar (lambda (dir)
              (cons (abbreviate-file-name dir)
                    (list (cons 'predicate
                                `(equal (my-ibuffer-buffer-directory
                                         (current-buffer))
                                        ,dir)))))
            (sort dirs #'string<))))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-hook
          (lambda ()
            (setq ibuffer-filter-groups (my-ibuffer-filter-groups-by-directory))
            (ibuffer-update nil t)))


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
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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
        (append '(".DS_Store" ".git" ".svn" "out" "repl" "target" "dist" "lib" "node_modules" "libs" "deploy" "coverage" ".nuxt" "log")
                projectile-globally-ignored-directories))
  (setq grep-find-ignored-directories (append '("dist" "deploy" "node_modules" "coverage" ".nuxt") grep-find-ignored-directories))
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
    (global-set-key (kbd "C-c h s") 'helm-do-grep-ag)
    (global-set-key (kbd "C-c h o") 'helm-occur))
  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source nil
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-apropos-fuzzy-match t)
  (helm-autoresize-mode 1)

  :init(progn
         (helm-mode 1)

         (global-set-key (kbd "M-i") 'helm-occur)

         (use-package helm-projectile
           :ensure t
           :commands (helm-projectile)
           :config
           (helm-projectile-on)
           :bind
           ("C-c h r" . helm-projectile-rg)
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
  ;;; Git만 남기고 나머지 VC 백엔드 비활성화. :vc 패키지 설치에 Git 백엔드 필요.
  (setq vc-handled-backends '(Git))
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
(use-package prettier-js
    :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.babelrc\\'" . js2-mode))
  (setq js2-basic-offset 2)

  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode) ;; better imenu.
  (add-hook 'js2-mode-hook 'lsp)
  (add-hook 'js2-mode-hook 'prettier-js-mode)  ;; if error, run: npm install -g prettier
  )

(use-package js2-refactor
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c j")
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
(use-package indium
  :ensure t
  :config
  (setq indium-chrome-executable "chrome"))


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
  (setq lsp-diagnostics-provider :flycheck)
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

(use-package helm-lsp
  :ensure t
  :after lsp-mode
  )


;;; java
;;; lsp-java
(use-package lsp-java
  :ensure t
  :init
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx2G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:/Users/inho/.m2/repository/org/projectlombok/lombok/1.18.12/lombok-1.18.12.jar"
         )
        )
  (setq lsp-java-completion-import-order ["com" "org" "java" "javax"])
  :config
  (add-hook 'java-mode-hook 'lsp)
  (require 'lsp-java-boot)
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (add-hook 'java-mode-hook (lambda ()
                              (setq c-basic-offset 4)))
  )

(use-package hydra :ensure t)
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))
(use-package dap-java :ensure nil)

;; java compilation buffer color
(use-package xterm-color :ensure t)
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(defun wrap-color-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'wrap-color-compilation-filter)


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


;;; feature-mode
(use-package feature-mode
  :ensure t
  )


;;; claude-code
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest)
  :ensure t)

;; vterm-yank은 먼저 vterm-goto-char로 터미널 커서를 point에 맞추려 하는데,
;; claude 같은 전체화면 TUI에서는 "End of buffer"로 실패해서 붙여넣기가 안 된다.
;; bracketed paste로 문자열만 보내면 커서를 건드리지 않으므로 잘 붙는다.
(defun my-vterm-paste ()
  "Paste the clipboard into the terminal without moving the terminal cursor.
`vterm-yank' calls `vterm-goto-char' first, which signals \"End of
buffer\" inside a full-screen TUI such as Claude Code."
  (interactive)
  (let ((text (ignore-errors (current-kill 0))))
    (if (or (null text) (string= text ""))
        (message "붙여넣을 내용이 없습니다")
      (vterm-send-string text t))))

(use-package vterm
  :ensure t
  :config
  ;; 기본값 1000줄이라 claude 대화가 금방 잘린다.
  ;; 100000은 vterm-module.h의 SB_MAX와 같은 최대값.
  ;; 터미널 생성 시점에 읽으므로 버퍼를 새로 만들어야 적용된다.
  (setq vterm-max-scrollback 100000)
  (define-key vterm-mode-map (kbd "C-y") #'my-vterm-paste)
  (add-hook 'vterm-copy-mode-hook
            (lambda ()
              (if vterm-copy-mode
                  (setq-local cursor-type 'box)
                (setq-local cursor-type nil)))))

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest)
  :ensure t)

;; 전체화면 TUI는 alternate screen(ESC[?1049h)을 써서 vterm에 스크롤백이
;; 전혀 쌓이지 않는다(버퍼가 터미널 높이에 고정됨).
;; alternate screen만 끄면 색상과 UI는 그대로 두고 지난 대화가 남는다.
(defun my-claude-code-disable-alternate-screen (&rest _)
  "Return env vars keeping Claude off the terminal's alternate screen.
Claude's fullscreen TUI otherwise leaves vterm with no scrollback."
  '("CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1"))

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :ensure t
  :config
  (setq claude-code-terminal-backend 'vterm)
  (add-hook 'claude-code-process-environment-functions
            #'my-claude-code-disable-alternate-screen)
  (add-hook 'claude-code-process-environment-functions
            #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))


;;; loading my  configuration
(add-to-list 'load-path "~/.config/emacs/modules/")
(load-file "~/.config/emacs/macros")

(require 'lang-vue)
(require 'lang-go)
(require 'lang-flutter)
(require 'text-file-mode)
(require 'lang-ruby)
(require 'lang-react)

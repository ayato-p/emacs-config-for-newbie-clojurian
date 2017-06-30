;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; language env
;;;

(set-language-environment "UTF-8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; setup package.el
;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(when (not (require 'use-package nil t))
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; basic configurations
;;;

;;; enable use-package
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")
(setq use-package-verbose t)

;;; load your preferred theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;;; set up exec-path
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; global keymap
(use-package bind-key
  :config
  (bind-keys :map global-map
             ("C-h" . delete-backward-char)))

(use-package mykie
  :config
  (setq mykie:use-major-mode-key-override t)
  (mykie:initialize))

;;; happy (((()))) !!!
(use-package paren
  :init
  (setq show-paren-style 'parenthesis)
  (show-paren-mode 1))

;;; yasnippet
(use-package yasnippet
  :bind (:map yas-minor-mode-map
         ("<tab>" . nil)
         ("TAB" . nil)
         ("C-i" . nil)
         ("C-o" . yas/expand))
  :init
  (yas-global-mode 1))

;;; comapany-mode!
(use-package company
  :bind (:map company-mode-map
         ("C-i" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-search-words-regexp)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-global-modes '(prog-mode))
  (global-company-mode))

;;; magit
(use-package magit
  :bind (("C-x g" . magit-status)))

;;; projectile
(use-package projectile
  :init
  (projectile-global-mode))

(use-package subword
  :init
  (global-subword-mode 1))

;;; answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;;; turn off graphical user interface if you want
;; (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
;;   (when (fboundp mode) (funcall mode -1)))

;;; some useful settings
(setq visible-bell t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      echo-keystrokes 0.1
      create-lockfiles nil
      ;; disable to buckup funciton
      backup-inhibited t
      delete-auto-save-files t
      ;; completion ignore case (lower/upper)
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      inhibit-startup-message t)

;; show me empty lines after buffer end
(set-default 'indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)
(setq uniquify-buffer-name-style 'post-forward)

;; whitespace
(use-package whitespace
  :config
  (setq whitespace-style '(face
                           trailing
                           tabs
                           spaces
                           empty
                           space-mark
                           tab-mark))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (global-whitespace-mode 1)
  (setq-default tab-width 4 indent-tabs-mode nil))

;;; cleanup whitespace before file save
(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package hl-line
  :config
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "#525252"))

;;; modeline
(setq display-time-string-forms
      '((format
         "%s/%s(%s) %s:%s" month day dayname 24-hours minutes))
      line-number-mode t
      column-number-mode t)
(display-time-mode 1)

;; http://flex.phys.tohoku.ac.jp/texi/eljman/eljman_142.html
(setq-default
 mode-line-format
 '(""
   mode-line-mule-info
   mode-line-modified
   " "
   mode-line-buffer-identification

   " / "
   (line-number-mode "L%l ")
   (column-number-mode "C%c ")
   (-3 . "%p")
   " / "
   mode-name
   minor-mode-alist "%n" mode-line-process
   " / "
   global-mode-string
   ))

(setq-default
 header-line-format
 '(""
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)))

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; helm
;;;

(use-package helm
  :bind (("M-x" . helm-M-x)
         :map helm-map
         ("C-h" . nil))
  :config
  (setq helm-quick-update t
        helm-buffers-fuzzy-matching t
        helm-ff-transformer-show-only-basename nil))

(use-package helm-projectile
  :config
  (use-package helm-ls-git)
  (mykie:set-keys nil
    "C-x C-f"
    :default (call-interactively 'find-file)
    :C-u helm-projectile-find-file
    :C-u*2! helm-ls-git-ls
    "C-x b"
    :default (call-interactively 'switch-to-buffer)
    :C-u helm-projectile-switch-to-buffer))

(use-package helm-ag
  :bind ("C-x C-g" . helm-do-ag-project-root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; lisp
;;;

(use-package paredit
  :bind (:map paredit-mode-map
         ("C-h" . paredit-backward-delete))
  :config
  (defun conditionally-enable-paredit-mode ()
    (if (eq this-command 'eval-expression)
        (paredit-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode))

(use-package eldoc
  :config
  (setq eldoc-idle-delay 0.1
        eldoc-minor-mode-string ""))

(use-package rainbow-delimiters
  :defer t)

(defun my/lisp-mode-defaults ()
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (eldoc-mode 1))

(defun my/lisp-mode-hook ()
  (my/lisp-mode-defaults))


(use-package lisp-mode
  :ensure nil
  :config
  (add-hook 'emacs-lisp-mode-hook 'my/lisp-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; clojure
;;;

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'my/lisp-mode-hook))

(use-package cider
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'my/lisp-mode-hook)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package clj-refactor
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))

(message "init.el loaded!!")

(add-hook 'after-init-hook
          (lambda ()
            (message "init time: %.3f sec"
                     (float-time (time-subtract after-init-time before-init-time)))))
                     
;;C-h will always mean back-space
(define-key key-translation-map [?\C-h] [?\C-?])

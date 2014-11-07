; no menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)

; no scroll bars
(scroll-bar-mode 0)

; enable narrowing
(put 'narrow-to-region 'disabled nil)

; backups in .saves
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

; shorter yes-no-dialogs
(fset 'yes-or-no-p 'y-or-n-p)

;; Package managers

; marmalade
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
    '("melpa" .
      "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; Set up the package manager of choice. Supports "el-get" and "package.el"
(setq pmoc "el-get")

;; List of all wanted packages
(setq
 wanted-packages
 '(
   ace-jump-mode
   smex
   projectile
   ;autopair
   ido-hacks
   ido-vertical-mode
   switch-window
   company-mode
   ;multiple-cursors
   ;elixir
   ;erlang-mode
   ;highlight-indentation
   expand-region
   ;browse-kill-ring
   powerline
   color-theme-solarized
   go-mode
   go-projectile
))

;; Package manager and packages handler
(defun install-wanted-packages ()
  "Install wanted packages according to a specific package manager"
  (interactive)
  (cond
   ;; package.el
   ((string= pmoc "package.el")
    (require 'package)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    (add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/"))
    (package-initialize)
    (let ((need-refresh nil))
      (mapc (lambda (package-name)
	  (unless (package-installed-p package-name)
	(set 'need-refresh t))) wanted-packages)
      (if need-refresh
	(package-refresh-contents)))
    (mapc (lambda (package-name)
	(unless (package-installed-p package-name)
	  (package-install package-name))) wanted-packages)
    )
   ;; el-get
   ((string= pmoc "el-get")
    (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
    (unless (require 'el-get nil 'noerror)
      (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
    (el-get 'sync wanted-packages))
   ;; fallback
   (t (error "Unsupported package manager")))
  )

;; Install wanted packages
(install-wanted-packages)

;; packages
; ace-jump-mode
(require 'ace-jump-mode)
(eval-when-compile
  (require 'cl))
(global-set-key (kbd "C-x C-j") 'ace-jump-word-mode)
;(global-set-key (kbd "C-M-j") 'ace-jump-word-mode)
;(global-set-key (kbd "C-x j") 'ace-jump-char-mode)
  
; ido-hacks & ido-vertical-mode
(require 'ido-hacks)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode)

(require 'switch-window)
(global-set-key (kbd "C-x C-o") 'switch-window)

(add-hook 'after-init-hook 'global-company-mode)

; go-mode
(require 'go-mode)

; go-projectile
(require 'go-projectile)

; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

; expand-region
(require 'expand-region)
(global-set-key (kbd "ESC <up>") 'er/expand-region)
(global-set-key (kbd "ESC <down>") 'er/contract-region)

; powerline
(require 'powerline)
(setq powerline-arrow-shape 'arrow)   ;; the default
(setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
(setq powerline-arrow-shape 'arrow14)
(powerline-default-theme)

;theme
(load-theme 'solarized-dark t)

; always indent using spaces
(setq-default indent-tabs-mode nil)

; tabs are 4 chars
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

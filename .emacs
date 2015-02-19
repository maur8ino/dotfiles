;; no menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; no scroll bars
(scroll-bar-mode 0)

;; enable narrowing
(put 'narrow-to-region 'disabled nil)

;; backups in .saves
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; shorter yes-no-dialogs
(fset 'yes-or-no-p 'y-or-n-p)

;; Package managers

;; List of all wanted packages
(setq
 wanted-packages
 '(
   exec-path-from-shell
   ace-jump-mode
   smex
   projectile
   magit
   magit-gh-pulls
   ido-hacks
   ido-vertical-mode
   switch-window
   company
   highlight-indentation
   expand-region
   browse-kill-ring
   powerline
   neotree
   go-mode
   go-projectile
   ;; template stuff
   web-mode
   ;; javascript stuff
   ac-js2
   js2-mode
   js2-refactor
   react-snippets
   ;; css, sass & scss
   css-mode
   scss-mode
   sass-mode
   ;; php stuff
   php-mode
   ;; yaml stuff
   yaml-mode
   ;; markdown stuff
   markdown-mode
   gotham-theme
))

;; Package manager and packages handler
(defun install-wanted-packages ()
  "Install wanted packages according to a specific package manager"
  ;; package.el
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

;; Install wanted packages
(install-wanted-packages)

;; packages
;; ace-jump-mode
(require 'ace-jump-mode)
(eval-when-compile
  (require 'cl))
(global-set-key (kbd "C-x C-j") 'ace-jump-word-mode)
(global-set-key (kbd "C-M-j") 'ace-jump-word-mode)
(global-set-key (kbd "C-x j") 'ace-jump-char-mode)
  
;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c l") 'magit-log)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; ido-hacks & ido-vertical-mode
(require 'ido-hacks)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode)

;; switch-window
(require 'switch-window)
(global-set-key (kbd "C-x C-o") 'switch-window)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "ESC <up>") 'er/expand-region)
(global-set-key (kbd "ESC <down>") 'er/contract-region)

; browse-kill-ring
(global-set-key (kbd "C-c C-y") 'browse-kill-ring)

;; powerline
(require 'powerline)
(setq powerline-arrow-shape 'arrow)   ;; the default
(setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
(setq powerline-arrow-shape 'arrow14)
(powerline-default-theme)

;; neotree
(require 'neotree)
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; go-mode
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;; go-projectile
(require 'go-projectile)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; using web-mode with html also
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; js2-mode
(require 'js2-mode)
;; js2-mode for *.js, *.jsx and *.json
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
;; tabs are 2 chars in js2
(custom-set-variables
 '(js2-basic-offset 2))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil
		  tab-width 2)
	    (ac-js2-mode)))
;; set highlight level
(setq js2-highlight-level 2)

;; scss-mode
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'scss-mode-hook
	  (lambda()
	    (setq indent-tabs-mode nil
		  tab-width 2)))

;; php-mode
(require 'php-mode)
(add-hook 'php-mode-hook 'php-enable-default-coding-style)

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  (lambda ()
	    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; theme
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/el-get/gotham-theme"))
(load-theme 'gotham t)

;; Mac: exec-path-from-shell-initialize
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; and set Option key to meta
(setq mac-option-modifier 'meta)
;; and left alt/option as super
(setq mac-option-key-is-meta t)


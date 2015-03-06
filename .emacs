;;; package --- Summary
;;; Commentary:
;;; Code:

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

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Package managers

;; List of all wanted packages
(setq
 wanted-packages
 '(
   ;; mac specific
   exec-path-from-shell
   ;; ignore common files
   ignoramus
   ace-jump-mode
   smex
   projectile
   magit
   magit-gh-pulls
   diff-hl
   ido-hacks
   ido-vertical-mode
   flx-ido
   switch-window
   company
   highlight-indentation
   expand-region
   browse-kill-ring
   powerline
   neotree
   ;; go
   go-mode
   go-projectile
   ;; template stuff
   web-mode
   web-beautify
   ;; javascript stuff
   ac-js2
   js2-mode
   js2-refactor
   react-snippets
   ;; css, sass & scss
   css-mode
   scss-mode
   sass-mode
   ;; coffee-mode (hipster's not dead)
   coffee-mode
   ;; php stuff
   php-mode
   ;; yaml stuff
   yaml-mode
   ;; markdown stuff
   markdown-mode
   ;; themes
   gotham-theme
   ;;color-theme-sanityinc-tomorrow
   ;;zenburn-theme
   obsidian-theme
   flycheck
   ))

;; Package manager and packages handler
(defun install-wanted-packages ()
  "Install wanted packages according to a specific package manager."
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

;; Devel text modes mode
(defun devel-modes-hook ()
  "Activate minor modes for developing."
  ;; diff-hl
  (diff-hl-mode t)
  ;; line mode
  (hl-line-mode t)
  ;; line numbers
  (linum-mode t))

;; Install wanted packages
(install-wanted-packages)

;; PACKAGES

;; ignoramus
(require 'ignoramus)
(ignoramus-setup)

;; projectile
(projectile-global-mode)

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
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c l") 'magit-log)
(global-set-key (kbd "C-c o") 'magit-checkout)
(add-hook 'magit-mode-hook
          (lambda ()
            (turn-on-magit-gh-pulls)
            (setq magit-gh-pulls-collapse-commits t)))


;; ido-hacks & ido-vertical-mode
(require 'ido-hacks)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode t)
(flx-ido-mode t)

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
(require 'browse-kill-ring)
(global-set-key (kbd "C-c C-y") 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; powerline
(require 'powerline)
(setq powerline-arrow-shape 'arrow)   ;; the default
(setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
(setq powerline-arrow-shape 'arrow14)
(powerline-default-theme)

;; neotree
(require 'neotree)
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; lisp-mode
(add-hook 'lisp-mode 'devel-modes-hook)

;; go-mode
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

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
;; using web-mode with handlebars also
(add-to-list 'auto-mode-alist '("\\.handlebars$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-enable-current-element-highlight t
		  web-mode-enable-current-column-highlight t)
            (devel-modes-hook)))

;; js2-mode
(require 'js2-mode)
;; js2-mode for *.js, *.jsx and *.json
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
;; tabs are 2 chars in js2
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil
                          tab-width 2)
            (ac-js2-mode)
            (devel-modes-hook)))

(setq-default js2-basic-offset 2)
;; set highlight level
(setq-default js2-highlight-level 2)
;; mirror mode off
(setq-default js2-mirror-mode nil)
;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t)

;; (s)css-mode
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'css-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  css-basic-offset 2)
            (devel-modes-hook)))

(add-hook 'scss-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 2))
            (devel-modes-hook))

;; php-mode
(require 'php-mode)
(add-hook 'php-mode-hook
          (lambda ()
            (php-enable-default-coding-style)
            (devel-modes-hook)))


;; ruby-mode
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(add-hook 'ruby-mode-hook
          (lambda()
            (local-set-key "\r" 'newline-and-indent)
            (flymake-ruby-load)
            (devel-modes-hook)))

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  (lambda ()
	    (define-key yaml-mode-map "\C-m" 'newline-and-indent)
      (devel-modes-hook)))

;; markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'devel-modes-hook)

;; theme
;;(load-theme 'gotham t)
;(load-theme 'sanityinc-tomorrow-night t)
;(load-theme 'zenburn t)
(load-theme 'obsidian t)

;; Mac: exec-path-from-shell-initialize
;; Setup environment variables from the user's shell.
(when is-mac
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  ;; change command to meta and ignore option
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)

  ;; mac friendly font
  (when window-system
    (setq maur8ino/default-font "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
    (setq maur8ino/presentation-font "-apple-Menlo-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
    (set-face-attribute 'default nil :font maur8ino/default-font))

  ;; Move to trash when deleting stuff
  (setq delete-by-moving-to-trash t
	trash-directory "~/.Trash/emacs")

  ;; Ignore .DS_Store files with ido mode
  (add-to-list 'ido-ignore-files "\\.DS_Store"))

(provide '.emacs)
;;; .emacs ends here

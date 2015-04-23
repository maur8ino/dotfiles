;;; package --- Summary
;;; Commentary:
;;; Code:

;; no menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; no scroll bars
(scroll-bar-mode 0)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; enable narrowing
(put 'narrow-to-region 'disabled nil)
;; Newline at end of file
(setq require-final-newline t)
;; delete the selection with a keypress
(delete-selection-mode t)
;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;; don't use tabs to indent
(setq-default indent-tabs-mode nil)
;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

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
   ;; editor stuff
   whitespace
   whitespace-cleanup-mode
   smartparens
   diff-hl
   anzu
   ;; project and completion stuff
   projectile
   helm
   helm-projectile
   helm-company
   ignoramus
   ag
   ;; magit and diff
   magit
   ;;magit-gh-pulls
   git-timemachine
   ;; graphical stuff
   ace-jump-mode
   switch-window
   company
   highlight-indentation
   expand-region
   neotree
   smart-mode-line
   ;; syntax checking on-fly
   flycheck
   ;; yasnippet
   yasnippet
   ;; go
   go-mode
   go-projectile
   ;;go-autocomplete
   ;; web mode
   web-mode
   web-beautify
   ;; javascript stuff
   js2-mode
   js2-refactor
   ;;ac-js2
   react-snippets
   ;; css, sass & scss
   css-mode
   scss-mode
   sass-mode
   ;; coffee-mode (hipster's not dead)
   coffee-mode
   ;; php stuff
   php-mode
   ;; toml stuff
   toml-mode
   ;; yaml stuff
   yaml-mode
   ;; ruby mode
   robe
   rubocop
   ;; markdown stuff
   markdown-mode
   ;; elixir stuff
   alchemist
   elixir-mode
   elixir-yasnippets
   ;; dockerfile-mode
   dockerfile-mode
   ;; themes
   gotham-theme
   obsidian-theme
   solarized-theme
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
  (linum-mode t)
  ;; whitespace
  (whitespace-mode +1)
  (whitespace-cleanup-mode t))

;; Presentation modes
(defun presentation-modes-hook ()
  "Activate minor modes for presenting."
  ;; diff-hl
  (diff-hl-mode nil)
  ;; line mode
  (hl-line-mode t)
  ;; line numbers
  (linum-mode nil)

  (when is-mac
    ;; mac presentation friendly font
    (when window-system
      (set-face-attribute 'default nil :font maur8ino/presentation-font))))

;; Install wanted packages
(install-wanted-packages)

;; dired
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)
;; was dired-advertised-find-file
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; was dired-up-directory
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once

;; PACKAGES

;; smartparens
;; smart pairing for all
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
(require 'anzu)
(global-anzu-mode)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; ignoramus
(ignoramus-setup)

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
;; ignore common temporary directories
(setq projectile-globally-ignored-directories
      (append projectile-globally-ignored-directories
              '("node_modules" "bower_components" ".bower-cache" "public/assets" "tmp")))

(helm-projectile-on)

;; yasnippet
;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
;;(ac-set-trigger-key "TAB")
;;(ac-set-trigger-key "<tab>")

;; helm
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r") 'helm-recentf)
;; definitions
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;; fuzzy matching everywhere
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-M-x-fuzzy-match t)
(helm-autoresize-mode t)

;; git-timemachine
(global-set-key (kbd "C-x t") 'git-timemachine-toggle)

;; ace-jump-mode
(require 'ace-jump-mode)
(eval-when-compile
  (require 'cl))
(global-set-key (kbd "C-x C-j") 'ace-jump-word-mode)
(global-set-key (kbd "C-M-j") 'ace-jump-word-mode)
(global-set-key (kbd "C-x j") 'ace-jump-char-mode)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c l") 'magit-log)
(global-set-key (kbd "C-c o") 'magit-checkout)
;;(add-hook 'magit-mode-hook
;;          (lambda ()
;;            (turn-on-magit-gh-pulls)
;;            (setq magit-gh-pulls-collapse-commits t)))
(setq magit-last-seen-setup-instructions "1.4.0")

;; switch-window
(require 'switch-window)
(global-set-key (kbd "C-x C-o") 'switch-window)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "ESC <up>") 'er/expand-region)
(global-set-key (kbd "ESC <down>") 'er/contract-region)

;; neotree
(require 'neotree)
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; lisp-mode
(add-hook 'lisp-mode 'devel-modes-hook)

;; go-mode
(require 'go-mode)
;;(require 'go-autocomplete)
(add-hook 'before-save-hook 'gofmt-before-save)

;; web-mode
(setq web-mode-enable-current-element-highlight t
      web-mode-enable-current-column-highlight t)

(require 'web-mode)
(require 'react-snippets)
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
;; web-mode for *.js, *.jsx and *.json
(add-to-list 'auto-mode-alist '("\\.jsx?" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)
            (devel-modes-hook)))

;;(setq flycheck-disabled-checkers '(javascript-jshint))
;;(setq flycheck-checkers '(javascript-eslint))

;; (s)css-mode
(add-hook 'css-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  css-indent-offset 2)
            (devel-modes-hook)))

(add-hook 'scss-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  css-indent-offset 2))
          (devel-modes-hook))

;; php-mode
(require 'php-mode)
(add-hook 'php-mode-hook
          (lambda ()
            (php-enable-default-coding-style)
            (devel-modes-hook)))


;; ruby-mode
(require 'rubocop)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(add-hook 'ruby-mode-hook
          (lambda()
            (local-set-key "\r" 'newline-and-indent)
            (flymake-ruby-load)
            (devel-modes-hook)
            (robe-mode)
            (rubocop-mode)
            (devel-modes-hook)))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(add-hook 'haml-mode-hook 'devel-modes-hook)

;; toml mode
(require 'toml-mode)
(add-to-list 'auto-mode-alist '("\\.toml$\\'" . toml-mode))
(add-hook 'toml-mode-hook 'devel-modes-hook)

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

;; elixir-mode
(require 'elixir-mode)
(add-hook 'elixir-mode-hook 'devel-modes-hook)

;; dockerfile-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

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

;; theme
;;(load-theme 'gotham t)
;;(load-theme 'obsidian t)
;; make the modeline high contrast
(load-theme 'solarized-dark t)

;; smart-line-mode
(setq sml/mode-width 'full)
(sml/setup)
(sml/apply-theme 'automatic)

(provide '.emacs)
;;; .emacs ends here

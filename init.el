(eval-when-compile
  (setq-default package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			   ("melpa" . "http://melpa.org/packages/")
			   ("elpa" . "http://elpa.gnu.org/packages/")))
  (setq-default package--init-file-ensured t)
  (setq-default package-enable-at-startup nil)

  (require 'package)
  (package-initialize)
  (package-refresh-contents)

  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))

(setq server-socket-dir "/tmp/emacs-server/")
(server-start)

(setq gc-cons-threshold (* 64 1024 1024))

(setq inhibit-startup-message t)

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(set-language-environment "Korean")

(add-hook 'after-make-frame-functions
	  #'(lambda (frame)
	      (modify-frame-parameters frame
				       '((vertical-scroll-bars . nil)
					 (horizontal-scroll-bars . nil)))
	      ))

(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

(use-package use-package
  :commands use-package-autoload-keymap)

(use-package exec-path-from-shell :ensure t
  :config (exec-path-from-shell-initialize))

(setq-default yas-triggers-in-field 1)
(use-package yasnippet-snippets :ensure t)
(use-package yasnippet :ensure t
  :config (progn
            (yas-global-mode 1)
            (define-key yas-minor-mode-map [(tab)] nil)
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            (define-key yas-minor-mode-map (kbd "C-.") yas-maybe-expand)
            (define-key yas-keymap [(tab)]       nil)
            (define-key yas-keymap (kbd "TAB")   nil)
            (define-key yas-keymap [(shift tab)] nil)
            (define-key yas-keymap [backtab]     nil)
            (define-key yas-keymap (kbd "C-.") 'yas-next-field-or-maybe-expand)
            (define-key yas-keymap (kbd "C-,") 'yas-prev)))

(use-package helm :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)))

(use-package helm-gtags :ensure t
  :requires helm
  :init
    (custom-set-variables
     '(helm-gtags-suggested-key-mapping t)
     '(helm-gtags-path-style 'relative)
     '(helm-gtags-auto-update t))
  :config
    (progn
      (add-hook 'c-mode-hook 'helm-gtags-mode)
      (add-hook 'c++-mode-hook 'helm-gtags-mode)
      (add-hook 'asm-mode-hook 'helm-gtags-mode)))

(use-package undo-tree :ensure t)
(use-package goto-chg :ensure t)
(use-package general :ensure t
  :config
  (progn
    (general-define-key
     :states 'normal
     "\M-x" 'helm-M-x
     "j" 'evil-next-visual-line
     "k" 'evil-previous-visual-line
     "<tab>" 'evil-mc-undo-all-cursors)
    (general-define-key
     :states 'visual
     "<tab>" "<escape>")
    (general-define-key
     :states 'insert
     "<tab>" "<escape>")
    (general-override-mode)
    (general-define-key
      :states 'normal
      :keymaps 'override
      :prefix "SPC"
      "yv" 'yas-visit-snippet-file
      "ei" 'open-init-script
      "f" 'helm-find-files
      "bb" 'helm-buffers-list
      "b SPC" "\C-xb"
      "bp" 'previous-buffer
      "bn" 'next-buffer
      "s" 'save-buffer
      "k" 'kill-buffer
      "v" 'find-alternate-file
      "o" "\C-xo"
      "-" "\C-x-"
      "+" "\C-x+"
      "^" "\C-x^"
      "{" "\C-x{"
      "}" "\C-x}"
      "0" "\C-x0"
      "1" "\C-x1"
      "2" "\C-x2"
      "3" "\C-x3"
      "4b" "\C-x4b"
      "5b" "\C-x5b"
      "50" "\C-x50"
      "51" "\C-x51"
      "52" "\C-x52"
      "53" "\C-x53")))

(setq-default init-script "~/.emacs.d/init.el")
(if load-file-name
    (setq init-script load-file-name))

(defun open-init-script ()
  (interactive)
  (find-file-existing init-script))

(use-package evil :ensure t
  :requires undo-tree goto-chg
  :init
    (custom-set-variables
      '(evil-emacs-state-modes nil)
      '(evil-insert-state-modes nil)
      '(evil-motion-state-modes nil))
  :config
    (evil-mode 1))

(use-package evil-escape :ensure t
  :requires evil
  :init (global-set-key (kbd "<tab>") 'evil-escape)
  :config (evil-escape-mode 1))

(use-package evil-mc :ensure t
  :requires evil
  :config (global-evil-mc-mode 1))

(use-package evil-surround :ensure t
  :requires evil
  :config (global-evil-surround-mode 1))

(use-package rust-mode :ensure t
  :mode "\\.rs\\'")
(use-package typescript-mode :ensure t
  :mode "\\.ts\\'")
(use-package haskell-mode :ensure t
  :mode "\\.hs\\'")
(use-package magit :ensure t)
(use-package markdown-mode :ensure t
  :mode "\\.md\\'")
(use-package bison-mode :ensure t
  :mode "\\.l\\'" "\\.y\\'")
(use-package go-mode :ensure t
  :mode "\\.go\\'")
(use-package ahk-mode :ensure t
  :mode "\\.ahk\\'")
(use-package masm-mode :ensure t
  :mode "\\.asm\\'")
(use-package arduino-mode :ensure t
  :mode "\\.ino\\'")
(use-package lua-mode :ensure t
  :mode "\\.lua\\'")

(use-package multiple-cursors :ensure t :disabled
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-?" . mc/mark-all-like-this))
  :config (define-key mc/keymap (kbd "<return>") nil))

(use-package emojify :ensure t
  :hook (after-init . global-emojify-mode))

(use-package dracula-theme :ensure t
  :config
    (progn
      (load-theme 'dracula t t)
      (load-theme 'leuven)
      (enable-theme 'leuven)))

(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)

(desktop-save-mode 1)
(winner-mode 1)
(electric-pair-mode 1)
(global-linum-mode 1)
(column-number-mode 1)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq-default show-trailing-whitespace t)

(setq-default indent-tabs-mode nil)

(setq local-config-file (expand-file-name "local-conf.el" user-emacs-directory))

;(global-set-key (kbd "<up>") 'toggle-input-method)

(global-set-key (kbd "C-x y") 'yank)

(global-set-key (kbd "C-x t") 'open-term)

(global-set-key (kbd "C-c C-,") 'winner-undo)
(global-set-key (kbd "C-c C-.") 'winner-redo)

(setq shell-name "/bin/bash")
(setq shell-args '("--login" "-i"))

(defun open-term ()
  (interactive)
  (split-window-below)
  (enlarge-window 7)
  (other-window 1)
  (switch-to-buffer (apply #'term-ansi-make-term `("terminal" ,shell-name nil ,@shell-args)))
  (term-char-mode))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun c-hook ()
  (add-hook 'window-size-change-functions nil 'make-it-local)
  ;(add-hook 'window-configuration-change-hook nil 'make-it-local)
      ; occurs "Symbol's function definition is void: nil"

  (setq c-basic-offset 8
          c-label-minimum-indentation 0
          c-offsets-alist '(
                          (arglist-close         . c-lineup-arglist-tabs-only)
                          (arglist-cont-nonempty .
                                                  (c-lineup-gcc-asm-reg c-lineup-arglist-tabs-only))
                          (arglist-intro         . +)
                          (brace-list-intro      . +)
                          (c                     . c-lineup-C-comments)
                          (case-label            . 0)
                          (comment-intro         . c-lineup-comment)
                          (cpp-define-intro      . +)
                          (cpp-macro             . -1000)
                          (cpp-macro-cont        . +)
                          (defun-block-intro     . +)
                          (else-clause           . 0)
                          (func-decl-cont        . +)
                          (inclass               . +)
                          (inher-cont            . c-lineup-multi-inher)
                          (knr-argdecl-intro     . 0)
                          (label                 . -1000)
                          (statement             . 0)
                          (statement-block-intro . +)
                          (statement-case-intro  . +)
                          (statement-cont        . +)
                          (substatement          . +))
          indent-tabs-mode t
          show-trailing-whitespace t)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'c-mode-hook 'c-hook)
(add-hook 'c++-mode-hook 'c-hook)

(unless (file-exists-p local-config-file)
  (write-region ";; local config file\n" nil local-config-file))
(load-file local-config-file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b46ee2c193e350d07529fcd50948ca54ad3b38446dcbd9b28d0378792db5c088" default)))
 '(evil-emacs-state-modes nil)
 '(evil-insert-state-modes nil)
 '(evil-motion-state-modes nil)
 '(helm-gtags-auto-update t)
 '(helm-gtags-path-style (quote relative))
 '(helm-gtags-suggested-key-mapping t)
 '(package-selected-packages
   (quote
    (yasnippet-snippets go-mode ahk-mode use-package typescript-mode rust-mode org-projectile multiple-cursors markdown-mode magit helm-gtags haskell-mode general exec-path-from-shell evil-surround evil-mc evil-escape dracula-theme bison-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

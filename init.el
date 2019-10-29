(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("elpa" . "http://elpa.gnu.org/packages/")))
(setq package--init-file-ensured t)
(setq package-enable-at-startup nil)

(eval-when-compile
  (require 'package)
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

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

(use-package use-package
  :commands use-package-autoload-keymap)

(use-package helm :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)))

(use-package haskell-mode :ensure t
  :mode "\\.hs\\'")
(use-package magit :ensure t)
(use-package markdown-mode :ensure t
  :mode "\\.md\\'")

(use-package bison-mode :ensure t
  :mode ("\\.l\\'" "\\.y\\'"))

(use-package multiple-cursors :ensure t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-?" . mc/mark-all-like-this)))

(use-package dracula-theme :ensure t
  :config
    (progn
      (load-theme 'dracula t t)
      (enable-theme 'dracula)))

(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)

(setq modes-to-enable '(desktop-save-mode winner-mode electric-pair-mode global-linum-mode))
(setq modes-to-disable '(menu-bar-mode toggle-scroll-bar tool-bar-mode))

(mapc #'(lambda (modes)
	  (mapc #'(lambda (mode) (funcall mode (car modes)))
		(cdr modes)))
      `(( 1 ,@modes-to-enable)
	(-1 ,@modes-to-disable)))

(setq local-config-file (expand-file-name "local-conf.el" user-emacs-directory))

(global-set-key (kbd "<up>") 'toggle-input-method)

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

(defun set-window-margin-80-columns ()
  (set-window-margins nil 0 (max (- (window-width) 80) 0)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (set-window-margin-80-columns)
	    (add-hook 'window-size-change-functions 'set-window-margin-80-columns nil 'make-it-local)
	    (add-hook 'window-configuration-change-hook 'set-window-margin-80-columns nil 'make-it-local)

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
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(unless (file-exists-p local-config-file)
  (write-region ";; local config file\n" nil local-config-file))
(load-file local-config-file)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (popup-el emacs-async use-package org-projectile multiple-cursors markdown-mode magit helm haskell-mode dracula-theme bison-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

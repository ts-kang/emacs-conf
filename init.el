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
					 (horizontal-scroll-bars . nil)))))


(use-package use-package
  :commands use-package-autoload-keymap)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors bison-mode use-package org-projectile markdown-mode magit haskell-mode dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

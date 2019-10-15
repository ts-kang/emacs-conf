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

(unless (file-exists-p local-config-file)
  (write-region ";; local config file\n" nil local-config-file))
(load-file local-config-file)

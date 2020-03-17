(eval-when-compile
  (setq-default package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			   ("melpa" . "http://melpa.milkbox.net/packages/")
			   ("elpa" . "http://elpa.gnu.org/packages/")))
  (setq-default package--init-file-ensured t)
  (setq-default package-enable-at-startup nil)

  (require 'package)
  (package-initialize)
  (package-refresh-contents)

  (unless (package-installed-p 'use-package)
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

(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

(use-package use-package
  :commands use-package-autoload-keymap)

(use-package exec-path-from-shell :ensure t
  :config (exec-path-from-shell-initialize))

(use-package undo-tree :ensure t)
(use-package goto-chg :ensure t)
(use-package general :ensure t)

(use-package evil :ensure t
  :requires (undo-tree goto-chg)
  :init
    (custom-set-variables
      '(evil-emacs-state-modes nil)
      '(evil-insert-state-modes nil)
      '(evil-motion-state-modes nil))
  :config
    (progn
      (define-key evil-normal-state-map (kbd "M-x") 'helm-M-x)
      (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
      (define-key evil-normal-state-map [escape] 'evil-mc-undo-all-cursors)
      (define-key evil-normal-state-map (kbd "SPC f") 'helm-find-files)
      (define-key evil-normal-state-map (kbd "SPC b") 'helm-buffers-list)
      (define-key evil-normal-state-map (kbd "SPC s") 'save-buffer)
      (define-key evil-normal-state-map (kbd "SPC o") "\C-xo")
      (define-key evil-normal-state-map (kbd "SPC 0") "\C-x0")
      (define-key evil-normal-state-map (kbd "SPC 1") "\C-x1")
      (define-key evil-normal-state-map (kbd "SPC 2") "\C-x2")
      (define-key evil-normal-state-map (kbd "SPC 3") "\C-x3")
      (define-key evil-normal-state-map (kbd "SPC 5") "\C-x5")
      (evil-mode 1)))

(use-package evil-escape :ensure t
  :requires evil
  :init (setq-default evil-escape-key-sequence "kj")
  :config (evil-escape-mode 1))

(use-package evil-mc :ensure t
  :requires evil
  :config (global-evil-mc-mode 1))

(use-package evil-surround :ensure t
  :requires evil
  :config (global-evil-surround-mode 1))

(use-package helm :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)))

(use-package helm-gtags :ensure t
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

(use-package typescript-mode  :ensure t
  :mode "\\.ts\\'")
(use-package haskell-mode :ensure t
  :mode "\\.hs\\'")
(use-package magit :ensure t)
(use-package markdown-mode :ensure t
  :mode "\\.md\\'")

(use-package bison-mode :ensure t
  :mode ("\\.l\\'" "\\.y\\'"))

(use-package multiple-cursors :ensure t :disabled
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-?" . mc/mark-all-like-this))
  :config (define-key mc/keymap (kbd "<return>") nil))

(use-package dracula-theme :ensure t)
;  :config
;    (progn
;      (load-theme 'dracula t t)
;      (enable-theme 'dracula)))

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



(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
 
(when window-system (set-frame-size (selected-frame) 160 40))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages '(use-package clojure-mode lsp-mode cider lsp-treemacs flycheck company parinfer-rust-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      lsp-keymap-prefix "C-l")
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
	which-key-separator ":"
	which-key-max-description-length 120
    which-key-side-window-max-width 50
    which-key-idle-delay 1.00))

(use-package magit
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package dired-sidebar
  :ensure t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar))

(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :init
  (progn
    (setq parinfer-rust-auto-download t)
    (add-hook 'clojure-mode-hook #'parinfer-rust-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-rust-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-rust-mode)
    (add-hook 'scheme-mode-hook #'parinfer-rust-mode)
    (add-hook 'lisp-mode-hook #'parinfer-rust-mode)))

(use-package diff-hl
  :ensure t
  :init (add-hook 'prog-mode-hook #'diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

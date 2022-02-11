;;; init.el --- Emacs config

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/init/")
(require 'init-local)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(when window-system (set-frame-size (selected-frame) 160 40))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      tab-always-indent 'complete)

;; Fine tune backups
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves/"))    ; don't litter my fs tree
      auto-save-file-name-transforms
      `((".*" "~/.saves/" t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

;; Draw tabs with the same color as trailing whitespace
(add-hook 'font-lock-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\t" 0 'trailing-whitespace prepend)))))

;; Cleanup trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode)))

(use-package clojure-mode
  :ensure t
  :config
  (progn
    (setq clojure-indent-style 'align-arguments
          clojure-align-forms-automatically t)))

(use-package rg
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(require 'org-clock)
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

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
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-vcs-max-length 17))

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
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package dired-sidebar
  :ensure t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar))

(use-package parinfer-rust-mode
  :ensure t
  :hook emacs-lisp-mode
  :init
  (progn
    (setq parinfer-rust-auto-download t)
    (add-hook 'clojure-mode-hook #'parinfer-rust-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-rust-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-rust-mode)
    (add-hook 'scheme-mode-hook #'parinfer-rust-mode)
    (add-hook 'lisp-mode-hook #'parinfer-rust-mode)))

(use-package ivy
  :ensure t
  :diminish
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers 1
          ivy-count-format "(%d/%d) ")))

(use-package diff-hl
  :ensure t
  :init (add-hook 'prog-mode-hook #'diff-hl-margin-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  :config
  (global-diff-hl-mode t))

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-space-between-root-nodes nil)
    (treemacs-follow-mode -1)
    (defun treemacs-ignore (filename absolute-path)
      (or (string-equal filename "foo-bar-baz")
          (cl-search "/.shadow-cljs" absolute-path)
          (cl-search "/.idea" absolute-path)
          (cl-search "/node_modules" absolute-path)))
    (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore)))

(use-package treemacs-all-the-icons
  :after (treemacs)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package projectile
  :ensure t
  :custom ((projectile-completion-system 'ivy))
  :init
  (progn
    (projectile-mode +1)
    (setq projectile-project-search-path '("~/work/"))
    (setq projectile-switch-project-action #'projectile-dired))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (progn
    (setq lsp-keymap-prefix "C-c l"
          lsp-idle-delay 1.0
          ;lsp-signature-auto-activate nil
          lsp-lens-enable t
          lsp-completion-enable nil ; Prefer cider completion
          lsp-enable-indentation nil) ; Prefer clojure-mode indentation
    ;; For logging IO between client and server
    (setq lsp-log-io nil))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (("<C-return>" . lsp-find-definition)
         ("<C-.>" . lsp-find-definition)
         ("<C-M-return>" . lsp-describe-thing-at-point)
         ("<f1>" . lsp-treemacs-symbols)
         ("<f2>" . lsp-treemacs-call-hierarchy))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :init (progn
          (setq lsp-ui-sideline-show-code-actions nil
                lsp-ui-doc-show-with-cursor t))
  :bind (("M-." . lsp-ui-peek-find-references)))

(use-package lsp-treemacs
  :ensure t
  :config
  (setq lsp-treemacs-symbols-sort-functions '(lsp-treemacs-sort-by-name)))

;; Locally build clojure-lsp
;;(setq lsp-clojure-custom-server-command '("bash" "-c" "/home/lassemaatta/work/clojure-lsp/clojure-lsp"))

(use-package cider
  :ensure t
  :defer t
  :config
  (progn
    (setq nrepl-log-messages t
          cider-repl-buffer-size-limit 1000
          cider-repl-display-in-current-window t
          cider-repl-use-clojure-font-lock t
          cider-prompt-save-file-on-load 'always-save
          cider-font-lock-dynamically '(macro core function var)
          cider-eldoc-display-for-symbol-at-point nil
          cider-offer-to-open-cljs-app-in-browser nil
          nrepl-hide-special-buffers t
          cider-overlays-use-font-lock t
          cider-enrich-classpath nil)

    (cider-repl-toggle-pretty-printing)

    (put-clojure-indent 'testit/fact 1)
    (put-clojure-indent 'testit.core/fact 1)
    (put-clojure-indent 'page/html5 1)
    (put-clojure-indent 'fact 1)
    (put-clojure-indent 'rf/reg-event-fx 1)
    (put-clojure-indent 'chain/reg-chain 1)
    (put-clojure-indent 'rf/reg-sub 1)
    (put-clojure-indent 'rf/reg-event-db 1)
    (put-clojure-indent 'futil/for-all 1)
    (put-clojure-indent 'futil/for-frag 1)
    (put-clojure-indent 'for-frag 1)
    (put-clojure-indent 'for-all 1)
    (put-clojure-indent 'u/for-all 1)
    (put-clojure-indent 'not-join 1)
    (put-clojure-indent 'r/with-let 1)
    (put-clojure-indent 'p/if-all-let 1)
    (put-clojure-indent 'test-seq/seq-tx 1)))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  ;:defer t
  ;:hook (cider-repl-mode cider-mode)
  :config
  (setq company-minimum-prefix-length 3))

(use-package lsp-java
  :ensure t
  :defer t
  :hook ((java-mode . lsp)))

(use-package groovy-mode
  :ensure t
  :defer t)

(defun jet-pretty ()
  "Run transit->edn conversion on the active buffer."
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "/home/lassemaatta/bin/jet --pretty --from transit --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 2))

(use-package company-graphviz-dot)

(provide 'init)

;;; init.el ends here

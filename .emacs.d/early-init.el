;;; early-init.el --- Minimal setup before loading the GUI -*- lexical-binding: t; -*-


;;; Commentary:
;; Do some minimal setup at startup

;;; Code:

(defconst emacs-start-time (current-time))

;; Set a high GC limit during startup
(setq gc-cons-threshold (* 256 1024 1024)
      garbage-collection-messages nil)

;; Don't warn if a file is missing the lexical binding cookie
(setq warning-suppress-log-types '((files missing-lexbind-cookie)))

;; When choosing between `.el[cn]', use latest
(require 'jka-compr) ; workaround for recursive load
(setq load-prefer-newer nil)

;; Disable menubar, toolbar, but not scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 1)

;; Don't resize the window when we alter the theme/fonts during startup.
(setq frame-inhibit-implied-resize t)

;; Don't try to round the frame size based on the font size.
(setq frame-resize-pixelwise t)

(setq default-frame-alist
      '(;; A semi-large frame
        (width . 170)
        (height . 60)
        ;; Set a black background so we won't flash white during startup
        (background-color . "#000000")
        ;; Make the title bar transparent (on macos)
        (ns-appearance . dark)
        (ns-transparent-titlebar . t)))

(set-face-attribute 'default nil :background "#000000" :foreground "#bbc2cf")

;; Don't report when native compilation produces warnings or errors
(setq native-comp-async-report-warnings-errors nil)

(setq package-enable-at-startup nil)

;; Use plists instead of hash-table in lsp-mode
(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)

;;; early-init.el ends here

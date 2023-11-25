;;; early-init.el --- Minimal setup before loading the GUI -*- lexical-binding: t; -*-


;;; Commentary:
;; Do some minimal setup at startup

;;; Code:

(defconst emacs-start-time (current-time))

;; Set a high GC limit during startup
(setq gc-cons-threshold (* 256 1024 1024)
      garbage-collection-messages t)

;; Disable menubar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Don't resize the window when we alter the theme/fonts during startup.
(setq frame-inhibit-implied-resize t)

;; Don't try to round the frame size based on the font size.
(setq frame-resize-pixelwise t)

;; Make the initial emacs frame go fullscreen at startup.
(add-to-list 'default-frame-alist '(width . 170))
(add-to-list 'default-frame-alist '(height . 60))

;; Set a black background so we won't flash white during startup
(set-face-attribute 'default nil :background "#000000" :foreground "#bbc2cf")

;; Don't report when native compilation produces warnings or errors
(setq native-comp-async-report-warnings-errors nil)

(setq package-enable-at-startup nil)

(provide 'early-init)

;;; early-init.el ends here

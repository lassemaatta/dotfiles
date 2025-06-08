;;; init.el --- Emacs config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar emacs-start-time (current-time))

(defvar my-require-tree nil)

(defun my-require--advice (orig-fun feature &rest args)
  (setq my-require-tree
        (append my-require-tree
                (list (let ((my-require-tree (list feature)))
                        (apply orig-fun feature args)
                        my-require-tree)))))

(advice-add 'require :around 'my-require--advice)

;; Define some utility functions

(defun get-string-from-file (filePath)
  "Read the contents of a FILEPATH into a string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun get-file-hash (filePath)
  "Calculate the MD5 hash of a FILEPATH."
  (md5 (get-string-from-file filePath)))

(load "~/.emacs.d/elpaca-bootstrap.el")

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loaded elpaca-bootstrap in %.3fs" elapsed))

;; Tangle the settings, but only if there is a hash mismatch

(defconst org-src (expand-file-name "settings.org" user-emacs-directory))
(defconst org-dst (expand-file-name "settings.el" user-emacs-directory))
(defconst org-hash (expand-file-name "settings.org.hash" user-emacs-directory))
(defconst load-src (expand-file-name "settings" user-emacs-directory))

(defconst stored-hash (if (file-exists-p org-hash)
                          (get-string-from-file org-hash)
                        "<no hash>"))
(defconst current-hash (get-file-hash org-src))

(defun my-tangle (src dst)
  "Tangle org mode SRC to DST in another Emacs process."
  (let ((command (format "emacs --quick --batch --eval \"(progn (require 'org) (org-babel-tangle-file \\\"%s\\\" \\\"%s\\\"))\""
                         src
                         dst)))
    (shell-command command)))

(if (not (string= stored-hash current-hash))
    (progn
      (message "Hashes do not match (%s vs %s) => Tangling settings.."
               stored-hash current-hash)
      (let ((org-start (float-time (current-time))))
        (my-tangle org-src org-dst)
        (let ((org-end (float-time (current-time))))
          (message "Tangled.. done (%.3fs)" (float-time (time-subtract (current-time)
                                                                       org-start)))))
      ;; Store the new hash
      (with-temp-file org-hash
        (insert current-hash)))
  (message "Hashes match, not tangling settings."))

;; Load the tangled settings
(let ((load-start (float-time (current-time))))
  (load load-src)
  (let ((org-end (float-time (current-time))))
    (message "Load.. done (%.3fs)" (float-time (time-subtract (current-time)
                                                              load-start)))))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Init.el finished...done (%.3fs)" elapsed))

(provide 'init)

;;; init.el ends here

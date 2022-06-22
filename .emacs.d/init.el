;;; init.el --- Emacs config

;;; Commentary:

;;; Code:

(defconst emacs-start-time (current-time))

(setq gc-cons-threshold most-positive-fixnum
      garbage-collection-messages t)

;; Define some utility functions

(defun get-string-from-file (filePath)
  "Read the contents of a FILEPATH into a string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun get-file-hash (filePath)
  "Calculate the MD5 hash of a FILEPATH."
  (md5 (get-string-from-file filePath)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(unless package--initialized
  (package-initialize))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

;; Tangle the settings, but only if there is a hash mismatch

(defconst org-src (expand-file-name "settings.org" user-emacs-directory))
(defconst org-dst (expand-file-name "settings.el" user-emacs-directory))
(defconst org-hash (expand-file-name "settings.org.hash" user-emacs-directory))

(defconst stored-hash (if (file-exists-p org-hash)
                          (get-string-from-file org-hash)
                        "<no hash>"))
(defconst current-hash (get-file-hash org-src))

;; Forward declare function
(declare-function org-babel-tangle-file "ob-tangle" (file &optional target-file lang-re))

(if (not (string= stored-hash current-hash))
    (progn
      (message "Hashes do not match (%s vs %s) => Tangling settings.."
               stored-hash current-hash)
      (let ((org-start (float-time (current-time))))
        (require 'org)
        (org-babel-tangle-file org-src org-dst)
        (let ((org-end (float-time (current-time))))
          (message "Tangled.. done (%.3fs)" (float-time (time-subtract (current-time)
                                                                       org-start)))))
      ;; Store the new hash
      (with-temp-file org-hash
        (insert current-hash)))
  (message "Hashes match, not tangling settings."))

;; Load the tangled settings
(let ((load-start (float-time (current-time))))
  (load org-dst)
  (let ((org-end (float-time (current-time))))
    (message "Load.. done (%.3fs)" (float-time (time-subtract (current-time)
                                                              load-start)))))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Init.el finished...done (%.3fs)" elapsed))

(provide 'init)

;;; init.el ends here

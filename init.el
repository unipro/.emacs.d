;;; init.el -- My personal emacs settings

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "init.ec" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Package manager settings
;;----------------------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))


;;----------------------------------------------------------------------------
;; Load common packages
;;----------------------------------------------------------------------------
(require-package 'diminish)


;;----------------------------------------------------------------------------
;; TRAMP
;;----------------------------------------------------------------------------
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
	     buffer-file-name))))


;;----------------------------------------------------------------------------
;; dired
;;----------------------------------------------------------------------------
(require 'dired-x)
(setq dired-omit-mode t)
(setq-default dired-omit-files-p t)


;;----------------------------------------------------------------------------
;; flycheck
;;----------------------------------------------------------------------------
(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
	flycheck-idle-change-delay 0.8)

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))


;;----------------------------------------------------------------------------
;; undo-tree
;;----------------------------------------------------------------------------
(require-package 'undo-tree)
(diminish 'undo-tree-mode)


;;----------------------------------------------------------------------------
;; Editing
;;----------------------------------------------------------------------------
(require-package 'whole-line-or-region)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(with-eval-after-load 'page-break-lines
  (push 'browse-kill-ring-mode page-break-lines-modes))


;;----------------------------------------------------------------------------
;; Locales
;;----------------------------------------------------------------------------
(when window-system
  (setq coding-system-for-read 'utf-8))


;;----------------------------------------------------------------------------
;; windmove
;;----------------------------------------------------------------------------
(windmove-default-keybindings)


;;----------------------------------------------------------------------------
;; helm
;;----------------------------------------------------------------------------
(require-package 'helm)
(require 'helm-config)


;;----------------------------------------------------------------------------
;; Fonts
;;----------------------------------------------------------------------------
(when (display-graphic-p)
  (let ((fontset "fontset-default"))
    (cond ((member "Droid Sans Mono" (font-family-list))
           (set-frame-font "Droid Sans Mono")
           (set-face-font 'default "Droid Sans Mono"))
          ((member "DejaVu Sans Mono" (font-family-list))
           (set-frame-font "DejaVu Sans Mono")
           (set-face-font 'default "DejaVu Sans Mono")))
    (when (member "나눔고딕코딩" (font-family-list))
      (set-fontset-font fontset 'hangul
                        '("NanumGothicCoding" . "unicode-bmp")))))


;;----------------------------------------------------------------------------
;; Themes
;;----------------------------------------------------------------------------
(require-package 'color-theme-sanityinc-solarized)


;;----------------------------------------------------------------------------
;; Whitespace
;;----------------------------------------------------------------------------
(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)


;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
(require-package 'magit)


;;----------------------------------------------------------------------------
;; misc
;;----------------------------------------------------------------------------
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here

;;; init.el -- My personal emacs settings

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "init.ec" user-emacs-directory))
(setq gc-cons-threshold (* 128 1024 1024))


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
;; Buffers, Files, Directories
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq recentf-mode t)
(setq recentf-max-saved-items 1000)
(setq recentf-exclude '("/tmp/" "/ssh:"))

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
	     buffer-file-name))))
(setq tramp-default-method "ssh")

(with-eval-after-load 'dired
  (require 'dired-x)
  (setq dired-omit-mode t)
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$"))

(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)


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

(require-package 'undo-tree)
(diminish 'undo-tree-mode)
(global-undo-tree-mode t)


;;----------------------------------------------------------------------------
;; Locales
;;----------------------------------------------------------------------------
(when window-system
  (setq coding-system-for-read 'utf-8))

(setq default-input-method "korean-hangul")


;;----------------------------------------------------------------------------
;; Windows, Frames, Fonts, Themes
;;----------------------------------------------------------------------------
(windmove-default-keybindings)
(setq shift-select-mode nil)
(setq windmove-wrap-around t)
(winner-mode t)

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

(require-package 'color-theme-sanityinc-solarized)

(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(setq use-dialog-box nil)
(setq use-file-dialog nil)

(global-set-key (kbd "M-i") 'imenu)

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "Byungwan Jun")


;;----------------------------------------------------------------------------
;; Syntax utilities
;;----------------------------------------------------------------------------
(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
	flycheck-idle-change-delay 0.8)

  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list))

(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

(setq indicate-empty-lines t)

(electric-pair-mode t)


;;----------------------------------------------------------------------------
;; Auto Completion
;;----------------------------------------------------------------------------
(require-package 'helm)
(require 'helm-config)


;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
(require-package 'magit)


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

;;; init.el -- My personal emacs settings

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(defconst *is-a-mac* (eq system-type 'darwin))


;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq gc-cons-threshold (* 128 1024 1024))


;;----------------------------------------------------------------------------
;; Package manager settings
;;----------------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish) ;; if you use :diminish
(require 'bind-key) ;; if you use any :bind variant

;; ignore the signature checks
;; (setq package-check-signature nil)


;;----------------------------------------------------------------------------
;; macOS key bindings
;;----------------------------------------------------------------------------
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))


;;----------------------------------------------------------------------------
;; Buffers, Files, Directories
;;----------------------------------------------------------------------------
(use-package ibuffer
  :bind
  (("C-x C-b" . ibuffer)))

(use-package recentf-mode
  :bind
  (("C-x C-r" . recentf-open-files))
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '("/tmp/" "/ssh:")))

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))
(setq tramp-default-method "ssh")

(use-package dired
  :config
  (use-package dired-x
    :init
    (setq dired-omit-mode t)
    (setq-default dired-omit-files-p t)
    (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")))

(use-package ido-mode
  :init
  (ido-mode t)
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t))


;;----------------------------------------------------------------------------
;; Editing
;;----------------------------------------------------------------------------
(use-package whole-line-or-region)

(use-package hippie-expand
  :bind
  (([remap dabbrev-expand] . hippie-expand)) ; M-/
  :init
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))

(use-package browse-kill-ring
  :bind
  (("M-Y" . browse-kill-ring))
  :init
  (setq browse-kill-ring-separator "\f")
  :config
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))

(use-package page-break-lines
  :diminish page-break-lines-mode
  ;; :init
  ;; (global-page-break-lines-mode)
  )

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode t))


;;----------------------------------------------------------------------------
;; Locales
;;----------------------------------------------------------------------------
(when window-system
  (setq coding-system-for-read 'utf-8))

(set-language-environment "Korean")
(setq default-input-method "korean-hangul")


;;----------------------------------------------------------------------------
;; Windows, Frames, Fonts, Themes
;;----------------------------------------------------------------------------
(use-package windmove
  :init
  (windmove-default-keybindings)
  (setq shift-select-mode nil)
  (setq windmove-wrap-around t))

(use-package winner
  :init
  (winner-mode t))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'show-paren-mode) (show-paren-mode 1))
(setq use-dialog-box nil)
(setq use-file-dialog nil)

(use-package imenu
  :bind
  (("M-i" . imenu)))

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "Byungwan Jun")

(when (display-graphic-p)
  (let ((fontset "fontset-default"))
    (cond ((member "Droid Sans Mono" (font-family-list))
           (set-frame-font "Droid Sans Mono")
           (set-face-font 'default "Droid Sans Mono"))
          ((member "DejaVu Sans Mono" (font-family-list))
           (set-frame-font "DejaVu Sans Mono")
           (set-face-font 'default "DejaVu Sans Mono")))
    (when (or (member "나눔고딕코딩" (font-family-list))
              (member "NanumGothicCoding" (font-family-list))) 
      (set-fontset-font fontset 'hangul
                        '("NanumGothicCoding" . "unicode-bmp")))))

(use-package color-theme-sanityinc-solarized)


;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
(use-package magit
  :bind ("C-c g" . magit-status))


;;----------------------------------------------------------------------------
;; Syntax utilities
;;----------------------------------------------------------------------------
(use-package flycheck
  :commands flycheck-mode
  :init
  ;; (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 5.0))

(use-package whitespace-cleanup-mode
  :init
  (global-whitespace-cleanup-mode t))

(setq-default indicate-empty-lines t)

(use-package electric-pair
  :init
  (electric-pair-mode t))


;;------------------------------------------------------------------------------
;; helm
;;------------------------------------------------------------------------------

(use-package helm
  :diminish helm-mode
  :bind
  (("C-c h" . helm-mini)
   ("C-h a" . helm-apropos)
   ("C-x C-b" . helm-buffers-list)
   ;; ("C-x b" . helm-buffers-list)
   ("M-y" . helm-show-kill-ring)
   ("M-x" . helm-M-x)
   ("C-x c o" . helm-occur)
   ("C-x c s" . helm-swoop)
   ("C-x c y" . helm-yas-complete)
   ("C-x c Y" . helm-yas-create-snippet-on-region)
   ("C-x c b" . my/helm-do-grep-book-notes)
   ("C-x c SPC" . helm-all-mark-rings)
   ("C-x C-o" . ffap))
  :init
  (use-package helm-config
    :init
    (autoload 'helm-c-yas-complete "helm-c-yasnippet" nil t))
  (use-package helm-ls-git
    :init
    (autoload 'helm-ls-git-ls "helm-ls-git" nil t)
    (autoload 'helm-browse-project "helm-ls-git" nil t))
  (use-package helm-gtags
    :config
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                  (helm-gtags-mode 1))))
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)))



;;------------------------------------------------------------------------------
;; company
;;------------------------------------------------------------------------------
(use-package company
  :diminish company-mode
  :init
  (if (fboundp 'evil-declare-change-repeat)
      (mapc #'evil-declare-change-repeat
            '(company-complete-common
              company-select-next
              company-select-previous
              company-complete-selection
              company-complete-number)))
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (use-package company-statistics
    :init
    (company-statistics-mode))
  (dolist ((package '(company-cmake
                      company-c-headers)))
    (use-package package
      :init
      (add-to-list 'company-backends package)))
  (setq company-backends (delete 'company-semantic company-backends))

  ;; Use Helm to complete suggestions
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))


;;------------------------------------------------------------------------------
;; yasnippet
;;------------------------------------------------------------------------------
(use-package yasnippet
  :init
  (yas-global-mode 1))


;;------------------------------------------------------------------------------
;; gtags
;;------------------------------------------------------------------------------

(use-package ggtags
  ;; :config
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
  ;;               (ggtags-mode 1))))
  ;; (add-hook 'eshell-mode-hook 'ggtags-mode)
  ;; (add-hook 'dired-mode-hook 'ggtags-mode)

  ;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  ;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  ;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  ;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  ;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  ;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  ;; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
  )


;;------------------------------------------------------------------------------
;; hs-minor
;;------------------------------------------------------------------------------
(add-hook 'c-mode-common-hook 'hs-minor-mode)


;;------------------------------------------------------------------------------
;; doxygen
;;------------------------------------------------------------------------------
(autoload 'doxygen-insert-function-comment
  "doxygen" "insert comment for the function at point" t)
(autoload 'doxygen-insert-file-comment
  "doxygen" "insert comment for file" t)
(autoload 'doxygen-insert-member-group-region
  "doxygen" "insert comment for member group" t)
(autoload 'doxygen-insert-compound-comment
  "doxygen" "insert comment for compound" t)


;;------------------------------------------------------------------------------
;; programming
;;------------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)


;;------------------------------------------------------------------------------
;; C & C++
;;------------------------------------------------------------------------------
(use-package cmake-mode
  :init
  (add-hook 'cmake-mode-hook 'global-company-mode))

(use-package c-eldoc)

(defun my-c-mode-setup ()
  (setq c-basic-offset 4
        indent-tabs-mode nil
        tab-width 4
        c-tab-always-indent t)

  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
  (setq cc-search-directories '("."
                                "/usr/include"
                                "/usr/local/include/*"
                                "../*/include"))

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))

  (autoload 'c-turn-on-eldoc-mode "c-eldoc" "" t)

  (when buffer-file-name
    (c-turn-on-eldoc-mode)

    (if (executable-find "cmake")
        (if (not (or (string-match "^/usr/local/include/.*" buffer-file-name)
                     (string-match "^/usr/src/linux/include/.*" buffer-file-name)))
            (cppcm-reload-all)))))

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "K&R")
            (my-c-mode-setup)))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "Stroustrup")
            (my-c-mode-setup)))


;;------------------------------------------------------------------------------
;; GDB & GUD
;;------------------------------------------------------------------------------
;; use gdb-many-windows by default
(setq gdb-many-windows t)
;; Non-nil means display source file containing the main routine
;; at startup
(setq gdb-show-main t)

(global-set-key "\C-x\C-a\C-g" 'gud-run)


;;------------------------------------------------------------------------------
;; Paredit
;;------------------------------------------------------------------------------
(use-package paredit
  :init
  (use-package paredit-everywhere)
  :config
  (with-eval-after-load 'paredit
    (diminish 'paredit-mode " Par")))


;;------------------------------------------------------------------------------
;; Emacs Lisp
;;------------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))


;;------------------------------------------------------------------------------
;; Common Lisp & SLIME
;;------------------------------------------------------------------------------
(add-hook 'lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq common-lisp-hyperspec-root (expand-file-name "~/Documents/HyperSpec/"))
;; To use C-h S in Lisp mode to look up the symbol at point
;; in the spec.
(require 'info-look)
(info-lookup-add-help :mode 'lisp-mode
                      :regexp "[^][()'\" \t\n]+"
                      :ignore-case t
                      :doc-spec '(("(ansicl)Symbol Index"
                                   nil nil nil)))

(use-package slime
  :commands slime
  :init
  (setq inferior-lisp-program (or (executable-find "sbcl")
                                  (executable-find "/usr/bin/sbcl")
                                  (executable-find "/usr/local/bin/sbcl")
                                  "sbcl"))
  :config
  (require 'slime-autoloads)
  ;; (slime-setup)
  (slime-setup '(slime-fancy))

  (mapc #'(lambda (top-dir)
            (let* ((file-name (concat top-dir
                                      "quicklisp/slime-helper.el")))
              (when (file-exists-p file-name)
                (load file-name))))
        (list "/opt/" "~/")))


;;------------------------------------------------------------------------------
;; Clojure & CIDER
;;------------------------------------------------------------------------------
(use-package clojure-mode
  :init
  (use-package flycheck-clojure
    :config
    (flycheck-clojure-setup))
  :config
  (add-hook 'clojure-mode-hook (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'clojure-mode-hook 'subword-mode))

(use-package cider
  :commands (cider cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode))


;;------------------------------------------------------------------------------
;; scheme & geiser
;;------------------------------------------------------------------------------
(add-hook 'scheme-mode-hook (lambda () (setq indent-tabs-mode nil)))

(use-package geiser
  :init
  (setq geiser-active-implementations '(guile))
  (setq geiser-guile-binary (or (executable-find "guile")
                                (executable-find "/usr/bin/guile")
                                (executable-(format "message" format-args)ind "/usr/local/bin/guile")
                                "guile")))


;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------
(use-package elpy
  :init
  (elpy-enable)
  (elpy-use-ipython)
  (setq python-shell-interpreter-args "--simple-prompt -i"))

(use-package ein)


;;------------------------------------------------------------------------------
;; Perl
;;------------------------------------------------------------------------------
(defalias 'perl-mode 'cperl-mode)


;;------------------------------------------------------------------------------
;; Lua
;;------------------------------------------------------------------------------
(use-package lua-mode)


;;------------------------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------------------------
(use-package markdown-mode
  :config
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))


;;------------------------------------------------------------------------------
;; sr-speedbar
;;------------------------------------------------------------------------------
(use-package sr-speedbar
  :init
  (setq sr-speedbar-auto-refresh nil)
  ;; (setq speedbar-show-unknown-files t) ; show all files
  ;; (setq speedbar-use-images nil) ; use text for buttons
  (setq sr-speedbar-right-side nil) ; put on left side
  )


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

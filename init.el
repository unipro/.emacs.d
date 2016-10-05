;;; init.el -- My personal emacs settings

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of sections.

;;; Code:

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(defconst *is-a-mac* (eq system-type 'darwin))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq gc-cons-threshold (* 128 1024 1024))

(defalias 'yes-or-no-p 'y-or-n-p)


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

(setq use-package-verbose t)

(eval-when-compile
  (require 'use-package))
(require 'diminish) ;; if you use :diminish
(require 'bind-key) ;; if you use any :bind variant

;; ignore the signature checks
(setq package-check-signature nil)


;;----------------------------------------------------------------------------
;; macOS key bindings
;;----------------------------------------------------------------------------
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))


;;----------------------------------------------------------------------------
;; Locales
;;----------------------------------------------------------------------------
(when window-system
  (setq coding-system-for-read 'utf-8))

(set-language-environment "Korean")
(setq default-input-method "korean-hangul")


;;----------------------------------------------------------------------------
;; Buffers, Files, Directories
;;----------------------------------------------------------------------------
(setq large-file-warning-threshold (* 100 1000 1000)) ; 100 MB

(use-package ibuffer
  :bind
  (("C-x C-b" . ibuffer)))

(use-package recentf
  :bind
  (("C-x C-r" . recentf-open-files))
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '("/tmp/" "/ssh:")))

(use-package dired
  :config
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-listing-switches "-lahp")
  (use-package dired-x
    :config
    (setq dired-omit-mode t)
    (setq-default dired-omit-files-p t)
    (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")))

(use-package ido-mode
  :init
  (ido-mode t)
  :config
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (defun sudo ()
    "Use TRAMP to `sudo' the current buffer"
    ((insert )nteractive)
    (when buffer-file-name
      (find-alternate-file
       (concat "/sudo:root@localhost:"
               buffer-file-name)))))


;;----------------------------------------------------------------------------
;; Editing
;;----------------------------------------------------------------------------
(use-package whole-line-or-region)

(use-package hippie-expand
  :bind
  (([remap dabbrev-expand] . hippie-expand)))

(use-package browse-kill-ring
  :bind
  (("M-Y" . browse-kill-ring)
   :map browse-kill-ring-mode-map
   ("C-g" . browse-kill-ring-quit)
   ("M-n" . browse-kill-ring-forward)
   ("M-p" . browse-kill-ring-previous))
  :config
  (setq browse-kill-ring-separator "\f"))

(setq undo-limit 100000)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode t))

(add-hook 'text-mode-hook 'turn-on-auto-fill)


;;----------------------------------------------------------------------------
;; Tabs, spaces, lines and parenthesis
;;----------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(use-package electric-pair
  :init
  (electric-pair-mode t))

;; (setq-default indicate-empty-lines t)

(use-package page-break-lines-mode
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))


;;----------------------------------------------------------------------------
;; Windows, Frames, Fonts, Themes
;;----------------------------------------------------------------------------
(when (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq use-dialog-box nil)
  (setq use-file-dialog nil))

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "Byungwan Jun")

(use-package windmove
  :init
  (windmove-default-keybindings)
  (setq shift-select-mode nil)
  (setq windmove-wrap-around t))

(use-package winner
  :init
  (winner-mode t))

(use-package imenu
  :commands imenu
  :bind
  (("M-i" . imenu)))

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
;; flycheck, flyspell, etc
;;----------------------------------------------------------------------------
(use-package flycheck
  :diminish flycheck-mode
  :commands flycheck-mode
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 5.0))

;; flyspell - use aspell instead of ispell
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config (setq ispell-program-name (executable-find "aspell")
                ispell-extra-args '("--sug-mode=ultra")))


;;----------------------------------------------------------------------------
;; whitespace
;;----------------------------------------------------------------------------
(use-package whitespace)

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :init
  (global-whitespace-cleanup-mode))


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
  (require 'helm-config)
  (setq helm-candidate-number-limit 100)
  (setq helm-yas-display-key-on-candidate t)
  ;; for pretty fast updates when hitting RET too quickly after typing fast:
  (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
        helm-input-idle-delay 0.01  ; this actually updates things
                                    ; reeeelatively quickly.
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t))

(use-package helm-descbinds
  :config (helm-descbinds-mode))


;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
(use-package magit
  :commands (magit-status projectile-vc)
  :bind ("C-c g" . magit-status))

(use-package helm-ls-git
    :bind
    (("C-x d" . helm-ls-git-ls)
     ("C-x C-d" . helm-browse-project)))

(use-package helm-gitignore)


;;------------------------------------------------------------------------------
;; projectile
;;------------------------------------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile)
  (projectile-global-mode))

(use-package helm-projectile
  :commands (helm-projectile)
  :config (helm-projectile-on))


;;------------------------------------------------------------------------------
;; company
;;------------------------------------------------------------------------------
(use-package company
  :diminish company-mode
  :commands (company-complete company-mode)
  :bind
  (("M-/" . company-complete))
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
    (company-statistics-mode)))

(use-package helm-company
  :commands (helm-company)
  :config (company-mode))


;;------------------------------------------------------------------------------
;; yasnippet
;;------------------------------------------------------------------------------
(use-package yasnippet
  :commands (yas-expand yas-insert-snippet)
  :config
  (use-package java-snippets)
  (yas-minor-mode))


;;------------------------------------------------------------------------------
;; gtags
;;------------------------------------------------------------------------------
(use-package helm-gtags
    :bind
    (:map helm-gtags-mode-map
          ("C-c g a" . helm-gtags-tags-in-this-function)
          ("C-j" . helm-gtags-select)
          ("M-." . helm-gtags-dwim)
          ("M-," . helm-gtags-pop-stack)
          ("C-c <" . helm-gtags-previous-history)
          ("C-c >" . helm-gtags-next-history))
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
    (add-hook 'dired-mode-hook 'helm-gtags-mode))

(use-package ggtags
  :commands ggtags-mode
  :diminish ggtags-mode
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
;; cc-mode
;;------------------------------------------------------------------------------
(use-package cc-mode
  :config
  (use-package company-c-headers)
  (add-to-list 'company-backends 'company-c-headers)
  (setq company-backends (delete 'company-semantic company-backends))

  (use-package c-eldoc
    ;; :init
    ;; (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
    :config
    (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
    (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode))

  (add-hook 'c-mode-common-hook
            (lambda ()
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
              (setq c-electric-pound-behavior 'alignleft)))

  (add-hook 'c-mode-hook
            (lambda () (c-set-style "K&R")))

  (add-hook 'c++-mode-hook
            (lambda ()
              (c-set-style "Stroustrup"))))


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
  :diminish paredit-mode
  :init
  (use-package paredit-everywhere))


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
(use-package cperl-mode
  :config
  (defalias 'perl-mode 'cperl-mode))


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
;; nginx
;;------------------------------------------------------------------------------
(use-package nginx-mode
  :mode ("nginx.conf$" "/etc/nginx/.*"))


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

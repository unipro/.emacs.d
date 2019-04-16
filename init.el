;; -*- coding: utf-8; -*-
;;; init.el -- My personal emacs settings

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of sections.

;;; Code:

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win-nt* (eq system-type 'windows-nt))
(defconst env-file (expand-file-name "env.el" user-emacs-directory))
(defconst local-file (expand-file-name "local.el" user-emacs-directory))
(defconst abbrev-file (expand-file-name "abbrev.el" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun gc-disable ()
  "Disable automatic garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))
(defun gc-enable ()
  "Enable automatic garbage collection."
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'gc-disable)
(add-hook 'minibuffer-exit-hook #'gc-enable)
(gc-enable)

(defalias 'yes-or-no-p 'y-or-n-p)


;;----------------------------------------------------------------------------
;; Package manager settings
;;----------------------------------------------------------------------------
(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(setq package-enable-at-startup nil)
(setq package-archive-priorities '(("melpa-stable" . 1)))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t)
;; (setq use-package-always-ensure t)
(setq use-package-always-bin "melpa-stable")

(eval-when-compile
  (require 'use-package))
(require 'bind-key) ;; if you use any :bind variant
(use-package diminish :ensure t) ;; if you use :diminish

;; ignore the signature checks
(setq package-check-signature nil)

(when (file-exists-p env-file)
  (load env-file))

;; Confirm before exit
(unless (daemonp)
  (setq confirm-kill-emacs 'y-or-n-p))

;; exec-path, PATH
(defun add-to-path (path)
  "Add the path directory to the `exec-path' and `PATH' variables."
  (when (file-directory-p path)
    (let ((path-env (getenv "PATH")))
        (when (not (cl-search path path-env))
       (setenv "PATH" (concat path ":" path-env))))
    (add-to-list 'exec-path path)))

(defconst home-bin-path (expand-file-name "bin" "~"))
(defconst home-local-bin-path (expand-file-name ".local/bin" "~"))

(add-to-path home-bin-path)
(add-to-path home-local-bin-path)


;;----------------------------------------------------------------------------
;; macOS key bindings
;;----------------------------------------------------------------------------
(when *is-a-mac*
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier 'none)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PATH")))


;;----------------------------------------------------------------------------
;; Locale, environment
;;----------------------------------------------------------------------------
(require 'ucs-normalize)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-write 'utf-8)
(cond (*is-a-win-nt*
       (setq-default coding-system-for-read 'utf-8) ; XXX 'utf-16-le
       (set-clipboard-coding-system 'utf-16-le)
       (set-selection-coding-system 'utf-16-le))
      (*is-a-mac*
       (setq-default coding-system-for-read 'utf-8-hfs)
       (set-clipboard-coding-system 'utf-8-hfs)
       (set-selection-coding-system 'utf-8-hfs)
       (set-file-name-coding-system 'utf-8-hfs)
       (setq default-process-coding-system '(utf-8-hfs . utf-8-hfs)))
      (t  ; linux
       (setq-default coding-system-for-read 'utf-8)
       (setq x-select-request-type
             '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))
(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)


;;----------------------------------------------------------------------------
;; Buffers, Files, Directories
;;----------------------------------------------------------------------------
(setq large-file-warning-threshold (* 100 1000 1000)) ; 100 MB

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 1000
        recentf-exclude '("/tmp/" "/ssh:")))

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

(use-package ido
  :init (ido-mode t)
  :config
  (setq ido-everywhere t
        ido-enable-flex-matching t))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (defun sudo ()
    "Use TRAMP to `sudo' the current buffer"
    (interactive)
    (when buffer-file-name
      (find-alternate-file (concat "/sudo:root@localhost:"
                                   buffer-file-name)))))


;;----------------------------------------------------------------------------
;; Editing
;;----------------------------------------------------------------------------
(use-package whole-line-or-region
  :ensure t)

(use-package hippie-expand
  :bind (([remap dabbrev-expand] . hippie-expand)))

(use-package browse-kill-ring
  :ensure t
  :bind (("M-Y" . browse-kill-ring)
         :map browse-kill-ring-mode-map
         ("C-g" . browse-kill-ring-quit)
         ("M-n" . browse-kill-ring-forward)
         ("M-p" . browse-kill-ring-previous))
  :config (setq browse-kill-ring-separator "\f"))

(setq undo-limit 100000)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode t))

(add-hook 'text-mode-hook 'turn-on-auto-fill)


;;----------------------------------------------------------------------------
;; Tabs, spaces, lines and parenthesis
;;----------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode t)

;; (setq-default indicate-empty-lines t)

(use-package page-break-lines
  :ensure page-break-lines
  :diminish page-break-lines-mode
  :init (global-page-break-lines-mode))


;;----------------------------------------------------------------------------
;; Windows, Frames, Fonts, Themes
;;----------------------------------------------------------------------------
(when (display-graphic-p)
  ;; (menu-bar-mode -1)
  (tool-bar-mode -1)
  ;; (scroll-bar-mode -1)
  (set-scroll-bar-mode 'right)
  (setq use-dialog-box nil)
  (setq use-file-dialog nil))

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "Byungwan Jun")

(use-package windmove
  :ensure t
  :bind (("C-c j" . windmove-left)
         ("C-c l" . windmove-right)
         ("C-c i" . windmove-up)
         ("C-c k" . windmove-down))
  :init
  (windmove-default-keybindings)
  (setq shift-select-mode nil
        windmove-wrap-around t))

(use-package ace-window
  :ensure t
  :bind (("M-p" . ace-window)
         ("C-x o" . ace-window)))

(use-package winner
  :bind (:map winner-mode-map
         ("C-c w u" . winner-undo)
         ("C-c w r" . winner-redo))
  :init (winner-mode t))

(use-package imenu
  :bind ("M-i" . imenu))

(when (display-graphic-p)
  (let ((fontset "fontset-default"))
    (cond ((member "Droid Sans Mono" (font-family-list))
           (set-fontset-font fontset 'unicode "Droid Sans Mono")
           (set-face-font 'default "Droid Sans Mono"))
          ((member "DejaVu Sans Mono" (font-family-list))
           (set-fontset-font fontset 'unicode "DejaVu Sans Mono")
           (set-face-font 'default "DejaVu Sans Mono"))
          (t
           (message "'Droid Sans Mono' or 'DejaVu Sans Mono' are not installed")))
    (cond ((member "D2Coding" (font-family-list))
           (set-fontset-font fontset 'hangul
                             '("D2Coding" . "unicode-bmp")))
          ((member "NanumGothicCoding" (font-family-list))
           (set-fontset-font fontset 'hangul
                             '("NanumGothicCoding" . "unicode-bmp")))
          ((member "나눔고딕코딩" (font-family-list))
           (set-fontset-font fontset 'hangul
                             '("나눔고딕코딩" . "unicode-bmp")))
          ((member "나눔고딕코딩" (font-family-list))
           (set-fontset-font fontset 'hangul
                             '("나눔고딕코딩" . "unicode-bmp")))
          (t
           (message "'D2Coding' or 'NanumGothicCoding' are not installed")))))

(use-package zenburn-theme
  :ensure t
  :if (display-graphic-p)
  :init
  (setq custom-enabled-themes '(zenburn))
  (setq custom-safe-themes
        '("84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba"
          default)))


;;----------------------------------------------------------------------------
;; diff, ediff
;;----------------------------------------------------------------------------
(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))


;;----------------------------------------------------------------------------
;; flycheck, flyspell, etc
;;----------------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :commands flycheck-mode
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 5.0))

;; flyspell - use aspell instead of ispell
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  (setq ispell-program-name (executable-find "aspell")
        ispell-extra-args '("--sug-mode=ultra")))


;;----------------------------------------------------------------------------
;; whitespace
;;----------------------------------------------------------------------------
(use-package whitespace)

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode))


;;-----------------------------------------------------------------------------
;; helm
;;-----------------------------------------------------------------------------
(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ;; ("C-x C-b" . helm-buffers-list)
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
  ;; for pretty fast updates when hitting RET too quickly
  ;; after typing fast:
  (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
        helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t)
  :config
  (use-package helm-descbinds
    :ensure t
    :config (helm-descbinds-mode)))

;;; https://github.com/syohex/emacs-helm-ag
(use-package helm-ag
  :ensure t)


;;----------------------------------------------------------------------------
;; projectile
;;----------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile)
  ;; https://github.com/bbatsov/projectile/issues/1183
  (setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile)
  :after helm
  :config (helm-projectile-on))


;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
(use-package magit
  :ensure t
  :commands (magit-status projectile-vc)
  :init
  (if *is-a-win-nt*
      (setenv "SSH_ASKPASS" "git-gui--askpass")))

;; (use-package helm-ls-git
;;   :ensure t
;;   :bind (("C-x d" . helm-ls-git-ls)
;;          ("C-x C-d" . helm-browse-project)))

;; (use-package helm-gitignore)


;;----------------------------------------------------------------------------
;; Subversion
;;----------------------------------------------------------------------------
;; (use-package psvn
;;   :ensure t
;;   ;; :pin melpa
;;   :commands (svn-status svn-examine projectile-vc)
;;   :config
;;   (setq svn-status-hide-unmodified t
;;         svn-status-hide-unknown t
;;         svn-status-svn-file-coding-system 'utf-8))


;;----------------------------------------------------------------------------
;; org-mode
;;----------------------------------------------------------------------------
(use-package org
  :init
  (setq org-log-done t
        org-use-fast-todo-selection t
        org-todo-keywords '((type "TODO(t)"
                                  "STARTED(s)"
                                  "WAITING(w@/!)"
                                  "|"
                                  "DONE(d!/!)"
                                  "CANCELLED(c@/!)"))
        org-todo-keyword-faces '(("TODO"
                                  :foreground "red"
                                  :weight bold)
                                 ("STARTED"
                                  :foreground "blue"
                                  :weight bold)
                                 ("DONE"
                                  :foreground "forest green"
                                  :weight bold)
                                 ("WAITING"
                                  :foreground "orange"
                                  :weight bold)
                                 ("CANCELLED"
                                  :foreground "forest green"
                                  :weight bold))
        org-directory "~/org"
        org-default-notes-file (concat org-directory "/refile.org")
        org-agenda-files `(,(concat org-directory "/work.org"))
        org-mobile-directory "~/Dropbox/org"
        org-mobile-inbox-for-pull (concat org-directory "/refile.org")
        org-mobile-files `(,(concat org-directory "/work.org")))
  ;; make org mode allow eval of some langs
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (clojure . t)
                                 (python . t)
                                 (ruby . t)))
  :config
  (add-hook 'org-mode-hook 'company-mode)
  ;; http://blog.zhengdong.me/2012/06/16/org-my-life/
  (defvar org-mobile-sync-timer nil)
  (defvar org-mobile-sync-idle-secs (* 60 10))
  (defun org-mobile-sync ()
    (interactive)
    (org-mobile-pull)
    (org-mobile-push))
  (defun org-mobile-sync-enable ()
    "enable mobile org idle sync"
    (interactive)
    (setq org-mobile-sync-timer
          (run-with-idle-timer org-mobile-sync-idle-secs t
                               'org-mobile-sync)));
  (defun org-mobile-sync-disable ()
    "disable mobile org idle sync"
    (interactive)
    (cancel-timer org-mobile-sync-timer))
  ;; (org-mobile-sync-enable)
  )


;;----------------------------------------------------------------------------
;; company
;;----------------------------------------------------------------------------
(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-complete company-mode)
  :bind (([remap dabbrev-expand] . company-complete)
         :map prog-mode-map
         ([tab] . company-indent-or-complete-common))
  :init (if (fboundp 'evil-declare-change-repeat)
            (mapc #'evil-declare-change-repeat
                  '(company-complete-common
                    company-select-next
                    company-select-previous
                    company-complete-selection
                    company-complete-number)))
  ;; (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-statistics
              :ensure t
              :init
              (company-statistics-mode))
  (setq company-idle-delay 0)
  (setq company-show-numbers "on")
  (add-hook 'prog-mode-hook 'company-mode))

(use-package helm-company
  :ensure t
  :commands (helm-company)
  :config (company-mode))


;;----------------------------------------------------------------------------
;; yasnippet
;;----------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :commands (yas-expand yas-insert-snippet)
  :config
  (yas-minor-mode))


;;----------------------------------------------------------------------------
;; gtags
;;----------------------------------------------------------------------------
(use-package helm-gtags
  :ensure t
  :bind (:map helm-gtags-mode-map
         ("C-c g a" . helm-gtags-tags-in-this-function)
         ("C-j" . helm-gtags-select)
         ("M-." . helm-gtags-dwim)
         ("M-," . helm-gtags-pop-stack)
         ("C-c <" . helm-gtags-previous-history)
         ("C-c >" . helm-gtags-next-history))
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update nil
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t)
  (add-hook 'dired-mode-hook 'helm-gtags-mode))

(use-package ggtags
  :ensure t
  :commands ggtags-mode
  :diminish ggtags-mode
  ;; :bind (:map ggtags-mode-map
  ;;        ("C-c g s" . ggtags-find-other-symbol)
  ;;        ("C-c g h" . ggtags-view-tag-history)
  ;;        ("C-c g r" . ggtags-find-reference)
  ;;        ("C-c g f" . ggtags-find-file)
  ;;        ("C-c g c" . ggtags-create-tags)
  ;;        ("C-c g u" . ggtags-update-tags)
  ;;        ("M-," . pop-tag-mark))
  ;; :config (add-hook 'dired-mode-hook 'ggtags-mode)
  )


;;----------------------------------------------------------------------------
;; hs-minor
;;----------------------------------------------------------------------------
(add-hook 'prog-mode-hook 'hs-minor-mode)


;;----------------------------------------------------------------------------
;; highlight-indent-guides
;;----------------------------------------------------------------------------
(use-package highlight-indent-guides
  :ensure t

  :config
  (setq highlight-indent-guides-method 'column)

  (when (not (display-graphic-p))
    (setq highlight-indent-guides-auto-enabled nil)
    (set-face-background 'highlight-indent-guides-odd-face "darkkhaki")
    (set-face-background 'highlight-indent-guides-even-face "khaki")
    (set-face-foreground 'highlight-indent-guides-character-face "khaki")))


;;----------------------------------------------------------------------------
;; doxygen
;;----------------------------------------------------------------------------
(autoload 'doxygen-insert-function-comment
  "doxygen" "insert comment for the function at point" t)
(autoload 'doxygen-insert-file-comment
  "doxygen" "insert comment for file" t)
(autoload 'doxygen-insert-member-group-region
  "doxygen" "insert comment for member group" t)
(autoload 'doxygen-insert-compound-comment
  "doxygen" "insert comment for compound" t)


;;----------------------------------------------------------------------------
;; C/C++
;;----------------------------------------------------------------------------
(when *is-a-win-nt*
  (setq compile-command "mingw32-make -f Makefile.mingw"))

(use-package cmake-mode
  :ensure t
  :config
  (setq cmake-tab-width 4))

(use-package company-cmake
  :after company)

(use-package cc-mode
  :init
  (use-package company-c-headers
    :ensure t
    ;; :pin melpa
    ;; :config
    ;; (add-to-list 'company-c-headers-path-system "/usr/include/c++/5/")
    )

  (add-to-list 'company-backends 'company-gtags)
  (add-to-list 'company-backends 'company-c-headers)
  (setq company-backends (delete 'company-semantic company-backends))

  (use-package c-eldoc
    :ensure t
    ;; :pin melpa
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
              (subword-mode t)
              (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
                (helm-gtags-mode 1))
              (when (derived-mode-p 'c-mode 'c++-mode)
                ;; emacs-c-opening-corresponding-header-file
                (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
                (setq cc-search-directories '("."
                                              "/usr/include"
                                              "/usr/local/include/*"
                                              "../*/include"))
                ;; make a #define be left-aligned
                (setq c-electric-pound-behavior '(alignleft)))))

  (add-hook 'c-mode-hook (lambda ()
                           (c-set-style "K&R")
                           (setq c-basic-offset 4)))
  (add-hook 'c++-mode-hook (lambda ()
                             (c-set-style "Stroustrup")
                             (cond (*is-a-mac* (setq flycheck-clang-language-standard "c++14"))
                                   (t (setq flycheck-gcc-language-standard "c++14"))))))


;;----------------------------------------------------------------------------
;; GDB & GUD
;;----------------------------------------------------------------------------
(use-package gdb-mi
  :commands (gdb)
  :init
  (setq gdb-many-windows nil
        gdb-show-main t))

(use-package gud
  :commands (gdb)
  :bind ("C-x C-a C-g" . gud-run)
  :init (add-hook 'gud-mode-hook #'gud-tooltip-mode))

;; (use-package realgud
;;   :ensure t)

(load (expand-file-name "gud-lldb.el" user-emacs-directory))


;;----------------------------------------------------------------------------
;; Rust
;;----------------------------------------------------------------------------
(add-to-path (expand-file-name ".cargo/bin" "~"))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save nil)
  (use-package flycheck-rust
    :ensure t
    :after flycheck
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package racer
  :ensure t
  :commands racer-mode
  ;; :hook
  ;; ((rust-mode . racer-mode)
  ;;  (rust-mode . eldoc-mode))
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (setq racer-rust-src-path (shell-command-to-string "echo -n `rustc --print sysroot`/lib/rustlib/src/rust/src"))
  :bind (:map rust-mode-map
         ("M-." . racer-find-definition))
  :config
  (use-package company-racer
    :ensure t
    :config
    (add-to-list 'company-backends 'company-racer)
    (setq company-tooltip-align-annotations t)))

(use-package cargo
  :ensure t
  :commands cargo-minor-mode
  ;; :hook (rust-mode . cargo-minor-mode)
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))


;;----------------------------------------------------------------------------
;; Java
;;----------------------------------------------------------------------------
(use-package groovy-mode
  :ensure t)

(use-package gradle-mode
  :ensure t)

(use-package meghanada
  :ensure t
  :commands (meghanada-mode)
  :init
  (setq meghanada-auto-start nil)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  (add-hook 'java-mode-hook
            (lambda ()
              (meghanada-mode t)
              (gradle-mode t)))
  (add-hook 'groovy-mode-hook
            (lambda ()
              (gradle-mode t)))
  (bind-key "C-c M-." 'meghanada-jump-declaration java-mode-map))


;;----------------------------------------------------------------------------
;; Paredit
;;----------------------------------------------------------------------------
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (use-package paredit-everywhere
    :ensure t))


;;----------------------------------------------------------------------------
;; Emacs Lisp
;;----------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq indent-tabs-mode nil)
                                  (turn-on-eldoc-mode)
                                  (eldoc-add-command
                                   'paredit-backward-delete
                                   'paredit-close-round)
                                  (paredit-mode t)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode t)))
(add-hook 'ielm-mode-hook (lambda () (paredit-mode t)))


;;----------------------------------------------------------------------------
;; Common Lisp & SLIME
;;----------------------------------------------------------------------------
(add-hook 'lisp-mode-hook (lambda ()
                            (setq indent-tabs-mode nil)
                            (paredit-mode t)
                            (helm-gtags-mode 1)))

(use-package slime
  :ensure t
  :commands slime
  :init
  (setq inferior-lisp-program (or (executable-find "sbcl")
                                  (executable-find "/usr/bin/sbcl")
                                  (executable-find "/usr/local/bin/sbcl")
                                  "sbcl"))
  :config
  (require 'slime-autoloads)
  (slime-setup '(slime-fancy))
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode t))))


;;----------------------------------------------------------------------------
;; Clojure & CIDER
;;----------------------------------------------------------------------------
(use-package clojure-mode
  :ensure t
  :config
  (use-package flycheck-clojure
    :ensure t
    :after flycheck
    :commands flycheck-clojure-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-clojure-setup))
  (add-hook 'clojure-mode-hook (lambda ()
                                 (setq indent-tabs-mode nil)

                                 ;; from Emacs and Clojure, a Lispy Love Affair
                                 (setq clojure-indent-style :always-align)
                                 ;; (put-clojure-indent 'symbol 2)
                                 ;; (put-clojure-indent 'GET 2)
                                 ;; (define-clojure-indent
                                 ;;   (-> 1)
                                 ;;   (letfn '(1 ((:defn)) nil))
                                 ;;   (defrecord '(2 :form :form (1))))

                                 (paredit-mode t)
                                 (subword-mode t))))

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook (lambda ()
                                    (turn-on-eldoc-mode)
                                    (paredit-mode t)
                                    (subword-mode t))))


;;----------------------------------------------------------------------------
;; Scala
;;----------------------------------------------------------------------------
(use-package ensime
  :ensure t)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition 'minibuffer-complete-word
                             'self-insert-command
                             minibuffer-local-completion-map))


;;----------------------------------------------------------------------------
;; scheme & geiser
;;----------------------------------------------------------------------------
(add-hook 'scheme-mode-hook (lambda ()
                              (setq indent-tabs-mode nil)
                              (paredit-mode t)
                              (helm-gtags-mode 1)))

(use-package geiser
  :ensure t
  :init
  (setq geiser-active-implementations '(guile))
  (setq geiser-guile-binary (or (executable-find "guile")
                                (executable-find "/usr/bin/guile")
                                (executable-find "/usr/local/bin/guile")
                                "guile")))


;;----------------------------------------------------------------------------
;; Python
;;----------------------------------------------------------------------------
(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)

  :init
  (setq-default indent-tabs-mode nil)
  (add-hook 'python-mode-hook #'highlight-indent-guides-mode)

  ;; electric-pair-mode and python triple quotes
  (defun python-electric-pair-string-delimiter ()
    (when (and electric-pair-mode
               (memq last-command-event '(?\" ?\'))
               (let ((count 0))
                 (while (eq (char-before (- (point) count)) last-command-event)
                   (setq count (1+ count)))
                 (= count 3)))
      (save-excursion (insert (make-string 3 last-command-event)))))

  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'post-self-insert-hook
                        #'python-electric-pair-string-delimiter 'append t)))

  :config
  (setq python-indent-offset 4))

(use-package cython-mode
  :ensure t)

(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :defer t
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t
  :commands (company-anaconda)
  :after company
  :init (add-to-list 'company-backends #'company-anaconda))

(use-package nose
  :commands (nosetests-one
             nosetests-pdb-one
             nosetests-all
             nosetests-pdb-all
             nosetests-module
             nosetests-pdb-module
             nosetests-suite
             nosetests-pdb-suite)
  :config
  (add-to-list 'nose-project-root-files "setup.cfg")
  (setq nose-use-verbose nil))

(use-package pytest
  :commands (pytest-one
             pytest-pdb-one
             pytest-all
             pytest-pdb-all
             pytest-module
             pytest-pdb-module)
  :config (add-to-list 'pytest-project-root-files "setup.cfg"))

(use-package pyenv-mode
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions))

(use-package pyvenv
  :defer t)


;;----------------------------------------------------------------------------
;; HTML
;;----------------------------------------------------------------------------
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.php\\'"))


;;----------------------------------------------------------------------------
;; Perl
;;----------------------------------------------------------------------------
(add-hook 'cperl-mode-hook (lambda () (helm-gtags-mode 1)))

(use-package cperl-mode
  :config (defalias 'perl-mode 'cperl-mode))


;;----------------------------------------------------------------------------
;; Erlang
;;----------------------------------------------------------------------------
(use-package erlang
  :ensure t
  ;; :config (require 'erlang-start)
  )

;; (use-package company-distel
;;   :ensure t
;;   :config
;;   (use-package company-distel-frontend
;;     :ensure t)
;;   (add-to-list 'company-backends 'company-distel))


;;----------------------------------------------------------------------------
;; Elixir
;;----------------------------------------------------------------------------
(use-package elixir-mode
  :ensure t)


;;----------------------------------------------------------------------------
;; Lua
;;----------------------------------------------------------------------------
(add-hook 'lua-mode-hook (lambda () (helm-gtags-mode 1)))

(use-package lua-mode
  :ensure t)


;;----------------------------------------------------------------------------
;; JavaScript and JSON
;;----------------------------------------------------------------------------
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :commands js2-mode
  :config (setq-default js2-basic-offset 2))

(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode-hook (lambda ()
                              (make-local-variable 'js-indent-level)
                              (setq js-indent-level 2))))


;;----------------------------------------------------------------------------
;; nxml
;;----------------------------------------------------------------------------
(use-package nxml-mode
  :mode ("\\.xml$" "\\.mpd$" "\\.isml?$" "\\.smil$")
  :init (add-to-list 'magic-mode-alist '("<\\?xml " . nxml-mode))
  :config
  (add-hook 'nxml-mode-hook
            (lambda ()
              (setq indent-tabs-mode t)
              (setq nxml-child-indent 8
                    nxml-attribute-indent 8)))
  (defun nxml-pretty-print (begin end)
    "Pretty-print selected region."
    (interactive "r")
    (save-excursion
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end)))
  (defun nxml-pretty-print-buffer ()
    "Pretty-print current region."
    (interactive)
    (nxml-pretty-print (point-min) (point-max))))


;;----------------------------------------------------------------------------
;; OpenCL
;;----------------------------------------------------------------------------
(use-package opencl-mode
  :ensure t
  :mode "\\.cl$")


;;----------------------------------------------------------------------------
;; YAML
;;----------------------------------------------------------------------------
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" "\\.yaml$" "\\.sls$")
  :init
  (add-hook 'yaml-mode-hook #'highlight-indent-guides-mode))


;;----------------------------------------------------------------------------
;; YAML
;;----------------------------------------------------------------------------
(use-package toml-mode
  :ensure t)


;;----------------------------------------------------------------------------
;; Markdown
;;----------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :config (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))


;;----------------------------------------------------------------------------
;; Vala
;;----------------------------------------------------------------------------
;; (use-package vala-mode
;;   :ensure t)


;;----------------------------------------------------------------------------
;; nginx
;;----------------------------------------------------------------------------
(use-package nginx-mode
  :ensure t
  :mode ("nginx.conf$" "/etc/nginx/.*"))


;;----------------------------------------------------------------------------
;; HCL
;;----------------------------------------------------------------------------
(use-package hcl-mode
  :ensure t
  :config (setq hcl-indent-level 4))


;;----------------------------------------------------------------------------
;; Terraform
;;----------------------------------------------------------------------------
(use-package terraform-mode
  :ensure t
  :config (setq terraform-indent-level 4))

(use-package company-terraform
  :ensure t
  :init (company-terraform-init))


;;----------------------------------------------------------------------------
;; sr-speedbar
;;----------------------------------------------------------------------------
(setq sr-speedbar-auto-refresh nil)
;; (setq speedbar-show-unknown-files t)
;; (setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)


;;----------------------------------------------------------------------------
;; Miscellaneous
;;----------------------------------------------------------------------------
(setq enable-local-variables :safe)
(add-to-list 'safe-local-variable-values
             '(eval add-to-list 'c-offsets-alist '(case-label . +)))
(add-to-list 'safe-local-variable-values
             '(eval (add-to-list 'c-offsets-alist '(case-label . +))))

(use-package ediff
  )

;; (setq make-backup-files nil)            ; stop creating backup~ files
;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq auto-save-default nil)          ; stop creating #autosave# files


;;----------------------------------------------------------------------------
;; Abbrev
;;----------------------------------------------------------------------------
(when (file-exists-p abbrev-file)
  (load abbrev-file))

;;----------------------------------------------------------------------------
;; Local configuration not shared by git
;;----------------------------------------------------------------------------
(when (file-exists-p local-file)
  (load local-file))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here

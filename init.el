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
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
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
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier 'none)
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PATH")))


;;----------------------------------------------------------------------------
;; Locale, environment
;;----------------------------------------------------------------------------
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(cond (*is-a-mac*
       (setq-default coding-system-for-read 'utf-8-hfs)
       (set-file-name-coding-system 'utf-8-hfs))
      (t
       (setq-default coding-system-for-read 'utf-8)
       (set-file-name-coding-system 'utf-8)))
(set-selection-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
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

(use-package ido-mode
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
(use-package electric-pair
  :init (electric-pair-mode t))

;; (setq-default indicate-empty-lines t)

(use-package page-break-lines-mode
  :diminish page-break-lines-mode
  :init (global-page-break-lines-mode))


;;----------------------------------------------------------------------------
;; Windows, Frames, Fonts, Themes
;;----------------------------------------------------------------------------
(when (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  ;; (scroll-bar-mode -1)
  (set-scroll-bar-mode 'right)
  (setq use-dialog-box nil)
  (setq use-file-dialog nil))

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "Byungwan Jun")

(use-package windmove
  :bind (("C-c j" . windmove-left)
         ("C-c l" . windmove-right)
         ("C-c i" . windmove-up)
         ("C-c k" . windmove-down))
  :init
  (windmove-default-keybindings)
  (setq shift-select-mode nil
        windmove-wrap-around t))

(use-package ace-window
  :bind (("M-p" . ace-window)))

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
          ((member "나눔고딕코딩" (font-family-list))
           (set-fontset-font fontset 'hangul
                             '("나눔고딕코딩" . "unicode-bmp")))
          (t
           (message "'D2Coding' or 'NanumGothicCoding' are not installed")))))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :commands (color-theme-sanityinc-solarized-dark
             color-theme-sanityinc-solarized-light)
  :if (display-graphic-p)
  :init
  (setq custom-enabled-themes '(sanityinc-solarized-light))
  (setq custom-safe-themes
        '("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
          "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328"
          default))
  (color-theme-sanityinc-solarized-light))


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


;;------------------------------------------------------------------------------
;; helm
;;------------------------------------------------------------------------------
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
        helm-ff-skip-boring-files t))

(use-package helm-descbinds
  :ensure t
  :config (helm-descbinds-mode))


;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
(use-package magit
  :ensure t
  :commands (magit-status projectile-vc))

;; (use-package helm-ls-git
;;   :ensure t
;;   :bind (("C-x d" . helm-ls-git-ls)
;;          ("C-x C-d" . helm-browse-project)))

(use-package helm-gitignore
  :ensure t)


;;----------------------------------------------------------------------------
;; Subversion
;;----------------------------------------------------------------------------
(use-package psvn
  :ensure t
  :commands (svn-status svn-examine projectile-vc)
  :config
  (setq svn-status-hide-unmodified t
        svn-status-hide-unknown t
        svn-status-svn-file-coding-system 'utf-8))


;;------------------------------------------------------------------------------
;; org-mode
;;------------------------------------------------------------------------------
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
  (org-mobile-sync-enable))


;;------------------------------------------------------------------------------
;; projectile
;;------------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile)
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile)
  :config (helm-projectile-on))


;;------------------------------------------------------------------------------
;; company
;;------------------------------------------------------------------------------
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
  :config
  (use-package company-statistics
              :ensure t
              :init
              (company-statistics-mode))
  (add-hook 'prog-mode-hook 'company-mode))

(use-package helm-company
  :ensure t
  :commands (helm-company)
  :config (company-mode))


;;------------------------------------------------------------------------------
;; yasnippet
;;------------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :commands (yas-expand yas-insert-snippet)
  :config
  (yas-minor-mode))


;;------------------------------------------------------------------------------
;; gtags
;;------------------------------------------------------------------------------
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


;;------------------------------------------------------------------------------
;; hs-minor
;;------------------------------------------------------------------------------
(add-hook 'prog-mode-hook 'hs-minor-mode)


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
;; C/C++ and Java
;;------------------------------------------------------------------------------
(use-package cc-mode
  :init
  (use-package company-c-headers
    :ensure t
    ;; :config
    ;; (add-to-list 'company-c-headers-path-system "/usr/include/c++/5/")
    )

  (add-to-list 'company-backends 'company-gtags)
  (add-to-list 'company-backends 'company-c-headers)
  (setq company-backends (delete 'company-semantic company-backends))

  (use-package c-eldoc
    :ensure t
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
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
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
                             (setq flycheck-gcc-language-standard "c++11"))))


;;------------------------------------------------------------------------------
;; JDEE
;;------------------------------------------------------------------------------
(use-package jdee
  :mode ("\\.java\\'" . jdee-mode)
  :init
  (add-hook 'jdee-mode-hook (lambda ()  (c-set-style "java")))
  (autoload 'jdee-mode "jdee" "jdee mode" t)
  :config
  (setq jdee-server-dir (expand-file-name "java" user-emacs-directory)))


;;------------------------------------------------------------------------------
;; GDB & GUD
;;------------------------------------------------------------------------------
(use-package gdb-mi
  :commands (gdb)
  :init
  (setq gdb-many-windows nil
        gdb-show-main t))

(use-package gud
  :commands (gdb)
  :bind ("C-x C-a C-g" . gud-run)
  :init (add-hook 'gud-mode-hook #'gud-tooltip-mode))


;;------------------------------------------------------------------------------
;; Paredit
;;------------------------------------------------------------------------------
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (use-package paredit-everywhere
    :ensure t))


;;------------------------------------------------------------------------------
;; Emacs Lisp
;;------------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq indent-tabs-mode nil)
                                  (turn-on-eldoc-mode)
                                  (eldoc-add-command
                                   'paredit-backward-delete
                                   'paredit-close-round)
                                  (paredit-mode t)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode t)))
(add-hook 'ielm-mode-hook (lambda () (paredit-mode t)))


;;------------------------------------------------------------------------------
;; Common Lisp & SLIME
;;------------------------------------------------------------------------------
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


;;------------------------------------------------------------------------------
;; Clojure & CIDER
;;------------------------------------------------------------------------------
(use-package clojure-mode
  :ensure t
  :init
  (use-package flycheck-clojure
    :ensure t
    :config (flycheck-clojure-setup))
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (setq indent-tabs-mode nil)

                                 ;; from Emacs and Clojure, a Lispy Love Affair
                                 (setq clojure-indent-style :always-align)
                                 (put-clojure-indent 'symbol 2)
                                 (put-clojure-indent 'GET 2)
                                 (define-clojure-indent
                                   (-> 1)
                                   (letfn '(1 ((:defn)) nil))
                                   (defrecord '(2 :form :form (1))))

                                 (paredit-mode t)
                                 (subword-mode t))))

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook (lambda ()
                                    (paredit-mode t)
                                    (subword-mode t))))


;;------------------------------------------------------------------------------
;; scheme & geiser
;;------------------------------------------------------------------------------
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


;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-backend "jedi")
  (elpy-use-cpython (or (executable-find "python3")
                        (executable-find "/usr/bin/python3")
                        (executable-find "/usr/local/bin/python3")
                        "python3"))
  (elpy-use-ipython)
  (setq python-shell-interpreter-args "--simple-prompt -i")
  (add-hook 'python-mode-hook (lambda ()
                              (setq indent-tabs-mode nil)
                              ;; (helm-gtags-mode 1)
                              )))

(use-package ein
  :ensure t)


;;------------------------------------------------------------------------------
;; HTML
;;------------------------------------------------------------------------------
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.php\\'" . web-mode)))


;;------------------------------------------------------------------------------
;; Perl
;;------------------------------------------------------------------------------
(add-hook 'cperl-mode-hook (lambda () (helm-gtags-mode 1)))

(use-package cperl-mode
  :config (defalias 'perl-mode 'cperl-mode))


;;------------------------------------------------------------------------------
;; Erlang
;;------------------------------------------------------------------------------
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


;;------------------------------------------------------------------------------
;; Elixir
;;------------------------------------------------------------------------------
(use-package elixir-mode
  :ensure t)


;;------------------------------------------------------------------------------
;; Lua
;;------------------------------------------------------------------------------
(add-hook 'lua-mode-hook (lambda () (helm-gtags-mode 1)))

(use-package lua-mode
  :ensure t)


;;------------------------------------------------------------------------------
;; JavaScript and JSON
;;------------------------------------------------------------------------------
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


;;------------------------------------------------------------------------------
;; nxml
;;------------------------------------------------------------------------------
(use-package nxml-mode
  :mode (("\\.xml$" . nxml-mode)
         ("\\.mpd$" . nxml-mode)
         ("\\.isml?$" . nxml-mode)
         ("\\.smil$" . nxml-mode))
  :init (add-to-list 'magic-mode-alist '("<\\?xml " . nxml-mode))
  :config
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


;;------------------------------------------------------------------------------
;; OpenCL
;;------------------------------------------------------------------------------
(use-package opencl-mode
  :ensure t
  :mode (("\\.cl$" . opencl-mode)))


;;------------------------------------------------------------------------------
;; YAML
;;------------------------------------------------------------------------------
(use-package yaml-mode
  :ensure t)


;;------------------------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :config (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))


;;------------------------------------------------------------------------------
;; nginx
;;------------------------------------------------------------------------------
(use-package nginx-mode
  :ensure t
  :mode ("nginx.conf$" "/etc/nginx/.*"))


;;------------------------------------------------------------------------------
;; sr-speedbar
;;------------------------------------------------------------------------------
(use-package sr-speedbar
  :init
  (setq sr-speedbar-auto-refresh nil)
  ;; (setq speedbar-show-unknown-files t)
  ;; (setq speedbar-use-images nil)
  (setq sr-speedbar-right-side nil))



;;----------------------------------------------------------------------------
;; Miscellaneous
;;----------------------------------------------------------------------------
(setq enable-local-variables :safe)
(add-to-list 'safe-local-variable-values
             '(eval add-to-list 'c-offsets-alist '(case-label . +)))
(add-to-list 'safe-local-variable-values
             '(eval (add-to-list 'c-offsets-alist '(case-label . +))))
(add-to-list 'safe-local-variable-values '(nxml-child-indent . 4))
(add-to-list 'safe-local-variable-values '(nxml-attribute-indent . 4))


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

;;; init.el -- My personal emacs settings

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(defconst *is-a-mac* (eq system-type 'darwin))


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

;; ignore the signature checks
;; (setq package-check-signature nil)


;;----------------------------------------------------------------------------
;; macOS key bindings
;;----------------------------------------------------------------------------
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))


;;----------------------------------------------------------------------------
;; Load common packages
;;----------------------------------------------------------------------------
(require-package 'diminish)


;;----------------------------------------------------------------------------
;; Buffers, Files, Directories
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)

(recentf-mode 1)
(setq recentf-max-saved-items 1000)
(setq recentf-exclude '("/tmp/" "/ssh:"))
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

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
(global-undo-tree-mode t)
(diminish 'undo-tree-mode)


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
    (when (or (member "나눔고딕코딩" (font-family-list))
              (member "NanumGothicCoding" (font-family-list))) 
      (set-fontset-font fontset 'hangul
                        '("NanumGothicCoding" . "unicode-bmp")))))

(require-package 'color-theme-sanityinc-solarized)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(show-paren-mode 1)

(global-set-key (kbd "M-i") 'imenu)

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "Byungwan Jun")


;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
(require-package 'magit)


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


;;------------------------------------------------------------------------------
;; helm
;;------------------------------------------------------------------------------

(require-package 'helm)

(require 'helm-config)

(autoload 'helm-c-yas-complete "helm-c-yasnippet" nil t)
(global-set-key (kbd "C-x C-o") 'ffap)

(require-package 'helm-ls-git)

(autoload 'helm-ls-git-ls "helm-ls-git" nil t)
(autoload 'helm-browse-project "helm-ls-git" nil t)

(require-package 'helm-gtags)

(eval-after-load 'helm-gtags
  '(progn
     (setq helm-gtags-ignore-case t
           helm-gtags-auto-update t
           helm-gtags-use-input-at-cursor t
           helm-gtags-pulse-at-cursor t
           helm-gtags-prefix-key "\C-cg"
           helm-gtags-suggested-key-mapping t)

     (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
     (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (helm-gtags-mode 1))))
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'dired-mode-hook 'helm-gtags-mode)


;;------------------------------------------------------------------------------
;; company
;;------------------------------------------------------------------------------

(require-package 'company)
(require-package 'company-statistics)
(require-package 'company-c-headers)

(add-hook 'prog-mode-hook 'global-company-mode)

(if (fboundp 'evil-declare-change-repeat)
    (mapc #'evil-declare-change-repeat
          '(company-complete-common
            company-select-next
            company-select-previous
            company-complete-selection
            company-complete-number)))

(eval-after-load 'company
  '(progn
     (require 'company-statistics)
     (company-statistics-mode)

     (add-to-list 'company-backends 'company-cmake)
     (add-to-list 'company-backends 'company-c-headers)
     (setq company-backends (delete 'company-semantic company-backends))))


;;------------------------------------------------------------------------------
;; yasnippet
;;------------------------------------------------------------------------------

(require-package 'yasnippet)
(require-package 'dropdown-list)

;; loading yasnippet will slow the startup
;; but it's necessary cost
(require 'yasnippet)

(yas-reload-all)

(yas-minor-mode 1)

(autoload 'snippet-mode "yasnippet" "")


;;------------------------------------------------------------------------------
;; gtags
;;------------------------------------------------------------------------------

(require-package 'ggtags)

(eval-after-load 'ggtags
  '(progn
     (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
     (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
     (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
     (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
     (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
     (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

     (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))
;; (add-hook 'eshell-mode-hook 'ggtags-mode)
;; (add-hook 'dired-mode-hook 'ggtags-mode)


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
(require-package 'cmake-mode)
(add-hook 'cmake-mode-hook 'global-company-mode)

(when (maybe-require-package 'c-eldoc)
  (package-install 'c-eldoc))

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
(require-package 'paredit)

(with-eval-after-load 'paredit
  (diminish 'paredit-mode " Par"))

(require-package 'paredit-everywhere)


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

(require-package 'slime)
(mapc #'(lambda (top-dir)
          (let* ((file-name (concat top-dir
                                    "quicklisp/slime-helper.el")))
            (when (file-exists-p file-name)
              (load file-name))))
      (list "/opt/" "~/"))
(setq inferior-lisp-program (or (executable-find "sbcl")
                                (executable-find "/usr/bin/sbcl")
                                (executable-find "/usr/local/bin/sbcl")
                                "sbcl"))
(require 'slime-autoloads)
;; (slime-setup)
(slime-setup '(slime-fancy))


;;------------------------------------------------------------------------------
;; scheme & geiser
;;------------------------------------------------------------------------------
(add-hook 'scheme-mode-hook (lambda () (setq indent-tabs-mode nil)))

(require-package 'geiser)
(setq geiser-active-implementations '(guile))
(setq geiser-guile-binary (or (executable-find "guile")
                              (executable-find "/usr/bin/guile")
                              (executable-find "/usr/local/bin/guile")
                              "guile"))


;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------
(require-package 'elpy)
(elpy-enable)
(elpy-use-ipython)
(setq python-shell-interpreter-args "--simple-prompt -i")

(require-package 'ein)


;;------------------------------------------------------------------------------
;; Perl
;;------------------------------------------------------------------------------
(defalias 'perl-mode 'cperl-mode)


;;------------------------------------------------------------------------------
;; Lua
;;------------------------------------------------------------------------------
(require-package 'lua-mode)


;;------------------------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------------------------
(when (maybe-require-package 'markdown-mode)
  (with-eval-after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))


;;------------------------------------------------------------------------------
;; sr-speedbar
;;------------------------------------------------------------------------------
(require-package 'sr-speedbar)

(setq sr-speedbar-auto-refresh nil)
;; (setq speedbar-show-unknown-files t) ; show all files
;; (setq speedbar-use-images nil) ; use text for buttons
(setq sr-speedbar-right-side nil) ; put on left side


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

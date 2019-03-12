;;; init --- my emacs configuration
;;; Commentary:
;;; Code:

;; GUI

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Settings

(savehist-mode 1)
(desktop-save-mode 1)
(recentf-mode 1)
(show-paren-mode 1)
(setq debug-on-error t)

(defun load-init-el ()
  "Reload init.el configuration."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Packages

(require 'color)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Theme

(use-package gotham-theme
  :ensure t
  :config
  (load-theme 'gotham t))

(global-hl-line-mode)

(set-face-attribute
 'default nil
 :family "DejaVu Sans Mono"
 :height 90
 :weight 'normal
 :width 'normal)

;; Core

(use-package avy
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0))

(use-package counsel
  :ensure t
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c f"   . counsel-git)       ; search for files in git repo
   ("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . counsel-ag)        ; search for regexp in git repo using ag
   ("C-c l"   . counsel-locate))   ; search for files or else using locate
  )

(defun counsel-ag-at-point ()
  "Ag symbol at point."
  (interactive)
  (counsel-ag (thing-at-point 'symbol)))

(use-package evil
  :ensure t
  :init
  (evil-mode))

(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-define-key
   "M-[" 'previous-buffer
   "M-]" 'next-buffer)
  (general-create-definer leader-def
   :states '(normal insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-c")
  (general-create-definer code-def
   :states '(normal insert emacs visual)
   :prefix ","
   :non-normal-prefix "C-,")
  (leader-def
    "`" '(save-buffer :which-key "write")
    "SPC" '(avy-goto-word-or-subword-1 :which-key "avy")
    "ESC" '(load-init-el :which-key "reload")
    "TAB" '(other-window :which-key "other")
    "b" '(nil :which-key "buffer")
    "bb" '(counsel-switch-buffer :which-key "switch")
    "bd" '(kill-buffer :which-key "kill")
    "bj" '(next-buffer :which-key "next")
    "bk" '(previous-buffer :which-key "prev")
    "f" '(nil :which-key "file")
    "ff" '(counsel-find-file :which-key "find")
    "s" '(nil :which-key "symbol")
    "ss" '(counsel-ag-at-point :which-key "ag")
    "w" '(nil :which-key "window")
    "w1" '(delete-other-windows :which-key "max")
    "wd" '(delete-window :which-key "kill"))
  (code-def
    ";" 'comment-line
    "e" '(nil :which-key "eval")
    "eb" '(eval-buffer :which-key "buffer")
    "ee" '(eval-last-sexp :which-key "last-sexp")
    "ef" '(eval-defun :which-key "defun")
    "h" '(nil :which-key "help")
    "hm" 'man))

(use-package auto-highlight-symbol
  :ensure t
  :config
  (setq ahs-idle-interval 0)
  (set-face-attribute
   ahs-face nil
   :underline nil
   :background (color-lighten-name (face-attribute 'default :background) 10)
   :foreground nil)
  (set-face-attribute
   ahs-plugin-defalt-face nil
   :background (face-attribute 'default :background)
   :foreground nil
   :underline nil)
  (leader-def
    "se" '(ahs-edit-mode :which-key "edit")))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
              ("C-'" . ivy-avy)
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line)) ; C-' to ivy-avy
  :config
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-ignore-buffers '("\*.+\*"))
  (setq ivy-height 20)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style 'fancy))

(use-package helpful
  :ensure t
  :config
  (code-def
    "hh" 'helpful-at-point)
  (evil-define-key 'normal helpful-mode-map "q" 'delete-window))

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (leader-def
    "p" '(projectile-command-map :which-key "project"))
  (projectile-mode +1))

(use-package smart-mode-line
  :ensure t
  :config
  (smart-mode-line-enable))

(use-package swiper
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "*")
    (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'symbol)))))
  (define-key evil-normal-state-map (kbd "#")
    (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'word))))))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.05)
  :diminish which-key-mode)

;; Git

(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (leader-def
    "g" '(nil :which-key "git")
    "gs" '(magit-status :which-key "git status")))

(use-package evil-magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :hook ((magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode))

;; LISP

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . eldoc-mode)))

(use-package cider
  :ensure t
  :hook ((cider-repl-mode . eldoc-mode))
  :config
  (setq cider-prompt-for-symbol nil)
  (leader-def clojure-mode-map
    "c" '(nil :which-key "cider")
    "ci" '(cider-jack-in-cljs :which-key "jack-in-cljs")
    "cj" '(cider-jack-in :which-key "jack-in")
    "cq" '(cider-quit :which-key "quit"))
  (code-def clojure-mode-map
    "eb" '(cider-eval-buffer :which-key "buffer")
    "ee" '(cider-eval-sexp-at-point :which-key "sexp-at-point")
    "ef" '(cider-eval-defun-at-point :which-key "defun-at-point")
    "hh" '(cider-doc :which-key "cider-doc"))
  (general-define-key
    "C-l" '(cider-repl-clear-buffer :which-key "clear REPL buffer")))

(use-package eval-sexp-fu
  :ensure t)

(use-package cider-eval-sexp-fu
  :ensure t)

(use-package clj-refactor
  :ensure t
  :hook ((clojure-mode . clj-refactor-mode)
	 (clojure-mode . yas-minor-mode))
  :config
  (code-def clojure-mode-map
    "r" '(nil :which-key "refactor"))
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
          (fn (cadr details)))
      (code-def clojure-mode-map key fn))))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
	 (clojure-mode . paredit-mode)
	 (cider-repl-mode . paredit-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)))

;; Rest

(use-package tex
  :ensure auctex)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(provide 'init)

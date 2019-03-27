;;; Init --- my emacs configuration
;;; Commentary:
;;; Code:

;; GUI


(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(defun skrat/disable-scroll-bars (frame)
  "Disable scroll bars on new FRAME."
  (modify-frame-parameters
   frame '((vertical-scroll-bars . nil)
	   (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'skrat/disable-scroll-bars)

;; Settings


(savehist-mode 1)
(desktop-save-mode 1)
(recentf-mode 1)
(show-paren-mode 1)
(setq debug-on-error t)
(setq vc-follow-symlinks t)
(setq mouse-autoselect-window t)

(defun skrat/load-init-el ()
  "Reload init.el configuration."
  (interactive)
  (load-file "~/.emacs"))

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)

(setq custom-file "~/.emacs.d/custom.el")
(shell-command (format "touch %s" custom-file))
(load custom-file)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Packages


(require 'color)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

(setq use-package-ensure-function 'quelpa)

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

(defun skrat/darken (face attribute pct)
  "Darker color of the FACE ATTRIBUTE by PCT."
  (color-darken-name (face-attribute face attribute) pct))

(defun skrat/lighten (face attribute pct)
  "Lighter color of the FACE ATTRIBUTE by PCT."
  (color-lighten-name (face-attribute face attribute) pct))

;; Core


(defun skrat/toggle-comment (beg end)
  "Comment or uncomment thing BEG END."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if (and beg end)
      (comment-or-uncomment-region beg end)
    (comment-line 1)))

(use-package avy
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (global-company-mode 1))

(defun skrat/counsel-ag-at-point ()
  "Ag symbol at point."
  (interactive)
  (counsel-ag (thing-at-point 'symbol)))

(use-package counsel
  :ensure t
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c f"   . counsel-git)       ; search for files in git repo
   ("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . skrat/counsel-ag-at-point) ; search for regexp in git repo using ag
   ("C-c l"   . counsel-locate))   ; search for files or else using locate
  )

(use-package dash
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode))

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

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
    "`"   '(save-buffer :which-key "write")
    "SPC" '(avy-goto-word-or-subword-1 :which-key "avy")
    "ESC" 'ivy-resume
    "TAB" '(other-window :which-key "other")
    "RET" 'make-frame
    "b"  '(nil :which-key "buffer")
    "bb" '(counsel-switch-buffer :which-key "switch")
    "bd" '(kill-this-buffer :which-key "kill")
    "bj" '(next-buffer :which-key "next")
    "bk" '(previous-buffer :which-key "prev")
    "f"  '(nil :which-key "file")
    "ff" '(counsel-find-file :which-key "find")
    "r"  '(nil :which-key "refactor")
    "s"  '(nil :which-key "symbol")
    "sj" '(counsel-semantic-or-imenu :which-key "jump")
    "t"  '(nil :which-key "toggle")
    "t SPC" '(whitespace-mode :which-key "whitespace")
    "w"  '(nil :which-key "window")
    "w1" '(delete-other-windows :which-key "max")
    "wd" '(delete-window :which-key "kill"))
  (code-def
    ";"  '(skrat/toggle-comment :which-key "comment")
    "e"  '(nil :which-key "eval")
    "eb" '(eval-buffer :which-key "buffer")
    "ee" '(eval-last-sexp :which-key "last-sexp")
    "er" '(eval-region :which-key "region")
    "ef" '(eval-defun :which-key "defun")
    "h"  '(nil :which-key "help")
    "hm" 'man
    "s"  '(nil :which-key "symbol")))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-color (skrat/lighten 'default :background 10))
  (leader-def
    "tc" '(fci-mode :which-key "fci")))

(use-package evil-anzu
  :ensure t
  :config
  (global-anzu-mode +1)
  (code-def
    "se" '(anzu-query-replace-at-cursor-thing :which-key "edit")))

(use-package focus
  :ensure t
  :config
  (general-define-key
   :modes '(normal)
   "<tab>" 'focus-next-thing
   "<backtab>" 'focus-prev-thing)
  (leader-def
    "tf" 'focus-mode))

(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0)
  (set-face-attribute
   'highlight-symbol-face nil :background (skrat/lighten 'default :background 10))
  (leader-def
    "th" '(highlight-symbol-mode :which-key "highlight"))
  :hook
  ((prog-mode-hook . highlight-symbol-mode)))

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
  (setq ivy-format-function 'ivy-format-function-arrow)
  (setq ivy-display-style 'fancy))

(use-package ivy-hydra
  :ensure t)

(use-package smex
  :ensure t)

(use-package helpful
  :ensure t
  :config
  (code-def
    "hh" 'helpful-at-point)
  (evil-define-key 'normal helpful-mode-map "q" 'delete-window))

(use-package form-feed
  :ensure t
  :config
  (form-feed-mode +1))

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (leader-def
    "p" '(projectile-command-map :which-key "project"))
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1)
  (leader-def
    "ss" '(counsel-projectile-ag :which-key "ag")))

(use-package smart-mode-line
  :ensure t
  :config
  (let ((modes '("Fly.*" "Projectile.*" "PgLn" "ivy" "Anzu" "WK"
		 "Paredit" "ARev" "EG" "Undo-Tree" "company" "hl-s"
		 "yas" "^L")))
    (setq rm-blacklist
	  (-> (mapconcat #'identity modes "\\|")
	      (format "^ \\(%s\\)$"))))
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

(use-package frame-fns
  :ensure t
  :quelpa (frame-fns :fetcher github :repo "emacsmirror/frame-fns"))

(use-package frame-cmds
  :ensure t
  :quelpa (frame-cmds :fetcher github :repo "emacsmirror/frame-cmds"))

(use-package zoom-frm
  :ensure t
  :quelpa (zoom-frm :fetcher github :repo "emacsmirror/zoom-frm")
  :config
  (general-define-key
   "C-=" 'zoom-frm-in
   "C--" 'zoom-frm-out))

;; Git


(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (leader-def
    "g" '(nil :which-key "git")
    "gs" '(magit-status :which-key "status")
    "gb" '(magit-blame :which-key "blame")))

(use-package evil-magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :hook
  ((magit-post-refresh-hook . diff-hl-magit-post-refresh)
   (magit-refresh-buffer-hook . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (set-face-attribute 'diff-hl-change nil :foreground "#edb443")
  (set-face-attribute 'diff-hl-change nil :background (skrat/darken 'diff-hl-change :foreground 50))
  (set-face-attribute 'diff-hl-insert nil :background (skrat/darken 'diff-hl-insert :foreground 30))
  (set-face-attribute 'diff-hl-delete nil :background (skrat/darken 'diff-hl-delete :foreground 30))
  (general-define-key
   "M-n" 'diff-hl-next-hunk
   "M-m" 'diff-hl-previous-hunk)
  (leader-def
    "gr" '(diff-hl-revert-hunk :which-key "revert-hunk")))

(use-package evil-goggles
  :ensure t
  :config
  (setq evil-goggles-blocking-duration 0.05)
  (set-face-attribute 'evil-goggles-delete-face nil :background (color-darken-name "red3" 30))
  (set-face-attribute 'evil-goggles-paste-face nil :background (color-darken-name "#edb443" 50)))

;; LISP


(use-package clojure-mode
  :ensure t
  :hook
  ((clojure-mode . eldoc-mode)))

(defun skrat/cider-debug-toggle-insert-state ()
  "Enter insert mode whenever in CIDER debugger to fix key bindings for evil."
  (if cider--debug-mode    ;; Checks if you're entering the debugger
      (evil-insert-state)  ;; If so, turn on evil-insert-state
    (evil-normal-state)))  ;; Otherwise, turn on normal-state

(use-package cider
  :ensure t
  :hook
  ((cider-repl-mode . eldoc-mode)
   (cider-debug-mode . skrat/cider-debug-toggle-insert-state))
  :config
  (setq cider-prompt-for-symbol nil)
  (setq cider-save-file-on-load nil)
  (leader-def clojure-mode-map
    "c" '(nil :which-key "cider")
    "ce" '(cider-enlighten-mode :which-key "enlighten")
    "ci" '(cider-jack-in-cljs :which-key "jack-in-cljs")
    "cj" '(cider-jack-in :which-key "jack-in")
    "cn" '(cider-repl-set-ns :which-key "repl-set-ns")
    "cr" '(cider-ns-refresh :which-key "ns-refresh")
    "cq" '(cider-quit :which-key "quit"))
  (code-def clojure-mode-map
    "eb" '(cider-eval-buffer :which-key "buffer")
    "ed" '(cider-debug-defun-at-point :which-key "debug-defun")
    "ee" '(cider-eval-sexp-at-point :which-key "sexp-at-point")
    "ef" '(cider-eval-defun-at-point :which-key "defun-at-point")
    "ep" '(cider-pprint-eval-defun-at-point :which-key "pp-defun-at-point")
    "er" '(cider-eval-region :which-key "region")
    "hh" '(cider-doc :which-key "doc")
    "hj" '(cider-javadoc :which-key "javadoc"))
  (general-define-key
   :keymap clojure-mode-map
   "M-RET" 'cider-eval-defun-at-point
   [(meta shift return)] 'cider-pprint-eval-defun-at-point)
  (general-define-key
   :keymap cider-repl-mode-map
   "C-l" '(cider-repl-clear-buffer :which-key "clear REPL buffer")))

(use-package eval-sexp-fu
  :ensure t)

;; (use-package cider-eval-sexp-fu
;;   :ensure t)

(use-package clj-refactor
  :ensure t
  :hook ((clojure-mode . clj-refactor-mode)
	 (clojure-mode . yas-minor-mode))
  :config
  (code-def clojure-mode-map
    "rt" '(nil :which-key "thread")
    "rtf" '(clojure-thread-first-all :which-key "first-all")
    "rtl" '(clojure-thread-last-all :which-key "first-all"))
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
          (fn (cadr details)))
      (code-def clojure-mode-map key fn))))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
	 (clojure-mode . paredit-mode)
	 (cider-repl-mode . paredit-mode)
	 (eshell-mode . paredit-mode)
	 (eval-expression-minibuffer-setup-hook . paredit-mode)
	 (eval-expression-minibuffer-setup-hook . eldoc-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)))

;; Rest


(defun skrat/gradle-installDebug ()
  "Android: Run gradle installDebug."
  (interactive)
  (gradle-execute "installDebug"))

(use-package gradle-mode
  :ensure t
  :config
  (code-def
    "bi" '(skrat/gradle-installDebug :which-key "gradle-installDebug")))

(use-package kotlin-mode
  :ensure t)

(use-package tex
  :ensure auctex)

(use-package latex-preview-pane
  :ensure t
  :config
  (customize-set-variable 'latex-preview-pane-use-frame t))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;(before-save . tide-format-before-save)
	 ))

(use-package csharp-mode
  :ensure t)

(use-package omnisharp
  :ensure t)

(provide 'init)

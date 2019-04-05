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

;; Backups and autosaves in one location

(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

;; Customs stuff

(setq custom-file "~/.emacs.d/custom.el")
(shell-command (format "touch %s" custom-file))
(load custom-file)

;; Whitespace nuking

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Packages


(require 'color)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;; (package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package quelpa
  :ensure t
  :init
  (setq quelpa-self-upgrade-p nil))

(use-package quelpa-use-package :ensure t)

;; Theme


(use-package gotham-theme
  :ensure t
  :config
  (load-theme 'gotham t))

(global-hl-line-mode)

(set-face-attribute
 'default  nil
 :family  "DejaVu Sans Mono"
 :height   90
 :weight  'normal
 :width   'normal)

(defun skrat/darken (face attribute pct)
  "Darker color of the FACE ATTRIBUTE by PCT."
  (color-darken-name (face-attribute face attribute) pct))

(defun skrat/lighten (face attribute pct)
  "Lighter color of the FACE ATTRIBUTE by PCT."
  (color-lighten-name (face-attribute face attribute) pct))

;; Functions


(defun skrat/counsel-ag-at-point ()
  "Ag symbol at point."
  (interactive)
  (counsel-ag (thing-at-point 'symbol)))

(defun skrat/gradle-installDebug ()
  "Android: Run gradle installDebug."
  (interactive)
  (gradle-execute "installDebug"))

(defun skrat/tide-if-tsx ()
  "Tide mode if filename ends with .tsx."
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (tide-setup)
    (eldoc-mode)))

(defun skrat/toggle-comment (beg end)
  "Comment or uncomment thing BEG END."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if (and beg end)
      (comment-or-uncomment-region beg end)
    (comment-line 1)))

(defun skrat/turn-on-highlight-symbol-mode ()
  "Turn on highlight-symbol-mode."
  (highlight-symbol-mode +1))

;; Core


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

(use-package smart-mode-line
  :ensure t
  :config
  (let ((modes '("Fly.*" "Projectile.*" "PgLn" "ivy" "Anzu" "WK"
		 "Paredit" "ARev" "EG" "Undo-Tree" "company" "hl-s"
		 "yas" "^L" "counsel")))
    (setq rm-blacklist
	  (-> (mapconcat #'identity modes "\\|")
	      (format "^ \\(%s\\)$"))))
  (smart-mode-line-enable))

(use-package diminish
  :ensure t)

(use-package general
  :ensure t
  :bind*
  (("M-[" . previous-buffer)
   ("M-]" . next-buffer))
  :config
  (general-evil-setup t)
  (general-create-definer leader-def
   :states '(normal insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-c")
  (general-create-definer local-def
   :states '(normal insert emacs visual)
   :prefix ","
   :non-normal-prefix "C-,")
  (leader-def
    "`"   '(save-buffer :which-key "write")
    "ESC" 'ivy-resume
    "TAB" '(other-window :which-key "other")
    "RET" 'make-frame
    "b"  '(nil :which-key "buffer")
    "bd" '(kill-this-buffer :which-key "kill")
    "bj" '(next-buffer :which-key "next")
    "bk" '(previous-buffer :which-key "prev")
    "f"  '(nil :which-key "file")
    "r"  '(nil :which-key "refactor")
    "s"  '(nil :which-key "symbol")
    "t"  '(nil :which-key "toggle")
    "t SPC" '(whitespace-mode :which-key "whitespace")
    "w"  '(nil :which-key "window")
    "w1" '(delete-other-windows :which-key "max")
    "wd" '(delete-window :which-key "kill"))
  (local-def
    ";"  '(skrat/toggle-comment :which-key "comment")
    "e"  '(nil :which-key "eval")
    "eb" '(eval-buffer :which-key "buffer")
    "ee" '(eval-last-sexp :which-key "last-sexp")
    "er" '(eval-region :which-key "region")
    "ef" '(eval-defun :which-key "defun")
    "h"  '(nil :which-key "help")
    "hm" 'man
    "s"  '(nil :which-key "symbol")))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
	      ("C-'" . ivy-avy)
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line)) ; C-' to ivy-avy
  :custom
  (ivy-use-virtual-buffers nil)
  (ivy-ignore-buffers '("\*.+\*"))
  (ivy-height 20)
  (ivy-count-format "(%d/%d) ")
  (ivy-format-function 'ivy-format-function-arrow)
  (ivy-display-style 'fancy))

(use-package ivy-hydra
  :ensure t)

(use-package smex
  ;; this enabled recent entries in ivy
  :ensure t)

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
  :general
  (leader-def
    "bb" '(counsel-switch-buffer :which-key "switch")
    "ff" '(counsel-find-file :which-key "find")
    "sj" '(counsel-semantic-or-imenu :which-key "jump"))
  :config
  (counsel-mode +1))

(use-package avy
  :general
  (leader-def
    "SPC" '(avy-goto-word-or-subword-1 :which-key "avy")))

(use-package swiper
  :general
  (:states '(normal)
   (kbd "*") (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'symbol))))
   (kbd "#") (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'word))))))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.05)
  :diminish which-key-mode)

(use-package company
  :custom
  (company-idle-delay 0)
  :config
  (global-company-mode 1))

(use-package flycheck
  :config
  (global-flycheck-mode +1))

(use-package fill-column-indicator
  :config
  (setq fci-rule-color (skrat/lighten 'default :background 10))
  (leader-def
    "tc" '(fci-mode :which-key "fci")))

(use-package evil-anzu
  :general
  (local-def
   "se" '(anzu-query-replace-at-cursor-thing :which-key "edit"))
  :config
  (global-anzu-mode +1))

(use-package focus
  :general
  (:modes '(normal)
   "<tab>" 'focus-next-thing
   "<backtab>" 'focus-prev-thing)
  (leader-def
    "tf" 'focus-mode))

(use-package highlight-symbol
  :general
  (leader-def
    "th" '(highlight-symbol-mode :which-key "highlight"))
  :custom
  (highlight-symbol-idle-delay 0)
  :config
  (set-face-attribute
   'highlight-symbol-face nil :background (skrat/lighten 'default :background 10))
  :hook
  ((prog-mode-hook . skrat/turn-on-highlight-symbol-mode)))

(use-package helpful
  :general
  (local-def
   "hh" 'helpful-at-point)
  :config
  (general-def
   :states '(normal)
   :keymap helpful-mode-map
   "q" 'delete-window))

(use-package form-feed
  :diminish form-feed-mode
  :custom-face
  (form-feed-line ((t (:strike-through "#091F2E"))))
  :hook
  ((emacs-lisp-mode-hook . form-feed-mode)))

(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-generic-command "fd . -0")
  :general
  (leader-def
    "p" '(projectile-command-map :which-key "project"))
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :after (projectile)
  :general
  (leader-def
    "ss" '(counsel-projectile-ag :which-key "ag"))
  :config
  (counsel-projectile-mode 1))

(use-package frame-fns
  :quelpa (frame-fns :fetcher github :repo "emacsmirror/frame-fns"))

(use-package frame-cmds
  :quelpa (frame-cmds :fetcher github :repo "emacsmirror/frame-cmds"))

(use-package zoom-frm
  :after (frame-fns frame-cmds)
  :quelpa (zoom-frm :fetcher github :repo "emacsmirror/zoom-frm")
  :bind*
  (("C-=" . 'zoom-frm-in)
   ("C--" . 'zoom-frm-out)))

;; Git


(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :general
  (leader-def
    "g" '(nil :which-key "git")
    "gs" '(magit-status :which-key "status")
    "gb" '(magit-blame :which-key "blame")))

(use-package evil-magit
  :after (magit))

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
  :general
  ("M-n" 'diff-hl-next-hunk
   "M-m" 'diff-hl-previous-hunk)
  (leader-def
    "gr" '(diff-hl-revert-hunk :which-key "revert-hunk")))

(use-package evil-goggles
  :after (evil)
  :custom
  (evil-goggles-blocking-duration 0.05)
  :config
  (set-face-attribute 'evil-goggles-delete-face nil :background (color-darken-name "red3" 30))
  (set-face-attribute 'evil-goggles-paste-face nil :background (color-darken-name "#edb443" 50)))

;; LISP


(use-package clojure-mode
  :mode "\\.\\(clj\\|cljs\\|dtm\\|edn\\)\\'"
  :hook
  ((clojure-mode-hook . eldoc-mode)))

(use-package cider
  :after (clojure-mode)
  :hook
  ((cider-repl-mode-hook . eldoc-mode))
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
  (local-def clojure-mode-map
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
  :hook ((clojure-mode-hook . clj-refactor-mode)
	 (clojure-mode-hook . yas-minor-mode))
  :general
  (local-def clojure-mode-map
    "rt" '(nil :which-key "thread")
    "rtf" '(clojure-thread-first-all :which-key "first-all")
    "rtl" '(clojure-thread-last-all :which-key "first-all")))

(use-package paredit
  :hook
  ((emacs-lisp-mode-hook . paredit-mode)
   (clojure-mode-hook    . paredit-mode)
   (cider-repl-mode-hook . paredit-mode)
   (eshell-mode          . paredit-mode)
   (eval-expression-minibuffer-setup-hook . paredit-mode)
   (eval-expression-minibuffer-setup-hook . eldoc-mode)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode-hook . rainbow-delimiters-mode)
	 (clojure-mode-hook    . rainbow-delimiters-mode)))

;; Rest


(use-package gradle-mode
  :general
  (local-def
    "bi" '(skrat/gradle-installDebug :which-key "gradle-installDebug")))

(use-package kotlin-mode
  :mode "\\.kt\\'")

(use-package tex
  :mode "\\.tex\\'"
  :ensure auctex)

(use-package latex-preview-pane
  :after (tex)
  :custom
  (latex-preview-pane-use-frame t))

(use-package web-mode
  :mode "\\.\\(html\\|tsx\\)\\'")

(use-package tide
  :mode "\\.\\(ts\\|tsx\\)\\'"
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode-hook . tide-setup)
         (typescript-mode-hook . tide-hl-identifier-mode)
	 (typescript-mode-hook . eldoc-mode)
	 (web-mode-hook        . skrat/tide-if-tsx)
         ;(before-save . tide-format-before-save)
	 )
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package omnisharp
  :mode "\\.cs\\'")

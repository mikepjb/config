(defconst *is-a-mac* (eq system-type 'darwin))

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(menu-bar-mode -1)
(tool-bar-mode -1)
(if (display-graphic-p)
    (scroll-bar-mode -1))

(setq inhibit-startup-screen t)

(load-theme 'tailstone t)

;; N.B make sure to install the seperate_statics font as I can't find a way to work with variable fonts in Emacs.
(when (x-list-fonts "Recursive Mono Linear Static")
  (set-face-attribute 'default nil :font "Recursive Mono Linear Static" :height 120 :weight 'normal)
  ;; TODO remove? (set-frame-font "Recursive Mono Linear Static" nil t)
)

(electric-pair-mode 1)
(fido-mode 1)
(tab-bar-mode 1)

(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-new-tab-to 'rightmost
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil)
(setq-default indent-tabs-mode nil)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(dolist (mode '(org-mode-hook
		term-mode-hook
		help-mode-hook
		helpful-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq custom-file (concat user-emacs-directory "custom.el"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(require 'package)
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(require 'use-package)
(setq use-package-always-ensure t)


;; TODO fn to show XXX/TODOs in current project please!
;; the tabs workflow for a tmux like setup..? needed?

;; (use-package evil
;;   :config (progn
;; 	    (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
;; 	    (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
;; 	    (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
;; 	    ;; (define-key evil-normal-state-map (kbd "cpp") 'eval-last-sexp)
;; 	    ;; (evil-mode 1)
;; ))


(use-package paredit)
(use-package clojure-mode)
(use-package cider)
(use-package markdown-mode)
(use-package diminish)
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src"))
    (setq projectile-switch-project-action #'projectile-dired)))
(use-package magit)
(use-package company :config (global-company-mode))

;; counsel?
;; ivy? helm?
;; helpful?
;; rainbow-delimiters?
;; which-key? gonna be important for discovering emacs/plugins on the fly.
;; counsel-projectile?
;; magit-todos
;; git-gutter
;; org-mode? (there's a lot of config in your old config)
;; org-bullets
;; markdown-mode
;; visual-fill-column
;; typescript-mode
;; lsp-mode (or potentailly eglot)

;; whitespace-mode? (in-built mode to display whitespace) & whitespace-cleanup?
;; flyspell-mode? on the fly spell checking (also flyspell-prog-mode for docstrings/strings only)
;; project.el? p270 allows running cmds based on project root (based on vcs files etc)
;; nov - epub reader for emacs
;; hydra?
;; dump-jump?
;; include code to use ripgrep where available

(defmacro ifn (fn)
  `(lambda () (interactive) ,fn))

(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word
    backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

(defun close-window-or-frame ()
  "Used to kill the current 'thing' focused on, which may be a
  lone frame or one of many windows."
  (interactive
   (if (eq 1 (length (window-list)))
       (tab-bar-close-tab)
     (delete-window))))

(define-prefix-command 'frame-map) ;; prefix for tmux-like actions

(dolist
    (binding
     `(("M-o" . other-window)
       ("C-c i" . ,(ifn (find-file user-init-file)))
       ("C-c n" . ,(ifn (find-file (concat user-emacs-directory "notes.org"))))
       ("C-c l" . ,(ifn (find-file "~/src")))
       ("C-c g" . magit)
       ("C-c p" . projectile-find-file)
       ("C-c P" . projectile-grep)
       ("C-h" . delete-backward-char)
       ("M-i" . imenu)
       ("M-j" . ,(ifn (join-line -1)))
       ("M-H" . ,help-map)
       ("M-/" . comment-dwim)
       ("C-;" . company-capf)
       ("M-k" . paredit-forward-barf-sexp)
       ("M-l" . paredit-forward-slurp-sexp)
       ("M-O" . tab-bar-switch-to-recent-tab)
       ("M-;" . ,frame-map)
       ("M-; o" . delete-other-windows)
       ("M-; =" . tab-new)
       ("M-; ]" . tab-rename)
       ("M-; v" . ,(ifn (progn (split-window-vertically) (other-window 1))))
       ("M-; s" . ,(ifn (progn (split-window-horizontally) (other-window 1))))
       ("M-; x" . close-window-or-frame)
       ("M-; 1" . ,(ifn (tab-bar-select-tab 1)))
       ("M-; 2" . ,(ifn (tab-bar-select-tab 2)))
       ("M-; 3" . ,(ifn (tab-bar-select-tab 3)))
       ("M-; 4" . ,(ifn (tab-bar-select-tab 4)))
       ("M-; 5" . ,(ifn (tab-bar-select-tab 5)))
       ("M-; 6" . ,(ifn (tab-bar-select-tab 6)))
       ("M-; 7" . ,(ifn (tab-bar-select-tab 7)))
       ("M-; 8" . ,(ifn (tab-bar-select-tab 8)))
       ("M-; 9" . ,(ifn (tab-bar-select-tab 9)))
       ("M-RET" . toggle-frame-fullscreen)
       ("M-F" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))

(setq display-fill-column-indicator-column 100)
(display-fill-column-indicator-mode 1)

(let ((local-config (concat user-emacs-directory "local.el")))
  (when (file-exists-p local-config)
    (load local-config)))

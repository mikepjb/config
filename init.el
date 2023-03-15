;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default system variables  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *is-a-mac* (eq system-type 'darwin))

(defconst *fixed-font*
  (cond ((x-list-fonts "Recursive Mono Linear Static") "Recursive Mono Linear Static")
	((x-list-fonts "Monaco") "Monaco") ;; defaul mac os
	((x-list-fonts "Menlo") "Menlo") ;; mac os with vertical line glyph
	((x-list-fonts "Monospace") "Monospace") ;; linux deja vu sans mono default
	(t nil)))

(defconst *variable-font*
  (cond ((x-list-fonts "Recursive") "Recursive")
	((x-list-fonts "Novaletra Serif CF") "Novaletra Serif CF")
	((x-list-fonts "Georgia") "Georgia")
	((x-list-fonts "Sans Serif") "Sans Serif")
	(t nil)))

(set-face-attribute 'default nil :font *fixed-font* :height 120)
(set-face-attribute 'fixed-pitch nil :font *fixed-font* :height 120)
;; TODO resets when you load the theme! think this is because we are
;; setting fill-column-indicator in both places
(set-face-attribute 'fill-column-indicator nil :font "Menlo" :height 120)
(setq-default display-fill-column-indicator-character 9474)
(set-face-attribute 'variable-pitch nil :font *variable-font* :height 120 :weight 'regular)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default system settings   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq-default
 display-fill-column-indicator-column 100)

(setq native-comp-deferred-compilation t
      native-comp-async-query-on-exit t
      native-comp-async-jobs-number 4
      native-comp-async-report-warnings-errors nil
      tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      fill-column 80
      ido-enable-flex-matching t
      inhibit-startup-screen t
      bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t
      custom-file (concat user-emacs-directory "custom.el")
      package-enable-at-startup nil
      ring-bell-function nil
      garbage-collection-messages t)

(when *is-a-mac*
  (setq mac-command-modifier 'meta
	mac-option-modifier 'none))

(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word
    backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

(when window-system
    (scroll-bar-mode -1)
    (set-frame-size (selected-frame) 200 80))

(defalias 'yes-or-no-p 'y-or-n-p)

(defmacro set-mode (mode value)
  `(funcall ,mode (if (eq ,value :enable) 1 -1)))

(dolist (mode '(electric-pair-mode
		fido-mode
		show-paren-mode
		column-number-mode
		global-auto-revert-mode
		savehist-mode))
  (set-mode mode :enable))

(dolist (mode '(menu-bar-mode
		tool-bar-mode
		electric-indent-mode)) ;; enabling this, disables indent for C-j
  (set-mode mode :disable))

(require 'local nil t) ;; optionally load a local.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package settings             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-files `(,(concat user-emacs-directory "org.org")
			 ,(concat user-emacs-directory "org"))
      org-agenda-start-on-weekday nil ; show the next 7 days
      org-agenda-start-day "0d"
      org-agenda-span 14
      org-tags-column 80
      org-agenda-category-filter-preset '("-shopping")
      org-deadline-warning-days 31
      org-agenda-start-with-log-mode t
      org-log-done 'time
      org-ellipsis " ▼"
      org-hide-emphasis-markers t
      org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")
      org-log-into-drawer t
      org-startup-with-inline-images t
      org-image-actual-width '(300)
      org-habit-graph-column 60
      org-agenda-todo-keyword-format "" ;; don't tell me the todo state in agenda view.
      org-refile-targets `((,(concat user-emacs-directory "org/archive.org") :maxlevel . 1)
			   (,(concat user-emacs-directory "org/index.org") :maxlevel . 1))
      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!) AXED(x)")
	(sequence "BACKLOG(b)" "PLAN(p)" "COMPLETED(c)" "|" "RELEASED(r)" "CANCELLED(k@)"))
      org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 31)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))
	  (todo "TODO"
		((org-agenda-overriding-header "All todos")))))
	("l" "Shopping List"
	 ((tags "SCHEDULED<\"<today>\""
		  ((org-agenda-overriding-header "Shopping List\n")
		   (org-agenda-prefix-format '((tags . " - ")))
		   (org-agenda-category-filter-preset '("+shopping"))))))
	("D" "Development"
	 ((agenda "" ((org-deadline-warning-days 31)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))
	  (todo "TODO"
		((org-agenda-overriding-header "All todos")))))))

(advice-add 'org-refile :after 'org-save-all-org-buffers)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-agenda-mode-hook 'variable-pitch-mode)

(require 'package)

(when (not (member "melpa" (mapcar 'car package-archives))) ;; check there isn't a local override
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/"))))

(setq config/internal-package-list
      '(;; (org . (add-hook 'org-mode-hook 'config/org-mode-setup))
	org-tempo
	(org-habit . (add-to-list 'org-modules 'org-habit))))

(setq config/external-package-list
      '((org-bullets . (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
	;; (olivetti . (add-hook 'org-mode-hook (lambda () (config/org-mode-olivetti))))
	clojure-mode
	racket-mode ;; run this too `raco pkg install --auto drracket'
	(company . (global-company-mode t)) ;; might be part of emacs?
	markdown-mode
	inf-clojure
	projectile
	projectile-ripgrep
	;;cider ;; inferior-lisp? inf-clojure? only if you can get cljs working
	paredit
	ledger-mode))

(defun config/load-packages (pkgs)
  (dolist (pkg pkgs)
    (if (listp pkg)
	(when (require (car pkg) nil t)
	  (eval (cdr pkg)))
      (require pkg nil t))))

(config/load-packages config/internal-package-list)
(config/load-packages config/external-package-list)

(defun config/update-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg config/external-package-list)
    (package-install (if (listp pkg) (car pkg) pkg))))

(defun code-config ()
  (display-line-numbers-mode 1)
  (display-fill-column-indicator-mode 1)
  (hl-line-mode 1))

(dolist (hook '(prog-mode-hook css-mode-hook)) (add-hook hook 'code-config))

(when (require 'paredit nil t)
  (dolist (hook '(emacs-lisp-mode-hook
		  eval-expression-minibuffer-setup-hook
		  ielm-mode-hook
		  lisp-mode-hook
		  lisp-interaction-mode-hook
		  clojure-mode-hook
		  scheme-mode-hook
		  racket-mode-hook))
    (add-hook hook #'enable-paredit-mode))

  (eval-after-load "paredit" (define-key paredit-mode-map (kbd "M-;") nil)))

(when (require 'inf-clojure nil t)
  (defun cljs-node-repl ()
    (interactive)
    (inf-clojure "clj -M -m cljs.main -co build.edn -re node -r")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user functions               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is probably something that magit makes quite easy and isn't
;; necessary.
(defun update-git-submodules ()
  (interactive)
  (async-shell-command "git submodule update --init"))

;; only intended for POSIX linux/mac os
(defun setup-env ()
  (interactive)
  ;; TODO .bash_profile? > . ~/.bashrc
  (shell-command (concat  "ln -s " user-emacs-directory "bashrc $HOME/.bashrc")))

(defun focus-mode ()
  (interactive)
  (setq olivetti-body-width 100)
  (if (bound-and-true-p focus-enabled)
      (progn
	(when (derived-mode-p 'prog-mode)
	  (display-line-numbers-mode 1)
	  (display-fill-column-indicator-mode 1))
	(olivetti-mode -1)
	(when (not (eq major-mode 'org-mode))
	  (variable-pitch-mode -1))
	(setq-local focus-enabled nil))
    (progn
      (when (derived-mode-p 'prog-mode)
	(display-line-numbers-mode -1)
	(display-fill-column-indicator-mode -1))
      (olivetti-mode 1)
      (variable-pitch-mode 1)
      (setq-local focus-enabled t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ifn (fn)
  `(lambda () (interactive) ,fn))

(defmacro ifn-from (from-dir fn)
  `(lambda () (interactive)
     (let ((default-directory ,from-dir)) (call-interactively ,fn))))

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
     `(
       ("M-o" . other-window)
       ("C-c i" . ,(ifn (find-file user-init-file)))
       ("C-c n" . ,(ifn (find-file (concat user-emacs-directory "notes.org"))))
       ("C-c o" . ,(ifn (find-file (concat user-emacs-directory "org.org"))))
       ("C-c O" . ,(ifn-from "~/.emacs.d/org/" 'find-file))
       ("C-c k" . ,(ifn-from "~/src/knowledge/src/" 'find-file))
       ("C-c P" . projectile-ripgrep)
       ("C-c f" . focus-mode)
       ("C-c a" . ,(ifn (org-agenda nil "d")))
       ("C-c A" . org-agenda)
       ("C-c l" . ,(ifn-from "~/src/" 'find-file))
       ("C-c g" . magit)
       ("C-c p" . projectile-find-file)
       ("C-;" . company-capf)
       ("M-k" . paredit-forward-barf-sexp)
       ("M-l" . paredit-forward-slurp-sexp)
       ("C-h" . delete-backward-char)
       ("M-s" . save-buffer) ;; this is easier but maybe C-x, C-s is just meant to be (it's hardwired into my fingers)
       ("C-M-s" . isearch-forward-symbol-at-point)
       ("M-i" . imenu)
       ("M-j" . ,(ifn (join-line -1)))
       ("M-H" . ,help-map)
       ("M-/" . comment-dwim)
       ("M-O" . tab-bar-switch-to-recent-tab)
       ("M-;" . ,frame-map)
       ("M-; o" . delete-other-windows)
       ("M-; =" . tab-new)
       ("M-; ]" . tab-rename)
       ("M-; v" . ,(ifn (progn (split-window-vertically) (other-window 1))))
       ("M-; s" . ,(ifn (progn (split-window-horizontally) (other-window 1))))
       ("M-; x" . close-window-or-frame)
       ("M-; q" . close-window-or-frame)
       ("M-; 1" . ,(ifn (tab-bar-select-tab 1)))
       ("M-; 2" . ,(ifn (tab-bar-select-tab 2)))
       ("M-; 3" . ,(ifn (tab-bar-select-tab 3)))
       ("M-; 4" . ,(ifn (tab-bar-select-tab 4)))
       ("M-; 5" . ,(ifn (tab-bar-select-tab 5)))
       ("M-; 6" . ,(ifn (tab-bar-select-tab 6)))
       ("M-; 7" . ,(ifn (tab-bar-select-tab 7)))
       ("M-; 8" . ,(ifn (tab-bar-select-tab 8)))
       ("M-; 9" . ,(ifn (tab-bar-select-tab 9)))
       ;; ("M-RET" . toggle-frame-fullscreen)
       ("M-F" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))

(load-theme 'hypalynx t)

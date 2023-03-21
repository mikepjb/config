;; use "describe-face" function to describe things that you want to theme.
;; teal/green actually => emerald/cyan
;; Develop/reload with:
;; (progn (eval-buffer) (hypalynx-theme-as :light) (load-theme 'hypalynx-light t))
;; (progn (eval-buffer) (hypalynx-theme-as :dark) (load-theme 'hypalynx-dark t))

(defcustom hypalynx-transparent-background nil
  "Make transparent background in terminal. (Workaround)")

(defconst *hypalynx/fill-column-font*
  (cond ((x-list-fonts "Menlo") "Menlo")
	((x-list-fonts "Monospace") "Monospace")
	(t nil)))

(defconst hypalynx-color-palette
  '(:neutral-50 "#fafafa"
		:neutral-200 "#e5e5e5"
		:neutral-300 "#d4d4d4"
		:neutral-500 "#737373"
		:neutral-700 "#404040"
		:neutral-800 "#262626"
		:neutral-900 "#171717"
		:stone-50 "#fafaf9"
		:stone-100 "#f5f5f4"
		:stone-200 "#e7e5e4"
		:stone-300 "#d6d3d1"
		:stone-400 "#a8a29e"
		:stone-500 "#78716c"
		:stone-600 "#57534e"
		:stone-700 "#44403c"
		:stone-800 "#292524"
		:stone-900 "#1c1917"
		:emerald-400 "#2dd4bf"
		:emerald-500 "#14b8a6"
		:emerald-600 "#0d9488"
		:emerald-700 "#047857"
		:emerald-800 "#115e59"
		:lime-200 "#d9f99d"
		:lime-300 "#bef264"
		:lime-400 "#a3e635"
		:lime-500 "#84cc16"
		:lime-600 "#65a30d"
		:orange-400 "#fb923c"
		:red-400 "#f87171"
		:blue-900 "#1e3a8a"
		:cyan-200 "#a5f3fc"
		:cyan-300 "#67e8f9"
		:cyan-400 "#22d3ee"
		:cyan-500 "#06b6d4"
		:cyan-700 "#0e7490"
		:cyan-800 "#075985"
		:cyan-light "#a5f3fc"
		:pink-400 "#f472b6"))

(defun hl-g (kw)
  (plist-get hypalynx-color-palette kw))

(defconst hypalynx-color-theme
  `(:dark (:background ,(hl-g :stone-900)
		       :foreground ,(hl-g :stone-50)
		       :background-tint ,(hl-g :stone-800)
		       :parens ,(hl-g :cyan-300)
		       :paren-match ,(hl-g :orange-400)
		       :core-fns ,(hl-g :cyan-500)
		       :keywords ,(hl-g :cyan-500)
		       :names ,(hl-g :emerald-500)
		       :string ,(hl-g :lime-300)
		       :comment ,(hl-g :stone-500)
		       :light-lime ,(hl-g :lime-200)
		       :lime ,(hl-g :lime-600)
		       :red ,(hl-g :red-400)
		       :pink ,(hl-g :pink-400)
		       :blue ,(hl-g :blue-900))
	  :light (:background ,(hl-g :stone-50)
			      :foreground ,(hl-g :stone-900)
			      :background-tint ,(hl-g :stone-200)
			      :parens ,(hl-g :cyan-700)
			      :paren-match ,(hl-g :orange-400)
			      :core-fns ,(hl-g :cyan-800)
			      :keywords ,(hl-g :cyan-700)
			      :names ,(hl-g :emerald-800)
			      :string ,(hl-g :lime-600)
			      :comment ,(hl-g :stone-500)
			      :light-lime ,(hl-g :lime-200)
			      :lime ,(hl-g :lime-600)
			      :red ,(hl-g :red-400)
			      :pink ,(hl-g :pink-400)
			      :blue ,(hl-g :blue-900))))

(defun hypalynx--color-theme-keys ()
  (seq-filter (apply-partially #'keywordp)
	      (plist-get hypalynx-color-theme :dark)))

(defmacro bind-theme-colors ()
  (mapcar (lambda (e)
	    `(,(intern (substring (symbol-name e) 1)) (plist-get colors ,e)))
	  (hypalynx--color-theme-keys)))

(defmacro hypalynx-define-with-colors (mode &rest body)
  "Execute `BODY' within the context of colors chosen by `MODE'."
  `(let* ((colors (plist-get hypalynx-color-theme ,mode))
	  (class '((class color) (min-colors 89)))
	  (background-mode (if (eq ,mode :dark) 'dark 'light))
	  (fci-font *hypalynx/fill-column-font*))
     (let ,(macroexp--expand-all '(bind-theme-colors))
       ,@body)))

(defmacro hypalynx-face-specs ()
  (quote
   (mapcar
    (lambda (entry)
      (list (car entry) `((,class ,@ (cdr entry)))))
    `(;; Standard font lock faces
      (default (:foreground ,foreground :background ,background))
      (bold (:weight bold))
      (bold-italic (:slant italic :weight bold))
      (underline (:underline t))
      (hl-line (:background ,background-tint))
      (show-paren-match (:foreground ,paren-match))
      (show-paren-mismatch (:foreground ,red))
      (font-lock-builtin-face (:foreground ,keywords :slant italic))
      (font-lock-comment-delimiter-face (:foreground ,comment :slant italic))
      (font-lock-comment-face (:foreground ,comment :slant italic))
      (font-lock-keyword-face (:foreground ,core-fns))
      (font-lock-variable-name-face (:foreground ,names))
      (font-lock-doc-face (:foreground ,pink))
      (font-lock-doc-string-face (:foreground ,pink))
      (font-lock-function-name-face (:foreground ,pink))
      ;; no italics originally
      (font-lock-constant-face (:foreground ,blue :slant italic))
      (font-lock-hypalynx-parens-face (:foreground ,red))
      (font-lock-negation-char-face (:foreground ,pink))
      (font-lock-preprocessor-face (:foreground ,pink))
      (font-lock-regexp-grouping-backslash (:foreground ,pink))
      (font-lock-regexp-grouping-construct (:foreground ,pink))
      (font-lock-string-face (:foreground ,string))
      (font-lock-type-face (:foreground ,pink))
      (font-lock-warning-face (:weight bold :foreground ,pink))
      (shadow (:foreground ,pink))
      (success (:foreground ,pink))
      (error (:foreground ,pink))
      (warning (:foreground ,pink))
      (tooltip (:foreground ,pink :background ,background :inverse-video t))

      (cursor (:background ,pink))
      (fringe (:background ,background-tint :foreground ,pink))
      ;; For whatever reason, family can be nil but font can't
      ;; be.. however they still accept the same font names and behave
      ;; the same otherwise.
      (fill-column-indicator (:foreground ,comment :family ,fci-font))
      (hypalynx-parens-face (:foreground ,parens))
      ))))

(defmacro hypalynx-theme-as (mode)
  "Apply the hypalynx theme in a scope with variables bound by the
given mode e.g `dark' or `light'"
  (let ((name (intern (concat "hypalynx-" (substring (symbol-name mode) 1))))
	(doc "A theme based on emerald & cyan colors with dark/light modes."))
    `(progn
       (deftheme ,name ,doc)
       (setq hypalynx-current-mode ,mode)
       (put ',name 'theme-immediate t)
       (hypalynx-define-with-colors
	',mode
	(apply 'custom-theme-set-faces ',name
	       (hypalynx-face-specs))
	(custom-theme-set-variables
	 ',name
	 `(frame-background-mode ',background-mode)
	 `(beacon-color ,red))))))

;; (let ((class '((class color) (min-colors 89)))
;;       (neutral-50 "#fafafa")
;;       (neutral-100 "#f5f5f5")
;;       (neutral-200 "#e5e5e5")
;;       (neutral-300 "#d4d4d4")
;;       (neutral-400 "#a3a3a3")
;;       (neutral-500 "#737373")
;;       (neutral-600 "#525252")
;;       (neutral-700 "#404040")
;;       (neutral-800 "#262626")
;;       (neutral-900 "#171717")

;;       (stone-50 "#fafaf9")
;;       (stone-100 "#f5f5f4")
;;       (stone-200 "#e7e5e4")
;;       (stone-300 "#d6d3d1")
;;       (stone-400 "#a8a29e")
;;       (stone-500 "#78716c")
;;       (stone-600 "#57534e")
;;       (stone-700 "#44403c")
;;       (stone-800 "#292524")
;;       (stone-900 "#1c1917")

;;       (amber-100 "#fef3c7")
;;       (amber-200 "#fde68a")
;;       (amber-300 "#fcd34d")
;;       (amber-400 "#fbbf24")
;;       (amber-500 "#f59e0b")
;;       (amber-600 "#ca8a04")
;;       (amber-700 "#a16207")
;;       (amber-800 "#854d0e")
;;       (amber-900 "#78350f")

;;       (orange-400 "#fb923c")

;;       (emerald-50 "#ecfdf5")
;;       (emerald-100 "#d1fae5")
;;       (emerald-200 "#a7f3d0")
;;       (emerald-300 "#6ee7b7")
;;       (emerald-400 "#34d399")
;;       (emerald-500 "#10b981")
;;       (emerald-600 "#059669")
;;       (emerald-700 "#047857")
;;       (emerald-800 "#065f46")
;;       (emerald-900 "#064e3b")

;;       (lime-100 "#ecfccb")
;;       (lime-300 "#bef264")
;;       (lime-400 "#a3e635")
;;       (lime-500 "#84cc16")

;;       (red-100 "#fee2e2")
;;       (red-300 "#fca5a5")
;;       (red-400 "#f87171")
;;       (red-500 "#ef4444")
;;       (red-600 "#dc2626")
;;       (red-700 "#b91c1c")
;;       (red-800 "#991b1b")
;;       (red-900 "#7f1d1d")

;;       (blue-50 "#eff6ff")
;;       (blue-100 "#dbeafe")
;;       (blue-200 "#bfdbfe")
;;       (blue-300 "#93c5fd")
;;       (blue-400 "#60a5fa")
;;       (blue-500 "#3b82f6")
;;       (blue-600 "#2563eb")
;;       (blue-700 "#1d4ed8")
;;       (blue-800 "#1e40af")
;;       (blue-900 "#1e3a8a")

;;       (cyan-50 "#ecfeff")
;;       (cyan-100 "#cffafe")
;;       (cyan-200 "#a5f3fc")
;;       (cyan-300 "#67e8f9")
;;       (cyan-400 "#22d3ee")
;;       (cyan-500 "#06b6d4")
;;       (cyan-600 "#0891b2")
;;       (cyan-700 "#0e7490")
;;       (cyan-800 "#155e75")
;;       (cyan-900 "#164e63")

;;       (pink-300 "#f9a8d4")
;;       (pink-400 "#f472b6")
;;       (pink-500 "#ec4899")

;;       (violet-400 "#a78bfa")
;;       (violet-500 "#8b5cf6")
;;       (violet-600 "#7c3aed"))

;;   (custom-theme-set-faces
;;    'hypalynx

;; ;;;;;;;;;;;;;;;;;;;;;
;;    ;; base theming    ;;
;; ;;;;;;;;;;;;;;;;;;;;;

;;    ;;`(default ((,class (:foreground ,neutral-900 :background ,neutral-100))))
;;    `(hl-line ((,class (:background ,neutral-200))))
;;    `(cursor ((,class (:background ,blue-800))))
;;    `(region ((,class (:background ,neutral-300))))
;;    `(highlight ((,class (:background ,neutral-200))))
;;    `(trailing-whitespace ((,class (:background ,red-300))))

;;    ;; N.B this is included to ensure we are using a font that has the
;;    ;; line glyph, it would be nicer to have a more concise function
;;    ;; for this.
;;    (if *hypalynx/fill-column-font*
;;        `(fill-column-indicator ((,class (:foreground ,neutral-300
;; 						     :font ,*hypalynx/fill-column-font*))))
;;      `(fill-column-indicator ((,class (:foreground ,neutral-300)))))

;;    ;; UI theming
;;    `(line-number ((,class (:foreground ,neutral-500))))
;;    `(line-number-current-line ((,class (:foreground ,blue-800))))
;;    `(mode-line ((,class (:background ,neutral-200))))

;;    ;; text/buffer theming
;;    `(font-lock-comment-face ((,class (:foreground ,neutral-500 :italic t))))
;;    ;; clojure keywords
;;    `(font-lock-constant-face ((,class (:foreground ,blue-900))))
;;    ;; clojure ns names, kw name prefixes, require aliases
;;    `(font-lock-type-face ((,class (:foreground ,emerald-700))))
;;    ;; clojure core fns e.g ns/defn/def
;;    `(font-lock-keyword-face ((,class (:foreground ,blue-900))))
;;    `(font-lock-function-name-face ((,class (:foreground ,emerald-800))))
;;    `(font-lock-string-face ((,class (:foreground ,emerald-800))))
;;    `(font-lock-variable-name-face ((,class (:foreground ,emerald-800))))
;;    `(font-lock-warning-face ((,class (:foreground ,red-800))))

;;    ;; org theming
;;    `(org-todo ((,class (:foreground ,red-800 :bold t))))
;;    `(org-date ((,class (:foreground ,red-800))))
;;    `(org-ellipsis ((,class (:foreground ,red-800))))
;;    `(org-level-1 ((,class (:foreground ,emerald-900 :bold t))))
;;    `(org-level-2 ((,class (:foreground ,blue-900 :bold t))))
;;    `(org-agenda-structure ((,class (:foreground ,blue-900 :bold t))))
;;    `(org-scheduled-previously ((,class (:foreground ,blue-900))))
;;    `(org-imminent-deadline ((,class (:foreground ,red-900 :bold t))))
;;    `(org-scheduled-today ((,class (:foreground ,emerald-800))))
;;    `(org-scheduled ((,class (:foreground ,emerald-800))))
;;    `(org-habit-clear-face ((,class (:background ,blue-700 :foreground ,blue-300))))
;;    `(org-habit-clear-future-face ((,class (:background ,blue-100))))
;;    `(org-habit-alert-future-face ((,class (:background ,amber-200))))
;;    `(org-habit-overdue-face ((,class (:background ,red-600 :foreground ,red-300))))
;;    `(org-habit-overdue-futureface ((,class (:background ,red-100))))
;;    ))

(defcustom hypalynx-disable-parens-highlight nil
  "Allows you to disable the font-locking that highlights
parentheses, brackets and braces.")

(defface hypalynx-parens-face
  '((t :foreground "#CC00CC"))
  "Font-lock for highlighting parentheses, brackets & braces in the hypalynx themes."
  :group 'hypalynx-parens)

;; (defface clojure-keyword-face
;;   '((t (:inherit font-lock-constant-face)))
;;   "Face used to font-lock Clojure keywords (:something)."
;;   :package-version '(clojure-mode . "3.0.0"))

;; (defface font-lock-hypalynx-parens-face
;;   '((((class color) (min-colors 88) (background light))
;;      :background "#008800")
;;     (((class color) (min-colors 88) (background dark))
;;      :background "darkolivegreen")
;;     (((class color) (min-colors 16) (background light))
;;      :background "darkseagreen2")
;;     (((class color) (min-colors 16) (background dark))
;;      :background "darkolivegreen")
;;     (((class color) (min-colors 8))
;;      :background "green" :foreground "black")
;;     (t :inverse-video t))
;;   "Font-lock for highlighting parentheses, brackets & braces in the hypalynx themes."
;;   :group 'hypalynx-theme-faces)

(define-minor-mode hypalynx-parens
  "Highlights parentheses, brackets & braces."
  :lighter " Hlp"
  (font-lock-add-keywords
   nil `((,(regexp-opt '("(" ")" "[" "]" "{" "}"))
	  . 'hypalynx-parens-face)))
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(when (not hypalynx-disable-parens-highlight)
  (dolist (hook '(emacs-lisp-mode-hook
		  eval-expression-minibuffer-setup-hook
		  ielm-mode-hook
		  lisp-mode-hook
		  lisp-interaction-mode-hook
		  clojure-mode-hook
		  scheme-mode-hook
		  racket-mode-hook))
    (add-hook hook #'hypalynx-parens)))


;;;###autoload
(when (and load-file-name
	   (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun hypalynx-dark ()
  "Apply the hypalynx dark theme."
  (interactive)
  (load-theme 'hypalynx-dark t))

;;;###autoload
(defun hypalynx-light ()
  "Apply the hypalynx light theme."
  (interactive)
  (load-theme 'hypalynx-light t))

(provide 'hypalynx)

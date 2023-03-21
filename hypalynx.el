;; use "describe-face" function to describe things that you want to theme.
;; teal/green actually => emerald/cyan
;; Built referencing sanityinc/tomorrow theme!
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
		       :background-light-tint ,(hl-g :stone-600)
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
		       :cyan ,(hl-g :cyan-400)
		       :pink ,(hl-g :pink-400)
		       :blue ,(hl-g :blue-900)
		       :yellow ,(hl-g :orange-400)
		       :orange ,(hl-g :orange-400)
		       :green ,(hl-g :emerald-500)

		       :primary ,(hl-g :emerald-500)
		       )
	  :light (:background ,(hl-g :stone-50)
			      :foreground ,(hl-g :stone-900)
			      :background-tint ,(hl-g :stone-200)
			      :background-light-tint ,(hl-g :stone-500)
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
			      :cyan ,(hl-g :cyan-400)
			      :pink ,(hl-g :pink-400)
			      :blue ,(hl-g :blue-900)
			      :yellow ,(hl-g :orange-400)
			      :orange ,(hl-g :orange-400)
			      :green ,(hl-g :emerald-500)


			      :primary ,(hl-g :emerald-800)
			      )))

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
      (font-lock-doc-face (:foreground ,comment))
      (font-lock-doc-string-face (:foreground ,comment))
      (font-lock-function-name-face (:foreground ,primary))
      ;; no italics originally
      (font-lock-constant-face (:foreground ,primary :slant italic))
      (font-lock-negation-char-face (:foreground ,pink))
      (font-lock-preprocessor-face (:foreground ,pink))
      (font-lock-regexp-grouping-backslash (:foreground ,pink))
      (font-lock-regexp-grouping-construct (:foreground ,pink))
      (font-lock-string-face (:foreground ,string))
      (font-lock-type-face (:foreground ,primary))
      (font-lock-warning-face (:weight bold :foreground ,pink))
      (shadow (:foreground ,background-light-tint))
      (success (:foreground ,lime))
      (error (:foreground ,pink))
      (warning (:foreground ,pink))
      (tooltip (:foreground ,foreground :background ,background :inverse-video t))
      (hypalynx-parens-face (:foreground ,parens))

      ;; Emacs interface
      (cursor (:background ,pink))
      (fringe (:background ,background-tint :foreground ,foreground))
      (fill-column-indicator (:foreground ,comment :family ,fci-font))
      (vertical-border (:foreground ,background-tint))
      (border (:background ,background-tint :foreground ,background-tint))
      (highlight (:inverse-video nil :background ,background-tint))
      (mode-line (:foreground ,foreground :background ,background-tint :weight normal
                              :box (:line-width 1 :color ,background-tint)))
      (mode-line-buffer-id (:foreground ,pink :background ,background-tint))
      (mode-line-inactive (:inherit mode-line
                                    :foreground ,comment
                                    :background ,background-tint
                                    :weight normal))
      (mode-line-emphasis (:foreground ,foreground :slant italic))
      (mode-line-highlight (:foreground ,pink :box nil :weight bold))
      (minibuffer-prompt (:foreground ,cyan))
      (region (:background ,background-tint :inverse-video nil :extend t))
      (secondary-selection (:background ,background-tint :extend t))
      (header-line (:inherit mode-line-inactive :foreground ,pink :background ,background-tint))

      (match (:foreground ,blue :background ,background :inverse-video t))
      (isearch (:foreground ,primary :background ,background :inverse-video t))
      (lazy-highlight (:foreground ,pink :background ,background :inverse-video t))
      (isearch-fail (:background ,background :inherit font-lock-warning-face :inverse-video t))

      (link (:foreground unspecified :underline t))
      (widget-button (:underline t))
      (widget-field (:background ,background-tint :box (:line-width 1 :color ,foreground)))

      ;;       ;; ansi-color (built-in, face scheme below from Emacs 28.1 onwards)
      ;; (ansi-color-black (:foreground ,term-black :background ,term-black))
      ;; (ansi-color-red (:foreground ,red :background ,red))
      ;; (ansi-color-green (:foreground ,green :background ,green))
      ;; (ansi-color-yellow (:foreground ,yellow :background ,yellow))
      ;; (ansi-color-blue (:foreground ,blue :background ,blue))
      ;; (ansi-color-magenta (:foreground ,purple :background ,purple))
      ;; (ansi-color-cyan (:foreground ,aqua :background ,aqua))
      ;; (ansi-color-white (:foreground ,term-white :background ,term-white))
      ;; (ansi-color-bright-black (:inherit ansi-color-black :weight bold))
      ;; (ansi-color-bright-red (:inherit ansi-color-red :weight bold))
      ;; (ansi-color-bright-green (:inherit ansi-color-green :weight bold))
      ;; (ansi-color-bright-yellow (:inherit ansi-color-yellow :weight bold))
      ;; (ansi-color-bright-blue (:inherit ansi-color-blue :weight bold))
      ;; (ansi-color-bright-magenta (:inherit ansi-color-magenta :weight bold))
      ;; (ansi-color-bright-cyan (:inherit ansi-color-cyan :weight bold))
      ;; (ansi-color-bright-white (:inherit ansi-color-white :weight bold))

      ;;       ;; ansi-term (built-in)
      ;; (term (:foreground unspecified :background unspecified :inherit default))
      ;; (term-color-black (:foreground ,term-black :background ,term-black))
      ;; (term-color-red (:foreground ,red :background ,red))
      ;; (term-color-green (:foreground ,green :background ,green))
      ;; (term-color-yellow (:foreground ,yellow :background ,yellow))
      ;; (term-color-blue (:foreground ,blue :background ,blue))
      ;; (term-color-magenta (:foreground ,purple :background ,purple))
      ;; (term-color-cyan (:foreground ,aqua :background ,aqua))
      ;; (term-color-white (:foreground ,term-white :background ,term-white))

      ;; calendar (built-in)
      (diary (:foreground ,yellow))
      (holiday (:foreground ,background :background ,orange))

      ;; Compilation (built-in)
      (compilation-column-number (:foreground ,yellow))
      (compilation-line-number (:foreground ,yellow))
      (compilation-message-face (:foreground ,blue))
      (compilation-mode-line-exit (:foreground ,green))
      (compilation-mode-line-fail (:foreground ,red))
      (compilation-mode-line-run (:foreground ,blue))

      ;; completion display (built-in)
      (completions-annotations (:foreground ,comment :slant italic))
      (completions-common-part (:foreground ,blue))
      (completions-first-difference (:foreground ,orange :weight bold))

      ;; custom (built-in)
      (custom-variable-tag (:foreground ,blue))
      (custom-group-tag (:foreground ,blue))
      (custom-state (:foreground ,green))

      ;; diff-mode (built-in)
      (diff-added (:foreground ,green :extend t))
      (diff-changed (:foreground ,blue))
      (diff-removed (:foreground ,orange :extend t))
      (diff-header (:foreground ,cyan :background unspecified :extend t))
      (diff-file-header (:foreground ,blue :background unspecified :extend t))
      (diff-hunk-header (:foreground ,pink))
      (diff-indicator-added (:inherit diff-added))
      (diff-indicator-changed (:inherit diff-changed))
      (diff-indicator-removed (:inherit diff-removed))
      (diff-refine-added (:foreground ,cyan))
      (diff-refine-changed (:foreground ,yellow))
      (diff-refine-removed (:foreground ,red))

      ;; ElDoc (built-in)
      (eldoc-highlight-function-argument (:foreground ,green :weight bold))

      ;; eshell (built-in)
      (eshell-prompt (:foreground ,yellow :weight bold))
      (eshell-ls-archive (:foreground ,blue))
      (eshell-ls-backup (:foreground ,comment))
      (eshell-ls-clutter (:foreground ,orange :weight bold))
      (eshell-ls-directory :foreground ,blue :weight bold)
      (eshell-ls-executable (:foreground ,yellow :weight bold))
      (eshell-ls-missing (:foreground ,red :weight bold))
      (eshell-ls-product (:foreground ,green))
      (eshell-ls-readonly (:foreground ,red))
      (eshell-ls-special (:foreground ,pink :weight bold))
      (eshell-ls-symlink (:foreground ,cyan :weight bold))
      (eshell-ls-unreadable (:foreground ,comment))

      ;; Flycheck (built-in)
      (flycheck-error (:underline (:style wave :color ,red)))
      (flycheck-info (:underline (:style wave :color ,cyan)))
      (flycheck-warning (:underline (:style wave :color ,orange)))
      (flycheck-fringe-error (:foreground ,red))
      (flycheck-fringe-info (:foreground ,cyan))
      (flycheck-fringe-warning (:foreground ,orange))
      (flycheck-color-mode-line-error-face (:foreground ,red))
      (flycheck-color-mode-line-warning-face (:foreground ,orange))
      (flycheck-color-mode-line-info-face (:foreground ,cyan))
      (flycheck-color-mode-line-running-face (:foreground ,comment))
      (flycheck-color-mode-line-success-face (:foreground ,green))

      ;; Flymake (built-in)
      (flymake-error (:underline (:style wave :color ,red)))
      (flymake-note (:underline (:style wave :color ,cyan)))
      (flymake-warning (:underline (:style wave :color ,orange)))

      ;; Flyspell (built-in)
      (flyspell-incorrect (:underline (:style wave :color ,red)))

      ;; grep (built-in)
      (grep-context-face (:foreground ,comment))
      (grep-error-face (:foreground ,red :weight bold :underline t))
      (grep-hit-face (:foreground ,blue))
      (grep-match-face (:foreground unspecified :background unspecified :inherit match))

      ;; icomplete (built-in)
      (icomplete-first-match (:foreground ,green :weight bold))

      ;; IDO (built-in)
      (ido-subdir (:foreground ,pink))
      (ido-first-match (:foreground ,orange))
      (ido-only-match (:foreground ,green))
      (ido-indicator (:foreground ,red :background ,background))
      (ido-virtual (:foreground ,comment))

      ;; info (built-in)
      (Info-quoted (:inherit font-lock-constant-face))
      (info-index-match (:inherit isearch))
      (info-menu-header (:foreground ,green :weight bold :height 1.4))
      (info-menu-star (:foreground ,yellow))
      (info-node (:foreground ,green :weight bold :slant italic))
      (info-title-1 (:weight bold :height 1.4))
      (info-title-2 (:weight bold :height 1.2))
      (info-title-3 (:weight bold :foreground ,orange))
      (info-title-4 (:weight bold :foreground ,pink))
      (info-xref-visited (:foreground ,comment :underline t))

      ;; kaocha-runner
      (kaocha-runner-error-face (:foreground ,red))
      (kaocha-runner-success-face (:foreground ,green))
      (kaocha-runner-warning-face (:foreground ,yellow))

      ;; Message-mode (built-in)
      (message-header-other (:foreground unspecified :background unspecified :weight normal))
      (message-header-subject (:inherit message-header-other :weight bold :foreground ,yellow))
      (message-header-to (:inherit message-header-other :weight bold :foreground ,orange))
      (message-header-cc (:inherit message-header-to :foreground unspecified))
      (message-header-name (:foreground ,blue :background unspecified))
      (message-header-newsgroups (:foreground ,cyan :background unspecified :slant normal))
      (message-separator (:foreground ,pink))

            ;; org-mode (built-in)
      (org-agenda-structure (:foreground ,pink))
      (org-agenda-current-time (:foreground ,yellow))
      (org-agenda-date (:foreground ,blue :underline nil))
      (org-agenda-done (:foreground ,green))
      (org-agenda-dimmed-todo-face (:foreground ,comment))
      (org-block (:background ,background-tint))
      (org-block-begin-line (:background ,background :foreground ,comment :slant italic))
      (org-block-end-line (:background ,background :foreground ,comment :slant italic))
      (org-code (:foreground ,yellow))
      (org-column (:background ,background-tint))
      (org-column-title (:inherit org-column :weight bold :underline t))
      (org-date (:foreground ,blue :underline t))
      (org-date-selected (:foreground ,cyan :inverse-video t))
      (org-document-info (:foreground ,cyan))
      (org-document-info-keyword (:foreground ,green))
      (org-document-title (:weight bold :foreground ,orange :height 1.4))
      (org-done (:foreground ,green))
      (org-ellipsis (:foreground ,comment))
      (org-footnote (:foreground ,cyan))
      (org-formula (:foreground ,red))
      (org-hide (:foreground ,background :background ,background))
      (org-habit-alert-face (:foreground ,background :background ,yellow))
      (org-habit-alert-future-face (:foreground ,background :background ,orange))
      (org-habit-clear-face (:foreground ,background :background ,comment))
      (org-habit-clear-future-face (:foreground ,background :background ,pink))
      (org-habit-overdue-face (:foreground ,background :background ,blue))
      (org-habit-overdue-future-face (:foreground ,background :background ,red))
      (org-habit-ready-face (:foreground ,background :background ,cyan))
      (org-habit-ready-future-face (:foreground ,background :background ,green))
      (org-headline-done (:foreground unspecified :strike-through t))
      (org-headline-todo (:foreground ,orange))
      (org-link (:foreground ,blue :underline t))
      (org-mode-line-clock-overrun (:inherit mode-line :background ,red))
      (org-scheduled (:foreground ,green))
      (org-scheduled-previously (:foreground ,cyan))
      (org-scheduled-today (:foreground ,green))
      (org-special-keyword (:foreground ,orange))
      (org-table (:foreground ,pink))
      (org-time-grid (:foreground ,yellow))
      (org-todo (:foreground ,red))
      (org-upcoming-deadline (:foreground ,orange))
      (org-warning (:weight bold :foreground ,red))

      ;; Ledger-mode
      (ledger-font-comment-face (:inherit font-lock-comment-face))
      (ledger-font-occur-narrowed-face (:inherit font-lock-comment-face :invisible t))
      (ledger-font-occur-xact-face (:inherit highlight))
      (ledger-font-payee-cleared-face (:foreground ,green))
      (ledger-font-payee-uncleared-face (:foreground ,cyan))
      (ledger-font-posting-date-face (:foreground ,orange))
      (ledger-font-posting-amount-face (:foreground ,foreground))
      (ledger-font-posting-account-cleared-face (:foreground ,blue))
      (ledger-font-posting-account-face (:foreground ,pink))
      (ledger-font-posting-account-pending-face (:foreground ,yellow))
      (ledger-font-xact-highlight-face (:inherit highlight))
      (ledger-occur-narrowed-face (:inherit font-lock-comment-face :invisible t))
      (ledger-occur-xact-face (:inherit highlight))

      ;; re-builder (built-in)
      (reb-match-0 (:foreground ,background :background ,cyan))
      (reb-match-1 (:foreground ,background :background ,yellow))
      (reb-match-2 (:foreground ,background :background ,orange))
      (reb-match-3 (:foreground ,background :background ,blue))

            ;; tab-bar (built-in)
      (tab-bar (:height 1.2 :foreground ,comment :background ,background-tint))
      (tab-bar-tab (:background ,background-tint
                                :foreground ,pink
                                :inverse-video nil
                                :box (:line-width 1 :style released-button)))
      (tab-bar-tab-inactive (:inherit tab-bar-tab
                                      :background ,comment
                                      :foreground ,background-tint
                                      :inverse-video t))

      ;; tab-line (built-in)
      (tab-line (:foreground ,comment :background ,background-tint))
      (tab-line-close-highlight (:foreground ,red))
      (tab-line-tab (:background ,background-tint
                                 :foreground ,pink
                                 :inverse-video nil
                                 :box (:line-width 1 :style released-button)))
      (tab-line-tab-inactive (:inherit tab-line-tab
                                       :background ,comment
                                       :foreground ,background-tint
                                       :inverse-video t))

      ;; whitespace (built-in)
      (whitespace-big-indent (:background ,red :foreground ,background-tint))
      (whitespace-empty (:background ,yellow :foreground ,orange))
      (whitespace-hspace (:background ,background-tint :foreground ,comment))
      (whitespace-indentation (:background ,background-tint :foreground ,comment))
      (whitespace-line (:background ,background-tint :foreground ,orange))
      (whitespace-newline (:background ,background-tint :foreground ,comment))
      (whitespace-space (:background ,background-tint :foreground ,comment))
      (whitespace-space-after-tab (:background ,background-tint :foreground ,yellow))
      (whitespace-space-before-tab (:background ,background-tint :foreground ,orange))
      (whitespace-tab (:background ,background-tint :foreground ,comment))
      (whitespace-trailing (:background ,orange :foreground ,background-tint))
      (trailing-whitespace (:inherit whitespace-trailing))

      ;; window-divider (built-in)
      (window-divider (:foreground ,comment))
      (window-divider-first-pixel (:foreground ,background-tint))
      (window-divider-last-pixel (:foreground ,background-tint))

      ;; CIDER
      (cider-debug-code-overlay-face (:background ,background-tint))
      (cider-deprecated-face (:foreground ,background-tint :background ,yellow))
      (cider-enlightened-face (:inherit cider-result-overlay-face :box (:color ,orange :line-width -1)))
      (cider-enlightened-local-face (:weight bold :foreground ,orange))
      (cider-error-highlight-face (:underline (:style wave :color ,red) :inherit unspecified))
      (cider-fringe-good-face (:foreground ,green))
      (cider-instrumented-face (:box (:color ,red :line-width -1)))
      (cider-result-overlay-face (:background ,background-tint :box (:line-width -1 :color ,yellow)))
      (cider-test-error-face (:foreground ,background-tint :background ,orange))
      (cider-test-failure-face (:foreground ,background-tint :background ,red))
      (cider-test-success-face (:foreground ,background-tint :background ,green))
      (cider-traced-face (:box ,cyan :line-width -1))
      (cider-warning-highlight-face (:underline (:style wave :color ,yellow) :inherit unspecified))

      ;; For Brian Carper's extended clojure syntax table
      (clojure-keyword (:foreground ,yellow))
      (clojure-parens (:foreground ,foreground))
      (clojure-braces (:foreground ,green))
      (clojure-brackets (:foreground ,yellow))
      (clojure-double-quote (:foreground ,cyan :background unspecified))
      (clojure-special (:foreground ,blue))
      (clojure-java-call (:foreground ,pink))

      ;; Clojure errors
      (clojure-test-failure-face (:background unspecified :inherit flymake-warnline))
      (clojure-test-error-face (:background unspecified :inherit flymake-errline))
      (clojure-test-success-face (:background unspecified :foreground unspecified :underline ,green))

      ;; company
      (company-preview (:foreground ,comment :background ,background-tint))
      (company-preview-common (:inherit company-preview :foreground ,red))
      (company-preview-search (:inherit company-preview :foreground ,blue))
      (company-tooltip (:background ,background-tint))
      (company-tooltip-selection (:foreground ,comment :inverse-video t))
      (company-tooltip-common (:inherit company-tooltip :foreground ,red))
      (company-tooltip-common-selection (:inherit company-tooltip-selection :foreground ,red))
      (company-tooltip-search (:inherit company-tooltip :foreground ,blue))
      (company-tooltip-annotation (:inherit company-tooltip :foreground ,green))
      (company-tooltip-annotation-selection (:inherit company-tooltip-selection :foreground ,green))
      (company-scrollbar-bg (:inherit 'company-tooltip :background ,background-tint))
      (company-scrollbar-fg (:background ,background-tint))
      (company-echo-common (:inherit company-echo :foreground ,red))

      ;; Magit
      (magit-bisect-bad (:foreground ,red))
      (magit-bisect-good (:foreground ,green))
      (magit-bisect-skip (:foreground ,orange))
      (magit-blame-date (:foreground ,red))
      (magit-blame-heading (:foreground ,orange :background ,background-tint :extend t))
      (magit-branch-current (:foreground ,blue))
      (magit-branch-local (:foreground ,cyan))
      (magit-branch-remote (:foreground ,green))
      (magit-cherry-equivalent (:foreground ,pink))
      (magit-cherry-unmatched (:foreground ,cyan))
      (magit-diff-added (:foreground ,green :extend t))
      (magit-diff-added-highlight (:foreground ,green :background ,background-tint :extend t))
      (magit-diff-base (:foreground ,background :background ,orange :extend t))
      (magit-diff-base-highlight (:foreground ,orange :background ,background-tint :extend t))
      (magit-diff-context (:foreground ,comment :extend t))
      (magit-diff-context-highlight (:foreground ,comment :background ,background-tint :extend t))
      (magit-diff-file-heading (:foreground ,foreground :extend t))
      (magit-diff-file-heading-highlight (:background ,background-tint :weight bold :extend t))
      (magit-diff-file-heading-selection (:foreground ,orange :background ,background-tint :extend t))
      (magit-diff-hunk-heading (:foreground ,foreground :background ,background-tint :extend t))
      (magit-diff-hunk-heading-highlight (:background ,background-tint :extend t))
      (magit-diff-lines-heading (:foreground ,yellow :background ,red :extend t))
      (magit-diff-removed (:foreground ,orange :extend t))
      (magit-diff-removed-highlight (:foreground ,orange :background ,background-tint :extend t))
      (magit-diffstat-added (:foreground ,green))
      (magit-diffstat-removed (:foreground ,orange))
      (magit-dimmed (:foreground ,comment))
      (magit-filename (:foreground ,pink))
      (magit-hash (:foreground ,comment))
      (magit-header-line (:inherit nil :weight bold))
      (magit-log-author (:foreground ,orange))
      (magit-log-date (:foreground ,blue))
      (magit-log-graph (:foreground ,comment))
      (magit-mode-line-process (:foreground ,orange))
      (magit-mode-line-process-error (:foreground ,red))
      (magit-process-ng (:inherit error))
      (magit-process-ok (:inherit success))
      (magit-reflog-amend (:foreground ,pink))
      (magit-reflog-checkout (:foreground ,blue))
      (magit-reflog-cherry-pick (:foreground ,green))
      (magit-reflog-commit (:foreground ,green))
      (magit-reflog-merge (:foreground ,green))
      (magit-reflog-other (:foreground ,cyan))
      (magit-reflog-rebase (:foreground ,pink))
      (magit-reflog-remote (:foreground ,cyan))
      (magit-reflog-reset (:inherit error))
      (magit-refname (:foreground ,comment))
      (magit-section-heading (:foreground ,yellow :weight bold :extend t))
      (magit-section-heading-selection (:foreground ,orange :weight bold :extend t))
      (magit-section-highlight (:background ,background-tint :weight bold :extend t))
      (magit-sequence-drop (:foreground ,red))
      (magit-sequence-head (:foreground ,blue))
      (magit-sequence-part (:foreground ,orange))
      (magit-sequence-stop (:foreground ,green))
      (magit-signature-bad (:inherit error))
      (magit-signature-error (:inherit error))
      (magit-signature-expired (:foreground ,orange))
      (magit-signature-good (:inherit success))
      (magit-signature-revoked (:foreground ,pink))
      (magit-signature-untrusted (:foreground ,cyan))
      (magit-tag (:foreground ,yellow))

      ;; markdown
      (markdown-url-face (:inherit link))
      (markdown-link-face (:foreground ,blue :underline t))
      (markdown-code-face (:inherit fixed-pitch :background ,background :foreground ,pink))
      (markdown-inline-code-face (:inherit markdown-code-face))

      ;; mu4e
      (mu4e-header-highlight-face (:underline nil :inherit region))
      (mu4e-header-marks-face (:underline nil :foreground ,yellow))
      (mu4e-flagged-face (:foreground ,orange :inherit nil))
      (mu4e-replied-face (:foreground ,blue :inherit nil))
      (mu4e-unread-face (:foreground ,yellow :inherit nil))
      (mu4e-cited-1-face (:foreground ,blue :slant normal))
      (mu4e-cited-2-face (:foreground ,pink :slant normal))
      (mu4e-cited-3-face (:foreground ,cyan :slant normal))
      (mu4e-cited-4-face (:foreground ,yellow :slant normal))
      (mu4e-cited-5-face (:foreground ,orange :slant normal))
      (mu4e-cited-6-face (:foreground ,blue :slant normal))
      (mu4e-cited-7-face (:foreground ,pink :slant normal))
      (mu4e-ok-face (:foreground ,green))
      (mu4e-view-contact-face (:inherit nil :foreground ,yellow))
      (mu4e-view-link-face (:inherit link :foreground ,blue))
      (mu4e-view-url-number-face (:inherit nil :foreground ,cyan))
      (mu4e-view-attach-number-face (:inherit nil :foreground ,orange))
      (mu4e-highlight-face (:inherit highlight))
      (mu4e-title-face (:inherit nil :foreground ,green))

      ;; SLIME
      (slime-highlight-edits-face (:weight bold))
      (slime-repl-input-face (:weight normal :underline nil))
      (slime-repl-prompt-face (:underline nil :weight bold :foreground ,pink))
      (slime-repl-result-face (:foreground ,green))
      (slime-repl-output-face (:foreground ,blue :background ,background))
      (slime-repl-inputed-output-face (:foreground ,comment))

      ;; ;; vterm
      ;; (vterm-color-black (:background ,term-black :foreground ,term-black))
      ;; (vterm-color-blue (:background ,blue :foreground ,blue))
      ;; (vterm-color-cyan (:background ,aqua :foreground ,aqua))
      ;; (vterm-color-default (:foreground unspecified :background unspecified :inherit default))
      ;; (vterm-color-green (:background ,green :foreground ,green))
      ;; (vterm-color-magenta (:background ,purple :foreground ,purple))
      ;; (vterm-color-red (:background ,red :foreground ,red))
      ;; (vterm-color-white (:background ,term-white :foreground ,term-white))
      ;; (vterm-color-yellow (:background ,yellow :foreground ,yellow))
      ;; (vterm-color-underline (:underline t))
      ;; (vterm-color-inverse-video (:background ,background :inverse-video t))

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
	 `(beacon-color ,red)
	 `(fci-rule-color ,background-tint)
         `(vc-annotate-color-map
           '((20  . ,red)
             (40  . ,orange)
             (60  . ,yellow)
             (80  . ,green)
             (100 . ,cyan)
             (120 . ,blue)
             (140 . ,pink)
             (160 . ,red)
             (180 . ,orange)
             (200 . ,yellow)
             (220 . ,green)
             (240 . ,cyan)
             (260 . ,blue)
             (280 . ,pink)
             (300 . ,red)
             (320 . ,orange)
             (340 . ,yellow)
             (360 . ,green)))
         `(vc-annotate-very-old-color nil)
         `(vc-annotate-background nil)
         `(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
         `(ansi-color-names-vector (vector ,background ,red ,green ,yellow ,blue ,pink ,cyan ,foreground))
         `(window-divider-mode nil)
	 ))
       (provide-theme ',name))))


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

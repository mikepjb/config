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
  '(
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
    :emerald-100 "#d1fae5"
    :emerald-200 "#99f6e4"
    :emerald-300 "#6ee7b7"
    :emerald-400 "#2dd4bf"
    :emerald-500 "#14b8a6"
    :emerald-600 "#0d9488"
    :emerald-700 "#047857"
    :emerald-800 "#115e59"
    :emerald-900 "#064e3b"
    :sky-200 "#bae6fd"
    :sky-300 "#7dd3fc"
    :sky-400 "#38bdf8"
    :sky-700 "#0369a1"
    :sky-800 "#075985"
    :sky-900 "#0c4a6e"
    :amber-700 "#b45309"
    :amber-600 "#d97706"
    :amber-500 "#f59e0b"
    :amber-400 "#fbbf24"
    :violet-700 "#6d28d9"
    :violet-800 "#5b21b6"
    :violet-900 "#4c1d95"
    :rose-200 "#fecdd3"
    :rose-300 "#fda4af"
    :rose-400 "#fb7185"
    :rose-900 "#881337"))

(defun hl-g (kw)
  (plist-get hypalynx-color-palette kw))

;; should we even have lime/red/cyan/pink/blue/yellow/green?
;; probably.. if only for ansi/term compatability

(defconst hypalynx-color-theme
  `(:dark (
	   :foreground ,(hl-g :stone-50)
	   :foreground+ ,(hl-g :stone-100)
	   :foreground++ ,(hl-g :stone-200)
	   :background ,(hl-g :stone-900)
	   :background+ ,(hl-g :stone-800)
	   :background++ ,(hl-g :stone-700)
	   :background+++ ,(hl-g :stone-500)
	   :primary ,(hl-g :emerald-400)
	   :primary+ ,(hl-g :emerald-300)
	   :primary++ ,(hl-g :emerald-200)
	   :primary+++ ,(hl-g :emerald-100)
	   :secondary ,(hl-g :sky-400)
	   :secondary+ ,(hl-g :sky-300)
	   :secondary++ ,(hl-g :sky-200)
	   :tertiary ,(hl-g :amber-700)
	   :violet ,(hl-g :violet-700)
	   :violet+ ,(hl-g :violet-800)
	   :violet++ ,(hl-g :violet-900)
	   :rose ,(hl-g :rose-200))
	  :light (
		  :foreground ,(hl-g :stone-900)
		  :foreground+ ,(hl-g :stone-800)
		  :foreground++ ,(hl-g :stone-700)
		  :background ,(hl-g :stone-100)
		  :background+ ,(hl-g :stone-200)
		  :background++ ,(hl-g :stone-300)
		  :background+++ ,(hl-g :stone-500)
		  :primary ,(hl-g :emerald-900)
		  :primary+ ,(hl-g :emerald-800)
		  :primary++ ,(hl-g :emerald-700)
		  :primary+++ ,(hl-g :emerald-600)
		  :secondary ,(hl-g :sky-900)
		  :secondary+ ,(hl-g :sky-800)
		  :secondary++ ,(hl-g :sky-700)
		  :tertiary ,(hl-g :amber-700)
		  :tertiary+ ,(hl-g :amber-600)
		  :tertiary++ ,(hl-g :amber-500)
		  :violet ,(hl-g :violet-900)
		  :violet+ ,(hl-g :violet-800)
		  :violet++ ,(hl-g :violet-700)
		  :rose ,(hl-g :rose-900))))

(defun hypalynx--color-theme-keys ()
  (seq-filter (apply-partially #'keywordp)
	      (plist-get hypalynx-color-theme :light)))

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
      (hl-line (:background ,background+))
      (show-paren-match (:foreground ,tertiary++)) ;; TODO not clear in light
      (show-paren-mismatch (:foreground ,rose))
      (font-lock-builtin-face (:foreground ,primary :slant italic))
      (font-lock-comment-delimiter-face (:foreground ,background+++ :slant italic))
      (font-lock-comment-face (:foreground ,background+++ :slant italic))
      (font-lock-keyword-face (:foreground ,primary))
      (font-lock-variable-name-face (:foreground ,secondary))
      (font-lock-doc-face (:foreground ,background+++))
      (font-lock-doc-string-face (:foreground ,background+++))
      (font-lock-function-name-face (:foreground ,secondary+))
      ;; no italics originally
      (font-lock-constant-face (:foreground ,primary :slant italic))
      (font-lock-negation-char-face (:foreground ,rose))
      (font-lock-preprocessor-face (:foreground ,rose))
      (font-lock-regexp-grouping-backslash (:foreground ,rose))
      (font-lock-regexp-grouping-construct (:foreground ,rose))
      (font-lock-string-face (:foreground ,primary++))
      (font-lock-type-face (:foreground ,primary))
      (font-lock-warning-face (:weight bold :foreground ,rose))
      (shadow (:foreground ,background+++)) ;; includes line numbers
      (success (:foreground ,violet))
      (error (:foreground ,rose))
      (warning (:foreground ,rose))
      (tooltip (:foreground ,foreground :background ,background :inverse-video t))
      (hypalynx-parens-face (:foreground ,primary+++))

      ;; Emacs interface
      (cursor (:background ,rose))
      (fringe (:background ,background :foreground ,background+++))
      (fill-column-indicator (:foreground ,background++ :family ,fci-font))
      (vertical-border (:foreground ,background++))
      (border (:background ,background+ :foreground ,background++))
      (highlight (:inverse-video nil :background ,background+++))
      (mode-line (:foreground ,foreground :background ,background+ :weight normal
                              :box (:line-width 1 :color ,background++)))
      (mode-line-buffer-id (:foreground ,primary :background ,background+))
      (mode-line-inactive (:inherit mode-line
                                    :foreground ,background+++
                                    :background ,background+
                                    :weight normal))
      (mode-line-emphasis (:foreground ,foreground :slant italic))
      (mode-line-highlight (:foreground ,rose :box nil :weight bold))
      (minibuffer-prompt (:foreground ,primary+++))
      (region (:background ,background+ :inverse-video nil :extend t))
      (secondary-selection (:background ,background+ :extend t))
      (header-line (:inherit mode-line-inactive :foreground ,rose :background ,background+))

      (match (:foreground ,primary+++ :background ,background :inverse-video t))
      (isearch (:foreground ,primary :background ,background :inverse-video t))
      (lazy-highlight (:foreground ,rose :background ,background :inverse-video t))
      (isearch-fail (:background ,background+++ :inherit font-lock-warning-face :inverse-video t))

      (link (:foreground unspecified :underline t))
      (widget-button (:underline t))
      (widget-field (:background ,background+ :box (:line-width 1 :color ,foreground)))

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
      (diary (:foreground ,tertiary))
      (holiday (:foreground ,background :background ,rose))

      ;; Compilation (built-in)
      (compilation-column-number (:foreground ,tertiary))
      (compilation-line-number (:foreground ,tertiary))
      (compilation-message-face (:foreground ,secondary))
      (compilation-mode-line-exit (:foreground ,primary))
      (compilation-mode-line-fail (:foreground ,rose))
      (compilation-mode-line-run (:foreground ,secondary))

      ;; completion display (built-in)
      (completions-annotations (:foreground ,background+++ :slant italic))
      (completions-common-part (:foreground ,secondary))
      (completions-first-difference (:foreground ,tertiary :weight bold))

      ;; custom (built-in)
      (custom-variable-tag (:foreground ,secondary))
      (custom-group-tag (:foreground ,secondary))
      (custom-state (:foreground ,primary))

      ;; diff-mode (built-in)
      (diff-added (:foreground ,primary :extend t))
      (diff-changed (:foreground ,secondary))
      (diff-removed (:foreground ,rose :extend t))
      (diff-header (:foreground ,secondary++ :background unspecified :extend t))
      (diff-file-header (:foreground ,secondary++ :background unspecified :extend t))
      (diff-hunk-header (:foreground ,rose))
      (diff-indicator-added (:inherit diff-added))
      (diff-indicator-changed (:inherit diff-changed))
      (diff-indicator-removed (:inherit diff-removed))
      (diff-refine-added (:foreground ,primary++))
      (diff-refine-changed (:foreground ,tertiary))
      (diff-refine-removed (:foreground ,rose))

      ;; ElDoc (built-in)
      (eldoc-highlight-function-argument (:foreground ,primary :weight bold))

      ;; eshell (built-in)
      (eshell-prompt (:foreground ,tertiary :weight bold))
      (eshell-ls-archive (:foreground ,secondary))
      (eshell-ls-backup (:foreground ,background+++))
      (eshell-ls-clutter (:foreground ,tertiary :weight bold))
      (eshell-ls-directory :foreground ,secondary :weight bold)
      (eshell-ls-executable (:foreground ,tertiary :weight bold))
      (eshell-ls-missing (:foreground ,rose :weight bold))
      (eshell-ls-product (:foreground ,primary))
      (eshell-ls-readonly (:foreground ,rose))
      (eshell-ls-special (:foreground ,rose :weight bold))
      (eshell-ls-symlink (:foreground ,secondary++ :weight bold))
      (eshell-ls-unreadable (:foreground ,background+++))

      ;; Flycheck (built-in)
      (flycheck-error (:underline (:style wave :color ,rose)))
      (flycheck-info (:underline (:style wave :color ,primary++)))
      (flycheck-warning (:underline (:style wave :color ,tertiary)))
      (flycheck-fringe-error (:foreground ,rose))
      (flycheck-fringe-info (:foreground ,secondary++))
      (flycheck-fringe-warning (:foreground ,tertiary))
      (flycheck-color-mode-line-error-face (:foreground ,rose))
      (flycheck-color-mode-line-warning-face (:foreground ,tertiary))
      (flycheck-color-mode-line-info-face (:foreground ,secondary))
      (flycheck-color-mode-line-running-face (:foreground ,background+++))
      (flycheck-color-mode-line-success-face (:foreground ,primary))

      ;; Flymake (built-in)
      (flymake-error (:underline (:style wave :color ,rose)))
      (flymake-note (:underline (:style wave :color ,secondary)))
      (flymake-warning (:underline (:style wave :color ,tertiary)))

      ;; Flyspell (built-in)
      (flyspell-incorrect (:underline (:style wave :color ,rose)))

      ;; grep (built-in)
      (grep-context-face (:foreground ,background+++))
      (grep-error-face (:foreground ,rose :weight bold :underline t))
      (grep-hit-face (:foreground ,secondary))
      (grep-match-face (:foreground unspecified :background unspecified :inherit match))

      ;; icomplete (built-in)
      (icomplete-first-match (:foreground ,primary :weight bold))

      ;; IDO (built-in)
      (ido-subdir (:foreground ,rose))
      (ido-first-match (:foreground ,tertiary))
      (ido-only-match (:foreground ,primary))
      (ido-indicator (:foreground ,rose :background ,background))
      (ido-virtual (:foreground ,background+++))

      ;; info (built-in)
      (Info-quoted (:inherit font-lock-constant-face))
      (info-index-match (:inherit isearch))
      (info-menu-header (:foreground ,primary :weight bold :height 1.4))
      (info-menu-star (:foreground ,tertiary))
      (info-node (:foreground ,primary :weight bold :slant italic))
      (info-title-1 (:weight bold :height 1.4))
      (info-title-2 (:weight bold :height 1.2))
      (info-title-3 (:weight bold :foreground ,tertiary))
      (info-title-4 (:weight bold :foreground ,rose))
      (info-xref-visited (:foreground ,background+++ :underline t))

      ;; kaocha-runner
      (kaocha-runner-error-face (:foreground ,rose))
      (kaocha-runner-success-face (:foreground ,primary))
      (kaocha-runner-warning-face (:foreground ,tertiary))

      ;; Message-mode (built-in)
      (message-header-other (:foreground unspecified :background unspecified :weight normal))
      (message-header-subject (:inherit message-header-other :weight bold :foreground ,tertiary))
      (message-header-to (:inherit message-header-other :weight bold :foreground ,tertiary))
      (message-header-cc (:inherit message-header-to :foreground unspecified))
      (message-header-name (:foreground ,secondary :background unspecified))
      (message-header-newsgroups (:foreground ,secondary++ :background unspecified :slant normal))
      (message-separator (:foreground ,rose))

      ;; org-mode (built-in)
      (org-agenda-structure (:foreground ,rose))
      (org-agenda-current-time (:foreground ,tertiary))
      (org-agenda-date (:foreground ,secondary :underline nil))
      (org-agenda-done (:foreground ,primary))
      (org-agenda-dimmed-todo-face (:foreground ,background+++))
      (org-block (:background ,background))
      (org-block-begin-line (:background ,background :foreground ,background+++ :slant italic))
      (org-block-end-line (:background ,background :foreground ,background+++ :slant italic))
      (org-code (:foreground ,tertiary))
      (org-column (:background ,background+))
      (org-column-title (:inherit org-column :weight bold :underline t))
      (org-date (:foreground ,secondary :underline t))
      (org-date-selected (:foreground ,secondary++ :inverse-video t))
      (org-document-info (:foreground ,secondary++))
      (org-document-info-keyword (:foreground ,primary))
      (org-document-title (:weight bold :foreground ,tertiary :height 1.4))
      (org-done (:foreground ,primary))
      (org-ellipsis (:foreground ,background+++))
      (org-footnote (:foreground ,secondary++))
      (org-formula (:foreground ,rose))
      (org-hide (:foreground ,background :background ,background))
      (org-habit-alert-face (:foreground ,background :background ,tertiary))
      (org-habit-alert-future-face (:foreground ,background :background ,tertiary))
      (org-habit-clear-face (:foreground ,background :background ,background+++))
      (org-habit-clear-future-face (:foreground ,background :background ,rose))
      (org-habit-overdue-face (:foreground ,background :background ,secondary))
      (org-habit-overdue-future-face (:foreground ,background :background ,rose))
      (org-habit-ready-face (:foreground ,background :background ,secondary++))
      (org-habit-ready-future-face (:foreground ,background :background ,primary))
      (org-headline-done (:foreground unspecified :strike-through t))
      (org-headline-todo (:foreground ,tertiary))
      (org-link (:foreground ,secondary :underline t))
      (org-mode-line-clock-overrun (:inherit mode-line :background ,rose))
      (org-scheduled (:foreground ,primary))
      (org-scheduled-previously (:foreground ,secondary++))
      (org-scheduled-today (:foreground ,primary))
      (org-special-keyword (:foreground ,tertiary))
      (org-table (:foreground ,rose))
      (org-time-grid (:foreground ,tertiary))
      (org-todo (:foreground ,rose))
      (org-upcoming-deadline (:foreground ,tertiary))
      (org-warning (:weight bold :foreground ,rose))
      (org-level-1 (:weight bold :foreground ,primary))
      (org-level-2 (:weight bold :foreground ,secondary))
      (org-level-3 (:weight bold :foreground ,primary+))
      (org-level-4 (:weight bold :foreground ,secondary+))


      ;; Ledger-mode
      (ledger-font-background+++-face (:inherit font-lock-background+++-face))
      (ledger-font-occur-narrowed-face (:inherit font-lock-background+++-face :invisible t))
      (ledger-font-occur-xact-face (:inherit highlight))
      (ledger-font-payee-clearose-face (:foreground ,primary))
      (ledger-font-payee-unclearose-face (:foreground ,secondary++))
      (ledger-font-posting-date-face (:foreground ,tertiary))
      (ledger-font-posting-amount-face (:foreground ,foreground))
      (ledger-font-posting-account-clearose-face (:foreground ,secondary))
      (ledger-font-posting-account-face (:foreground ,rose))
      (ledger-font-posting-account-pending-face (:foreground ,tertiary))
      (ledger-font-xact-highlight-face (:inherit highlight))
      (ledger-occur-narrowed-face (:inherit font-lock-background+++-face :invisible t))
      (ledger-occur-xact-face (:inherit highlight))

      ;; re-builder (built-in)
      (reb-match-0 (:foreground ,background :background ,secondary++))
      (reb-match-1 (:foreground ,background :background ,tertiary))
      (reb-match-2 (:foreground ,background :background ,tertiary))
      (reb-match-3 (:foreground ,background :background ,secondary))

            ;; tab-bar (built-in)
      (tab-bar (:height 1.2 :foreground ,background+++ :background ,background+))
      (tab-bar-tab (:background ,background+
                                :foreground ,rose
                                :inverse-video nil
                                :box (:line-width 1 :style released-button)))
      (tab-bar-tab-inactive (:inherit tab-bar-tab
                                      :background ,background+++
                                      :foreground ,background+
                                      :inverse-video t))

      ;; tab-line (built-in)
      (tab-line (:foreground ,background+++ :background ,background+))
      (tab-line-close-highlight (:foreground ,rose))
      (tab-line-tab (:background ,background+
                                 :foreground ,rose
                                 :inverse-video nil
                                 :box (:line-width 1 :style released-button)))
      (tab-line-tab-inactive (:inherit tab-line-tab
                                       :background ,background+++
                                       :foreground ,background+
                                       :inverse-video t))

      ;; whitespace (built-in)
      (whitespace-big-indent (:background ,rose :foreground ,background+))
      (whitespace-empty (:background ,tertiary :foreground ,tertiary))
      (whitespace-hspace (:background ,background+ :foreground ,background+++))
      (whitespace-indentation (:background ,background+ :foreground ,background+++))
      (whitespace-line (:background ,background+ :foreground ,tertiary))
      (whitespace-newline (:background ,background+ :foreground ,background+++))
      (whitespace-space (:background ,background+ :foreground ,background+++))
      (whitespace-space-after-tab (:background ,background+ :foreground ,tertiary))
      (whitespace-space-before-tab (:background ,background+ :foreground ,tertiary))
      (whitespace-tab (:background ,background+ :foreground ,background+++))
      (whitespace-trailing (:background ,tertiary :foreground ,background+))
      (trailing-whitespace (:inherit whitespace-trailing))

      ;; window-divider (built-in)
      (window-divider (:foreground ,background+++))
      (window-divider-first-pixel (:foreground ,background+))
      (window-divider-last-pixel (:foreground ,background+))

      ;; CIDER
      (cider-debug-code-overlay-face (:background ,background+))
      (cider-deprecated-face (:foreground ,background+ :background ,tertiary))
      (cider-enlightened-face (:inherit cider-result-overlay-face :box (:color ,tertiary :line-width -1)))
      (cider-enlightened-local-face (:weight bold :foreground ,tertiary))
      (cider-error-highlight-face (:underline (:style wave :color ,rose) :inherit unspecified))
      (cider-fringe-good-face (:foreground ,primary))
      (cider-instrumented-face (:box (:color ,rose :line-width -1)))
      (cider-result-overlay-face (:background ,background+ :box (:line-width -1 :color ,tertiary)))
      (cider-test-error-face (:foreground ,background+ :background ,tertiary))
      (cider-test-failure-face (:foreground ,background+ :background ,rose))
      (cider-test-success-face (:foreground ,background+ :background ,primary))
      (cider-traced-face (:box ,secondary++ :line-width -1))
      (cider-warning-highlight-face (:underline (:style wave :color ,tertiary) :inherit unspecified))

      ;; For Brian Carper's extended clojure syntax table
      (clojure-keyword (:foreground ,tertiary))
      (clojure-parens (:foreground ,foreground))
      (clojure-braces (:foreground ,primary))
      (clojure-brackets (:foreground ,tertiary))
      (clojure-double-quote (:foreground ,secondary++ :background unspecified))
      (clojure-special (:foreground ,secondary))
      (clojure-java-call (:foreground ,rose))

      ;; Clojure errors
      (clojure-test-failure-face (:background unspecified :inherit flymake-warnline))
      (clojure-test-error-face (:background unspecified :inherit flymake-errline))
      (clojure-test-success-face (:background unspecified :foreground unspecified :underline ,primary))

      ;; company
      (company-preview (:foreground ,background+++ :background ,background+))
      (company-preview-common (:inherit company-preview :foreground ,rose))
      (company-preview-search (:inherit company-preview :foreground ,secondary))
      (company-tooltip (:background ,background+))
      (company-tooltip-selection (:foreground ,background+++ :inverse-video t))
      (company-tooltip-common (:inherit company-tooltip :foreground ,rose))
      (company-tooltip-common-selection (:inherit company-tooltip-selection :foreground ,rose))
      (company-tooltip-search (:inherit company-tooltip :foreground ,secondary))
      (company-tooltip-annotation (:inherit company-tooltip :foreground ,primary))
      (company-tooltip-annotation-selection (:inherit company-tooltip-selection :foreground ,primary))
      (company-scrollbar-bg (:inherit 'company-tooltip :background ,background+))
      (company-scrollbar-fg (:background ,background+))
      (company-echo-common (:inherit company-echo :foreground ,rose))

      ;; Magit
      (magit-bisect-bad (:foreground ,rose))
      (magit-bisect-good (:foreground ,primary))
      (magit-bisect-skip (:foreground ,tertiary))
      (magit-blame-date (:foreground ,rose))
      (magit-blame-heading (:foreground ,tertiary :background ,background+ :extend t))
      (magit-branch-current (:foreground ,secondary))
      (magit-branch-local (:foreground ,secondary++))
      (magit-branch-remote (:foreground ,primary))
      (magit-cherry-equivalent (:foreground ,rose))
      (magit-cherry-unmatched (:foreground ,secondary++))
      (magit-diff-added (:foreground ,primary :extend t))
      (magit-diff-added-highlight (:foreground ,primary :background ,background+ :extend t))
      (magit-diff-base (:foreground ,background :background ,tertiary :extend t))
      (magit-diff-base-highlight (:foreground ,tertiary :background ,background+ :extend t))
      (magit-diff-context (:foreground ,background+++ :extend t))
      (magit-diff-context-highlight (:foreground ,background+++ :background ,background+ :extend t))
      (magit-diff-file-heading (:foreground ,foreground :extend t))
      (magit-diff-file-heading-highlight (:background ,background+ :weight bold :extend t))
      (magit-diff-file-heading-selection (:foreground ,tertiary :background ,background+ :extend t))
      (magit-diff-hunk-heading (:foreground ,foreground :background ,background+ :extend t))
      (magit-diff-hunk-heading-highlight (:background ,background+ :extend t))
      (magit-diff-lines-heading (:foreground ,tertiary :background ,rose :extend t))
      (magit-diff-removed (:foreground ,tertiary :extend t))
      (magit-diff-removed-highlight (:foreground ,tertiary :background ,background+ :extend t))
      (magit-diffstat-added (:foreground ,primary))
      (magit-diffstat-removed (:foreground ,tertiary))
      (magit-dimmed (:foreground ,background+++))
      (magit-filename (:foreground ,rose))
      (magit-hash (:foreground ,background+++))
      (magit-header-line (:inherit nil :weight bold))
      (magit-log-author (:foreground ,tertiary))
      (magit-log-date (:foreground ,secondary))
      (magit-log-graph (:foreground ,background+++))
      (magit-mode-line-process (:foreground ,tertiary))
      (magit-mode-line-process-error (:foreground ,rose))
      (magit-process-ng (:inherit error))
      (magit-process-ok (:inherit success))
      (magit-reflog-amend (:foreground ,rose))
      (magit-reflog-checkout (:foreground ,secondary))
      (magit-reflog-cherry-pick (:foreground ,primary))
      (magit-reflog-commit (:foreground ,primary))
      (magit-reflog-merge (:foreground ,primary))
      (magit-reflog-other (:foreground ,secondary++))
      (magit-reflog-rebase (:foreground ,rose))
      (magit-reflog-remote (:foreground ,secondary++))
      (magit-reflog-reset (:inherit error))
      (magit-refname (:foreground ,background+++))
      (magit-section-heading (:foreground ,tertiary :weight bold :extend t))
      (magit-section-heading-selection (:foreground ,tertiary :weight bold :extend t))
      (magit-section-highlight (:background ,background+ :weight bold :extend t))
      (magit-sequence-drop (:foreground ,rose))
      (magit-sequence-head (:foreground ,secondary))
      (magit-sequence-part (:foreground ,tertiary))
      (magit-sequence-stop (:foreground ,primary))
      (magit-signature-bad (:inherit error))
      (magit-signature-error (:inherit error))
      (magit-signature-expirose (:foreground ,tertiary))
      (magit-signature-good (:inherit success))
      (magit-signature-revoked (:foreground ,rose))
      (magit-signature-untrusted (:foreground ,secondary++))
      (magit-tag (:foreground ,tertiary))

      ;; markdown
      (markdown-url-face (:inherit link))
      (markdown-link-face (:foreground ,secondary :underline t))
      (markdown-code-face (:inherit fixed-pitch :background ,background :foreground ,rose))
      (markdown-inline-code-face (:inherit markdown-code-face))

      ;; mu4e
      (mu4e-header-highlight-face (:underline nil :inherit region))
      (mu4e-header-marks-face (:underline nil :foreground ,tertiary))
      (mu4e-flagged-face (:foreground ,tertiary :inherit nil))
      (mu4e-replied-face (:foreground ,secondary :inherit nil))
      (mu4e-unread-face (:foreground ,tertiary :inherit nil))
      (mu4e-cited-1-face (:foreground ,secondary :slant normal))
      (mu4e-cited-2-face (:foreground ,rose :slant normal))
      (mu4e-cited-3-face (:foreground ,secondary++ :slant normal))
      (mu4e-cited-4-face (:foreground ,tertiary :slant normal))
      (mu4e-cited-5-face (:foreground ,tertiary :slant normal))
      (mu4e-cited-6-face (:foreground ,secondary :slant normal))
      (mu4e-cited-7-face (:foreground ,rose :slant normal))
      (mu4e-ok-face (:foreground ,primary))
      (mu4e-view-contact-face (:inherit nil :foreground ,tertiary))
      (mu4e-view-link-face (:inherit link :foreground ,secondary))
      (mu4e-view-url-number-face (:inherit nil :foreground ,secondary++))
      (mu4e-view-attach-number-face (:inherit nil :foreground ,tertiary))
      (mu4e-highlight-face (:inherit highlight))
      (mu4e-title-face (:inherit nil :foreground ,primary))

      ;; SLIME
      (slime-highlight-edits-face (:weight bold))
      (slime-repl-input-face (:weight normal :underline nil))
      (slime-repl-prompt-face (:underline nil :weight bold :foreground ,rose))
      (slime-repl-result-face (:foreground ,primary))
      (slime-repl-output-face (:foreground ,secondary :background ,background))
      (slime-repl-inputed-output-face (:foreground ,background+++))

      ;; ;; vterm
      ;; (vterm-color-black (:background ,term-black :foreground ,term-black))
      ;; (vterm-color-secondary (:background ,secondary :foreground ,secondary))
      ;; (vterm-color-secondary++ (:background ,aqua :foreground ,aqua))
      ;; (vterm-color-default (:foreground unspecified :background unspecified :inherit default))
      ;; (vterm-color-primary (:background ,primary :foreground ,primary))
      ;; (vterm-color-magenta (:background ,purple :foreground ,purple))
      ;; (vterm-color-rose (:background ,rose :foreground ,rose))
      ;; (vterm-color-white (:background ,term-white :foreground ,term-white))
      ;; (vterm-color-tertiary (:background ,tertiary :foreground ,tertiary))
      ;; (vterm-color-underline (:underline t))
      ;; (vterm-color-inverse-video (:background ,background :inverse-video t))

      ))))

(defmacro hypalynx-theme-as (mode)
  "Apply the hypalynx theme in a scope with variables bound by the
given mode e.g `dark' or `light'"
  (let ((name (intern (concat "hypalynx-" (substring (symbol-name mode) 1))))
	(doc "A theme based on emerald & secondary++ colors with dark/light modes."))
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
	 `(beacon-color ,rose)
	 `(fci-rule-color ,background+)
         `(vc-annotate-color-map
           '((20  . ,rose)
             (40  . ,tertiary)
             (60  . ,tertiary)
             (80  . ,primary)
             (100 . ,secondary++)
             (120 . ,secondary)
             (140 . ,rose)
             (160 . ,rose)
             (180 . ,tertiary)
             (200 . ,tertiary)
             (220 . ,primary)
             (240 . ,secondary++)
             (260 . ,secondary)
             (280 . ,rose)
             (300 . ,rose)
             (320 . ,tertiary)
             (340 . ,tertiary)
             (360 . ,primary)))
         `(vc-annotate-very-old-color nil)
         `(vc-annotate-background nil)
         `(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
         `(ansi-color-names-vector (vector ,background ,rose ,primary ,tertiary ,secondary ,rose ,secondary++ ,foreground))
         `(window-divider-mode nil)
	 ))
       (provide-theme ',name))))

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

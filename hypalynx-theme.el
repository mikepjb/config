(deftheme hypalynx "A teal/green primary theme with dark/light modes")

;; use "describe-face" function to describe things that you want to theme.
;; teal/green actually => emerald/cyan

(defcustom hypalynx-transparent-background nil
  "Make transparent background in terminal. (Workaround)")

(defconst *hypalynx/fill-column-font*
  (cond ((x-list-fonts "Menlo") "Menlo")
	((x-list-fonts "Monospace") "Monospace")
	(t nil)))


(let ((class '((class color) (min-colors 89)))
      (neutral-50 "#fafafa")
      (neutral-100 "#f5f5f5")
      (neutral-200 "#e5e5e5")
      (neutral-300 "#d4d4d4")
      (neutral-400 "#a3a3a3")
      (neutral-500 "#737373")
      (neutral-600 "#525252")
      (neutral-700 "#404040")
      (neutral-800 "#262626")
      (neutral-900 "#171717")

      (stone-50 "#fafaf9")
      (stone-100 "#f5f5f4")
      (stone-200 "#e7e5e4")
      (stone-300 "#d6d3d1")
      (stone-400 "#a8a29e")
      (stone-500 "#78716c")
      (stone-600 "#57534e")
      (stone-700 "#44403c")
      (stone-800 "#292524")
      (stone-900 "#1c1917")

      (amber-100 "#fef3c7")
      (amber-200 "#fde68a")
      (amber-300 "#fcd34d")
      (amber-400 "#fbbf24")
      (amber-500 "#f59e0b")
      (amber-600 "#ca8a04")
      (amber-700 "#a16207")
      (amber-800 "#854d0e")
      (amber-900 "#78350f")

      (orange-400 "#fb923c")

      (emerald-50 "#ecfdf5")
      (emerald-100 "#d1fae5")
      (emerald-200 "#a7f3d0")
      (emerald-300 "#6ee7b7")
      (emerald-400 "#34d399")
      (emerald-500 "#10b981")
      (emerald-600 "#059669")
      (emerald-700 "#047857")
      (emerald-800 "#065f46")
      (emerald-900 "#064e3b")

      (lime-100 "#ecfccb")
      (lime-300 "#bef264")
      (lime-400 "#a3e635")
      (lime-500 "#84cc16")

      (red-100 "#fee2e2")
      (red-300 "#fca5a5")
      (red-400 "#f87171")
      (red-500 "#ef4444")
      (red-600 "#dc2626")
      (red-700 "#b91c1c")
      (red-800 "#991b1b")
      (red-900 "#7f1d1d")

      (blue-50 "#eff6ff")
      (blue-100 "#dbeafe")
      (blue-200 "#bfdbfe")
      (blue-300 "#93c5fd")
      (blue-400 "#60a5fa")
      (blue-500 "#3b82f6")
      (blue-600 "#2563eb")
      (blue-700 "#1d4ed8")
      (blue-800 "#1e40af")
      (blue-900 "#1e3a8a")

      (cyan-50 "#ecfeff")
      (cyan-100 "#cffafe")
      (cyan-200 "#a5f3fc")
      (cyan-300 "#67e8f9")
      (cyan-400 "#22d3ee")
      (cyan-500 "#06b6d4")
      (cyan-600 "#0891b2")
      (cyan-700 "#0e7490")
      (cyan-800 "#155e75")
      (cyan-900 "#164e63")

      (pink-300 "#f9a8d4")
      (pink-400 "#f472b6")
      (pink-500 "#ec4899")

      (violet-400 "#a78bfa")
      (violet-500 "#8b5cf6")
      (violet-600 "#7c3aed"))

  (custom-theme-set-faces
   'hypalynx

   ;;;;;;;;;;;;;;;;;;;;;
   ;; base theming    ;;
   ;;;;;;;;;;;;;;;;;;;;;

   `(default ((,class (:foreground ,neutral-900 :background ,neutral-100))))
   `(hl-line ((,class (:background ,neutral-200))))
   `(cursor ((,class (:background ,blue-800))))
   `(region ((,class (:background ,neutral-300))))
   `(highlight ((,class (:background ,neutral-200))))
   `(trailing-whitespace ((,class (:background ,red-300))))

   ;; N.B this is included to ensure we are using a font that has the
   ;; line glyph, it would be nicer to have a more concise function
   ;; for this.
   (if *hypalynx/fill-column-font*
       `(fill-column-indicator ((,class (:foreground ,neutral-300
						     :font ,*hypalynx/fill-column-font*))))
     `(fill-column-indicator ((,class (:foreground ,neutral-300)))))

   ;; UI theming
   `(line-number ((,class (:foreground ,neutral-500))))
   `(line-number-current-line ((,class (:foreground ,blue-800))))
   `(mode-line ((,class (:background ,neutral-200))))

   ;; text/buffer theming
   `(font-lock-comment-face ((,class (:foreground ,neutral-500 :italic t))))
   ;; clojure keywords
   `(font-lock-constant-face ((,class (:foreground ,blue-900))))
   ;; clojure ns names, kw name prefixes, require aliases
   `(font-lock-type-face ((,class (:foreground ,emerald-700))))
   ;; clojure core fns e.g ns/defn/def
   `(font-lock-keyword-face ((,class (:foreground ,blue-900))))
   `(font-lock-function-name-face ((,class (:foreground ,emerald-800))))
   `(font-lock-string-face ((,class (:foreground ,emerald-800))))
   `(font-lock-variable-name-face ((,class (:foreground ,emerald-800))))
   `(font-lock-warning-face ((,class (:foreground ,red-800))))

   ;; org theming
   `(org-todo ((,class (:foreground ,red-800 :bold t))))
   `(org-date ((,class (:foreground ,red-800))))
   `(org-ellipsis ((,class (:foreground ,red-800))))
   `(org-level-1 ((,class (:foreground ,emerald-900 :bold t))))
   `(org-level-2 ((,class (:foreground ,blue-900 :bold t))))
   `(org-agenda-structure ((,class (:foreground ,blue-900 :bold t))))
   `(org-scheduled-previously ((,class (:foreground ,blue-900))))
   `(org-imminent-deadline ((,class (:foreground ,red-900 :bold t))))
   `(org-scheduled-today ((,class (:foreground ,emerald-800))))
   `(org-scheduled ((,class (:foreground ,emerald-800))))
   `(org-habit-clear-face ((,class (:background ,blue-700 :foreground ,blue-300))))
   `(org-habit-clear-future-face ((,class (:background ,blue-100))))
   `(org-habit-alert-future-face ((,class (:background ,amber-200))))
   `(org-habit-overdue-face ((,class (:background ,red-600 :foreground ,red-300))))
   `(org-habit-overdue-futureface ((,class (:background ,red-100))))
   ))




;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'hypalynx)

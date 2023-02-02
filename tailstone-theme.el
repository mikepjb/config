(deftheme tailstone "A dark colour theme")

(defcustom tailstone-transparent-background nil
  "Make transparent background in terminal. (Workaround)")

(let ((class '((class color) (min-colors 89)))
      (tailstone-50 "#fafaf9")
      (tailstone-100 "#f5f5f4")
      (tailstone-200 "#e7e5e4")
      (tailstone-300 "#d6d3d1")
      (tailstone-400 "#a8a29e")
      (tailstone-500 "#78716c")
      (tailstone-600 "#57534e")
      (tailstone-700 "#44403c")
      (tailstone-800 "#292524")
      (tailstone-900 "#1c1917")

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

      (emerald-400 "#34d399")
      (emerald-500 "#10b981")
      (emerald-600 "#059669")

      (lime-100 "#ecfccb")
      (lime-300 "#bef264")
      (lime-400 "#a3e635")
      (lime-500 "#84cc16")

      (red-100 "#fee2e2")
      (red-300 "#fca5a5")
      (red-400 "#f87171")
      (red-500 "#ef4444")
      (red-600 "#dc2626")

      (cyan-100 "#cffafe")
      (cyan-200 "#a5f3fc")
      (cyan-300 "#67e8f9")
      (cyan-400 "#22d3ee")
      (cyan-500 "#06b6d4")
      (cyan-800 "#155e75")

      (pink-300 "#f9a8d4")
      (pink-400 "#f472b6")
      (pink-500 "#ec4899")

      (violet-400 "#a78bfa")
      (violet-500 "#8b5cf6")
      (violet-600 "#7c3aed"))

 (custom-theme-set-faces
   'tailstone
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,cyan-400 :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,violet-400 :underline t :weight normal))))

   ;; blue
   `(blue ((,class (:foreground ,cyan-400))))
   `(bold ((,class (:bold t))))
   `(border-glyph ((,class (nil))))

   `(buffers-tab ((,class (:background ,tailstone-900 :foreground ,tailstone-100))))

   ;;; basic coloring
   `(default ((,class (:foreground ,tailstone-100 :background ,tailstone-900))))
   `(cursor ((,class (:background ,amber-400))))
   `(escape-glyph-face ((,class (:foreground ,amber-400))))
   ;; `(fringe ((,class (:foreground ,radiant-fg :background ,radiant-bg+1))))
   `(header-line ((,class (:foreground ,amber-400
                                       :background ,tailstone-900
                                       :box (:line-width -1 :style released-button)))))
   `(highlight ((,class (:background ,tailstone-700))))

   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,amber-400))))
   `(compilation-enter-directory-face ((,class (:foreground ,lime-400))))
   `(compilation-error-face ((,class (:foreground ,red-400 :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,tailstone-100))))
   `(compilation-info-face ((,class (:foreground ,cyan-200))))
   `(compilation-info ((,class (:foreground ,lime-400 :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,lime-400))))
   `(compilation-line-face ((,class (:foreground ,amber-400))))
   `(compilation-line-number ((,class (:foreground ,amber-400))))
   `(compilation-message-face ((,class (:foreground ,cyan-400))))
   `(compilation-warning-face ((,class (:foreground ,amber-400 :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,tailstone-900 :background ,pink-300))))
   `(grep-error-face ((,class (:foreground ,red-400 :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,tailstone-900 :background ,red-400))))
   `(grep-match-face ((,class (:foreground ,tailstone-900 :background ,pink-300))))
   `(match ((,class (:background ,tailstone-900 :foreground ,pink-300))))

   ;; faces used by isearch
   
   `(isearch ((,class (:foreground ,tailstone-900 :background ,tailstone-300))))
   `(isearch-fail ((,class (:background ,red-400))))

   `(lazy-highlight ((,class (:foreground ,tailstone-900 :background ,amber-400))))
   `(query-replace ((,class (:background ,tailstone-700))))
   `(Highline-face ((,class (:background ,lime-500))))
   `(left-margin ((,class (nil))))
   `(toolbar ((,class (nil))))
   `(text-cursor ((,class (:background ,amber-400 :foreground ,tailstone-900))))

   `(menu ((,class (:foreground ,tailstone-100 :background ,tailstone-900))))
   `(minibuffer-prompt ((,class (:foreground ,cyan-500 :background ,tailstone-900))))
   `(mode-line
     ((,class (:foreground ,amber-400
                           :background ,tailstone-800
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   ;; `(mode-line-buffer-id ((,class (:foreground ,radiant-yellow :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,tailstone-500
                           :background ,tailstone-800
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(region ((,class (:background ,tailstone-700))))
   `(secondary-selection ((,class (:background ,tailstone-700))))
   `(trailing-whitespace ((,class (:background ,red-400))))
   `(vertical-border ((,class (:foreground ,tailstone-700 :background ,tailstone-900))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,cyan-500))))
   `(font-lock-comment-face ((,class (:foreground ,tailstone-500 :italic t))))
   ;; `(font-lock-comment-delimiter-face ((,class (:foreground ,radiant-green)))) 
   `(font-lock-constant-face ((,class (:foreground ,cyan-500))))
   ;; `(font-lock-doc-face ((,class (:foreground ,radiant-green+1))))
   `(font-lock-doc-face ((,class (:foreground ,amber-400))))
   `(font-lock-function-name-face ((,class (:foreground ,cyan-500))))
   `(font-lock-keyword-face ((,class (:foreground ,emerald-400))))
   ;; `(font-lock-negation-char-face ((,class (:foreground ,radiant-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,tailstone-700))))
   `(font-lock-string-face ((,class (:foreground ,amber-400))))
   `(font-lock-type-face ((,class (:foreground ,red-400))))
   `(font-lock-variable-name-face ((,class (:foreground ,lime-400))))
   `(font-lock-warning-face ((,class (:foreground ,pink-300))))
   `(font-lock-reference-face ((,class (:foreground ,tailstone-700))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,amber-400))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,red-400))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   `(gui-element ((,class (:background ,tailstone-700 :foreground ,cyan-500))))

   ;;; external

   ;; full-ack
   `(ack-separator ((,class (:foreground ,tailstone-100))))
   `(ack-file ((,class (:foreground ,cyan-500))))
   `(ack-line ((,class (:foreground ,amber-400))))
   `(ack-match ((,class (:foreground ,amber-400 :background ,tailstone-900 :weigth bold))))

   ;; auto-complete
   `(ac-completion-face ((,class (:background ,tailstone-700 :underline t))))
   `(ac-candidate-face ((,class (:background ,tailstone-700 :foreground ,tailstone-900))))
   `(ac-selection-face ((,class (:background ,pink-300 :foreground ,tailstone-900))))
   `(popup-tip-face ((,class (:background ,tailstone-700 :foreground ,tailstone-100))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,tailstone-700))))
   `(popup-scroll-bar-background-face ((,class (:background ,tailstone-700))))
   `(popup-isearch-match ((,class (:background ,tailstone-900 :foreground ,pink-300))))

   `(window-number-face ((,class (:background ,tailstone-700 :foreground ,cyan-400))))

   ;; company-mode
   `(company-tooltip ((,class (:background ,tailstone-800 :foreground ,tailstone-100))))
   `(company-tooltip-common ((,class (:inherit company-tooltip :foreground ,cyan-400))))
   `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection :foreground ,cyan-400))))
   `(company-tooltip-selection ((,class (:foreground ,tailstone-100 :background ,tailstone-900))))
   `(company-tooltip-annotation ((,class (:inherit company-tooltip :foreground ,tailstone-700))))
   `(company-scrollbar-fg ((,class (:background ,tailstone-700))))
   `(company-scrollbar-bg ((,class (:background ,tailstone-700))))
   `(company-preview ((,class (:foreground ,tailstone-100 :background ,tailstone-800))))
   `(company-preview-common ((,class (:foreground ,tailstone-100 :background ,tailstone-800))))

   ;; diff
   `(diff-added ((,class (:foreground ,lime-500))))
   `(diff-changed ((,class (:foreground ,amber-400))))
   `(diff-removed ((,class (:foreground ,red-400))))
   `(diff-header ((,class (:background ,tailstone-700))))
   `(diff-file-header ((,class (:background ,tailstone-700 :foreground ,tailstone-100 :bold t))))

   ;; tab-bar
   `(tab-bar ((t (:background ,tailstone-800
                              :box (:line-width -1 :style released-button)))))
   `(tab-bar-tab ((t (:foreground ,amber-400
                                  :background ,tailstone-800
                                  :weight bold
                                  :box (:line-width -1 :style released-button)))))
   `(tab-bar-tab-inactive ((t (:foreground ,tailstone-300
                                           :background ,tailstone-800
                                           :box (:line-width -1 :style released-button)))))

;;    ;; ediff
;;    `(ediff-current-diff-Ancestor ((,class (:foreground ,radiant-fg :background ,radiant-pink))))
;;    `(ediff-current-diff-A ((,class (:foreground ,radiant-fg :background ,radiant-bg-05))))
;;    `(ediff-current-diff-B ((,class (:foreground ,radiant-fg :background ,radiant-bg+1))))
;;    `(ediff-current-diff-C ((,class (:foreground ,radiant-fg :background ,radiant-bg+2))))
;;    `(ediff-even-diff-Ancestor ((,class (:foreground ,radiant-white :background ,radiant-bg-05))))
;;    `(ediff-even-diff-A ((,class (:foreground ,radiant-white :background ,radiant-bg+1))))
;;    `(ediff-even-diff-B ((,class (:foreground ,radiant-white :background ,radiant-bg+2))))
;;    `(ediff-even-diff-C ((,class (:foreground ,radiant-white :background ,radiant-bg+3))))
;;    `(ediff-fine-diff-Ancestor ((,class (:foreground ,radiant-black :background ,radiant-pink))))
;;    `(ediff-fine-diff-A ((,class (:foreground ,radiant-black :background ,radiant-blue-5))))
;;    `(ediff-fine-diff-B ((,class (:foreground ,radiant-black :background ,radiant-blue-5))))
;;    `(ediff-fine-diff-C ((,class (:foreground ,radiant-black :background ,radiant-blue-5))))
;;    `(ediff-odd-diff-Ancestor ((,class (:foreground ,radiant-black :background ,radiant-gray-2))))
;;    `(ediff-odd-diff-A ((,class (:foreground ,radiant-black :background ,radiant-gray-3))))
;;    `(ediff-odd-diff-B ((,class (:foreground ,radiant-black :background ,radiant-gray-4))))
;;    `(ediff-odd-diff-C ((,class (:foreground ,radiant-black :background ,radiant-gray))))

;;    ;; ert
;;    `(ert-test-result-expected ((,class (:foreground ,radiant-green+4 :background ,radiant-bg))))
;;    `(ert-test-result-unexpected ((,class (:foreground ,radiant-red :background ,radiant-bg))))

;;    ;; eshell
;;    `(eshell-prompt ((,class (:foreground ,radiant-blue-5 :weight bold))))
;;    `(eshell-ls-archive ((,class (:foreground ,radiant-magenta :weight bold))))
;;    `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
;;    `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
;;    `(eshell-ls-directory ((,class (:foreground ,radiant-blue+1 :weight bold))))
;;    `(eshell-ls-executable ((,class (:foreground ,radiant-red+1 :weight bold))))
;;    `(eshell-ls-unreadable ((,class (:foreground ,radiant-fg))))
;;    `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
;;    `(eshell-ls-product ((,class (:inherit font-lock-doc))))
;;    `(eshell-ls-special ((,class (:foreground ,radiant-yellow :weight bold))))
;;    `(eshell-ls-symlink ((,class (:foreground ,radiant-cyan :weight bold))))

;;    ;; flymake
;;    `(flymake-errline ((,class (:foreground ,radiant-red-1 :weight bold :underline t))))
;;    `(flymake-warnline ((,class (:foreground ,radiant-yellow-1 :weight bold :underline t))))

;;    ;; flyspell
;;    `(flyspell-duplicate ((,class (:foreground ,radiant-yellow-1 :weight bold :underline t))))
;;    `(flyspell-incorrect ((,class (:foreground ,radiant-orange-2 :weight bold :underline t))))

;;    ;; erc
;;    `(erc-action-face ((,class (:inherit erc-default-face))))
;;    `(erc-bold-face ((,class (:weight bold))))
;;    `(erc-current-nick-face ((,class (:foreground ,radiant-blue :weight bold))))
;;    `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
;;    `(erc-default-face ((,class (:foreground ,radiant-fg))))
;;    `(erc-direct-msg-face ((,class (:inherit erc-default))))
;;    `(erc-error-face ((,class (:inherit font-lock-warning))))
;;    `(erc-fool-face ((,class (:inherit erc-default))))
;;    `(erc-highlight-face ((,class (:inherit hover-highlight))))
;;    `(erc-input-face ((,class (:foreground ,radiant-yellow))))
;;    `(erc-keyword-face ((,class (:foreground ,radiant-blue :weight bold))))
;;    `(erc-nick-default-face ((,class (:foreground ,radiant-yellow :weight bold))))
;;    `(erc-my-nick-face ((,class (:foreground ,radiant-red :weigth bold))))
;;    `(erc-nick-msg-face ((,class (:inherit erc-default))))
;;    `(erc-notice-face ((,class (:foreground ,radiant-green))))
;;    `(erc-pal-face ((,class (:foreground ,radiant-orange :weight bold))))
;;    `(erc-prompt-face ((,class (:foreground ,radiant-orange :background ,radiant-bg :weight bold))))
;;    `(erc-timestamp-face ((,class (:foreground ,radiant-green+1))))
;;    `(erc-underline-face ((t (:underline t))))

;;    ;; gnus
;;    `(gnus-group-mail-1 ((,class (:bold t :inherit gnus-group-mail-1-empty))))
;;    `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
;;    `(gnus-group-mail-2 ((,class (:bold t :inherit gnus-group-mail-2-empty))))
;;    `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
;;    `(gnus-group-mail-3 ((,class (:bold t :inherit gnus-group-mail-3-empty))))
;;    `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
;;    `(gnus-group-mail-4 ((,class (:bold t :inherit gnus-group-mail-4-empty))))
;;    `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-news-4-empty))))
;;    `(gnus-group-mail-5 ((,class (:bold t :inherit gnus-group-mail-5-empty))))
;;    `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-news-5-empty))))
;;    `(gnus-group-mail-6 ((,class (:bold t :inherit gnus-group-mail-6-empty))))
;;    `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-news-6-empty))))
;;    `(gnus-group-mail-low ((,class (:bold t :inherit gnus-group-mail-low-empty))))
;;    `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
;;    `(gnus-group-news-1 ((,class (:bold t :inherit gnus-group-news-1-empty))))
;;    `(gnus-group-news-2 ((,class (:bold t :inherit gnus-group-news-2-empty))))
;;    `(gnus-group-news-3 ((,class (:bold t :inherit gnus-group-news-3-empty))))
;;    `(gnus-group-news-4 ((,class (:bold t :inherit gnus-group-news-4-empty))))
;;    `(gnus-group-news-5 ((,class (:bold t :inherit gnus-group-news-5-empty))))
;;    `(gnus-group-news-6 ((,class (:bold t :inherit gnus-group-news-6-empty))))
;;    `(gnus-group-news-low ((,class (:bold t :inherit gnus-group-news-low-empty))))
;;    `(gnus-header-content ((,class (:inherit message-header-other))))
;;    `(gnus-header-from ((,class (:inherit message-header-from))))
;;    `(gnus-header-name ((,class (:inherit message-header-name))))
;;    `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
;;    `(gnus-header-subject ((,class (:inherit message-header-subject))))
;;    `(gnus-summary-cancelled ((,class (:foreground ,radiant-orange))))
;;    `(gnus-summary-high-ancient ((,class (:foreground ,radiant-blue))))
;;    `(gnus-summary-high-read ((,class (:foreground ,radiant-green :weight bold))))
;;    `(gnus-summary-high-ticked ((,class (:foreground ,radiant-orange :weight bold))))
;;    `(gnus-summary-high-unread ((,class (:foreground ,radiant-fg :weight bold))))
;;    `(gnus-summary-low-ancient ((,class (:foreground ,radiant-blue))))
;;    `(gnus-summary-low-read ((t (:foreground ,radiant-green))))
;;    `(gnus-summary-low-ticked ((,class (:foreground ,radiant-orange :weight bold))))
;;    `(gnus-summary-low-unread ((,class (:foreground ,radiant-fg))))
;;    `(gnus-summary-normal-ancient ((,class (:foreground ,radiant-blue+1))))
;;    `(gnus-summary-normal-read ((,class (:foreground ,radiant-green))))
;;    `(gnus-summary-normal-ticked ((,class (:foreground ,radiant-orange :weight bold))))
;;    `(gnus-summary-normal-unread ((,class (:foreground ,radiant-fg))))
;;    `(gnus-summary-selected ((,class (:foreground ,radiant-yellow :weight bold))))
;;    `(gnus-cite-1 ((,class (:foreground ,radiant-yellow-2))))
;;    `(gnus-cite-10 ((,class (:foreground ,radiant-yellow-1))))
;;    `(gnus-cite-11 ((,class (:foreground ,radiant-yellow))))
;;    `(gnus-cite-2 ((,class (:foreground ,radiant-blue-1))))
;;    `(gnus-cite-3 ((,class (:foreground ,radiant-blue-2))))
;;    `(gnus-cite-4 ((,class (:foreground ,radiant-green+2))))
;;    `(gnus-cite-5 ((,class (:foreground ,radiant-green+1))))
;;    `(gnus-cite-6 ((,class (:foreground ,radiant-green))))
;;    `(gnus-cite-7 ((,class (:foreground ,radiant-red))))
;;    `(gnus-cite-8 ((,class (:foreground ,radiant-red-1))))
;;    `(gnus-cite-9 ((,class (:foreground ,radiant-red-2))))
;;    `(gnus-group-news-1-empty ((,class (:foreground ,radiant-yellow))))
;;    `(gnus-group-news-2-empty ((,class (:foreground ,radiant-green+3))))
;;    `(gnus-group-news-3-empty ((,class (:foreground ,radiant-green+1))))
;;    `(gnus-group-news-4-empty ((,class (:foreground ,radiant-blue-2))))
;;    `(gnus-group-news-5-empty ((,class (:foreground ,radiant-blue-3))))
;;    `(gnus-group-news-6-empty ((,class (:foreground ,radiant-bg+2))))
;;    `(gnus-group-news-low-empty ((,class (:foreground ,radiant-bg+2))))
;;    `(gnus-signature ((,class (:foreground ,radiant-yellow))))
;;    `(gnus-x ((,class (:background ,radiant-fg :foreground ,radiant-bg))))

;;    ;; helm
;;    `(helm-header
;;      ((,class (:foreground ,radiant-green
;;                            :background ,radiant-bg
;;                            :underline nil
;;                            :box nil))))
;;    `(helm-source-header
;;      ((,class (:foreground ,radiant-yellow
;;                            :background ,radiant-bg-1
;;                            :underline nil
;;                            :weight bold
;;                            :box (:line-width -1 :style released-button)))))
;;    `(helm-selection ((,class (:background ,radiant-bg+1 :underline nil))))
;;    `(helm-selection-line ((,class (:background ,radiant-bg+1))))
;;    `(helm-visible-mark ((,class (:foreground ,radiant-bg :background ,radiant-yellow-2))))
;;    `(helm-candidate-number ((,class (:foreground ,radiant-green+4 :background ,radiant-bg-1))))

   ;; hl-line-mode
   `(hl-sexp-face ((,class (:background ,tailstone-800))))
   `(hl-line ((,class (:background ,tailstone-800))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,red-400 :background ,tailstone-900))))
   `(ido-only-match ((,class (:foreground ,red-400 :background ,tailstone-900))))
   `(ido-subdir ((,class (:foreground ,tailstone-700 :backgroun ,tailstone-900))))
   `(ido-indicator ((,class (:foreground ,tailstone-900 :background ,amber-400))))

;;    ;; js2-mode
;;    `(js2-warning-face ((,class (:underline ,radiant-orange))))
;;    `(js2-error-face ((,class (:foreground ,radiant-red :weight bold))))
;;    `(js2-jsdoc-tag-face ((,class (:foreground ,radiant-green-1))))
;;    `(js2-jsdoc-type-face ((,class (:foreground ,radiant-green+2))))
;;    `(js2-jsdoc-value-face ((,class (:foreground ,radiant-green+3))))
;;    `(js2-function-param-face ((,class (:foreground ,radiant-green+3))))
;;    `(js2-external-variable-face ((,class (:foreground ,radiant-orange))))

;;    ;; jabber-mode
;;    `(jabber-roster-user-away ((,class (:foreground ,radiant-green+2))))
;;    `(jabber-roster-user-online ((,class (:foreground ,radiant-blue-1))))
;;    `(jabber-roster-user-dnd ((,class (:foreground ,radiant-red+1))))
;;    `(jabber-rare-time-face ((,class (:foreground ,radiant-green+1))))
;;    `(jabber-chat-prompt-local ((,class (:foreground ,radiant-blue-1))))
;;    `(jabber-chat-prompt-foreign ((,class (:foreground ,radiant-red+1))))
;;    `(jabber-activity-face((,class (:foreground ,radiant-red+1))))
;;    `(jabber-activity-personal-face ((,class (:foreground ,radiant-blue+1))))
;;    `(jabber-title-small ((,class (:height 1.1 :weight bold))))
;;    `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
;;    `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,tailstone-700 :background ,tailstone-900))))

   ;; column-number/line-number mode
   `(line-number ((,class (:foreground ,tailstone-600))))
   `(line-number-current-line ((,class (:foreground ,amber-400))))

   ;; magit
   ;; magit headings and diffs
   `(magit-section-highlight ((t (:background ,tailstone-800))))
   `(magit-section-heading ((t (:foreground ,cyan-500 :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,red-400 :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,tailstone-800  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,tailstone-800
                                            :foreground ,cyan-500 :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,tailstone-900))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,red-400))))
   `(magit-diff-hunk-heading-selection ((t (:background ,red-400
                                            :foreground ,cyan-500))))
   `(magit-diff-lines-heading          ((t (:background ,cyan-500
                                            :foreground ,red-400))))
   `(magit-diff-added                  ((t (:foreground ,lime-500))))
   `(magit-diff-added-highlight        ((t (:inherit magit-diff-added :weight bold))))
   `(magit-diff-removed                ((t (:foreground ,red-500))))
   `(magit-diff-removed-highlight      ((t (:inherit magit-diff-removed :weight bold))))
   `(magit-diff-context                ((t (:foreground ,tailstone-600))))
   `(magit-diff-context-highlight      ((t (:inherit magit-diff-context :weight bold))))
   `(magit-diffstat-added   ((t (:inherit magit-diff-added))))
   `(magit-diffstat-removed ((t (:inherit magit-diff-removed))))
   ;; magit popup
   `(magit-popup-heading             ((t (:foreground ,tailstone-800  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,cyan-200 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,cyan-300   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,tailstone-100    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,cyan-500  :weight bold))))
   ;; ;; magit process
   `(magit-process-ok    ((t (:foreground ,lime-500  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,pink-300    :weight bold))))
   ;; ;; magit log
   `(magit-log-author    ((t (:foreground ,pink-300))))
   `(magit-log-date      ((t (:foreground ,tailstone-600))))
   `(magit-log-graph     ((t (:foreground ,tailstone-200))))
   ;; ;; magit sequence
   `(magit-sequence-pick ((t (:foreground ,pink-300))))
   `(magit-sequence-stop ((t (:foreground ,lime-100))))
   `(magit-sequence-part ((t (:foreground ,pink-300))))
   `(magit-sequence-head ((t (:foreground ,cyan-200))))
   `(magit-sequence-drop ((t (:foreground ,amber-400))))
   `(magit-sequence-done ((t (:foreground ,tailstone-600))))
   `(magit-sequence-onto ((t (:foreground ,tailstone-600))))
   ;; ;; magit bisect
   `(magit-bisect-good ((t (:foreground ,lime-400))))
   `(magit-bisect-skip ((t (:foreground ,pink-300))))
   `(magit-bisect-bad  ((t (:foreground ,amber-400))))
   ;; ;; magit blame
   `(magit-blame-heading ((t (:background ,tailstone-700 :foreground ,lime-400))))
   `(magit-blame-hash    ((t (:background ,tailstone-700 :foreground ,lime-400))))
   `(magit-blame-name    ((t (:background ,tailstone-700 :foreground ,pink-300))))
   `(magit-blame-date    ((t (:background ,tailstone-700 :foreground ,amber-400))))
   `(magit-blame-summary ((t (:background ,tailstone-700 :foreground ,cyan-400
                                          :weight bold))))
   ;; ;; magit references etc
   `(magit-dimmed         ((t (:foreground ,tailstone-500))))
   `(magit-hash           ((t (:foreground ,tailstone-500))))
   `(magit-tag            ((t (:foreground ,pink-300 :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,lime-300  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,cyan-500   :weight bold))))
   `(magit-branch-current ((t (:foreground ,lime-500   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,cyan-500   :weight bold))))
   `(magit-refname        ((t (:background ,tailstone-700 :foreground ,tailstone-100 :weight bold))))
   `(magit-refname-stash  ((t (:background ,tailstone-700 :foreground ,tailstone-100 :weight bold))))
   `(magit-refname-wip    ((t (:background ,tailstone-700 :foreground ,tailstone-100 :weight bold))))
   `(magit-signature-good      ((t (:foreground ,lime-500))))
   `(magit-signature-bad       ((t (:foreground ,red-400))))
   `(magit-signature-untrusted ((t (:foreground ,amber-400))))
   `(magit-cherry-unmatched    ((t (:foreground ,cyan-500))))
   `(magit-cherry-equivalent   ((t (:foreground ,pink-300))))
   `(magit-reflog-commit       ((t (:foreground ,lime-500))))
   `(magit-reflog-amend        ((t (:foreground ,pink-300))))
   `(magit-reflog-merge        ((t (:foreground ,lime-500))))
   `(magit-reflog-checkout     ((t (:foreground ,cyan-500))))
   `(magit-reflog-reset        ((t (:foreground ,red-400))))
   `(magit-reflog-rebase       ((t (:foreground ,pink-300))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,lime-500))))
   `(magit-reflog-remote       ((t (:foreground ,cyan-500))))
   `(magit-reflog-other        ((t (:foreground ,cyan-500))))
   
;;    `(eval-sexp-fu-flash ((,class (:background ,radiant-gray-8 :foreground ,radiant-pink-2))))

;;    ;; message-mode
;;    `(message-cited-text ((,class (:inherit font-lock-comment))))
;;    `(message-header-name ((,class (:foreground ,radiant-blue-5))))
;;    `(message-header-other ((,class (:foreground ,radiant-green))))
;;    `(message-header-to ((,class (:foreground ,radiant-pink-1 :weight bold))))
;;    `(message-header-from ((,class (:foreground ,radiant-yellow :weight bold))))
;;    `(message-header-cc ((,class (:foreground ,radiant-yellow :weight bold))))
;;    `(message-header-newsgroups ((,class (:foreground ,radiant-yellow :weight bold))))
;;    `(message-header-subject ((,class (:foreground ,radiant-orange :weight bold))))
;;    `(message-header-xheader ((,class (:foreground ,radiant-green))))
;;    `(message-mml ((,class (:foreground ,radiant-yellow :weight bold))))
;;    `(message-separator ((,class (:inherit font-lock-comment))))

;;    ;; mew
;;    `(mew-face-header-subject ((,class (:foreground ,radiant-orange))))
;;    `(mew-face-header-from ((,class (:foreground ,radiant-yellow))))
;;    `(mew-face-header-date ((,class (:foreground ,radiant-green))))
;;    `(mew-face-header-to ((,class (:foreground ,radiant-red))))
;;    `(mew-face-header-key ((,class (:foreground ,radiant-green))))
;;    `(mew-face-header-private ((,class (:foreground ,radiant-green))))
;;    `(mew-face-header-important ((,class (:foreground ,radiant-blue))))
;;    `(mew-face-header-marginal ((,class (:foreground ,radiant-fg :weight bold))))
;;    `(mew-face-header-warning ((,class (:foreground ,radiant-red))))
;;    `(mew-face-header-xmew ((,class (:foreground ,radiant-green))))
;;    `(mew-face-header-xmew-bad ((,class (:foreground ,radiant-red))))
;;    `(mew-face-body-url ((,class (:foreground ,radiant-orange))))
;;    `(mew-face-body-comment ((,class (:foreground ,radiant-fg :slant italic))))
;;    `(mew-face-body-cite1 ((,class (:foreground ,radiant-green))))
;;    `(mew-face-body-cite2 ((,class (:foreground ,radiant-blue))))
;;    `(mew-face-body-cite3 ((,class (:foreground ,radiant-orange))))
;;    `(mew-face-body-cite4 ((,class (:foreground ,radiant-yellow))))
;;    `(mew-face-body-cite5 ((,class (:foreground ,radiant-red))))
;;    `(mew-face-mark-review ((,class (:foreground ,radiant-blue))))
;;    `(mew-face-mark-escape ((,class (:foreground ,radiant-green))))
;;    `(mew-face-mark-delete ((,class (:foreground ,radiant-red))))
;;    `(mew-face-mark-unlink ((,class (:foreground ,radiant-yellow))))
;;    `(mew-face-mark-refile ((,class (:foreground ,radiant-green))))
;;    `(mew-face-mark-unread ((,class (:foreground ,radiant-red-2))))
;;    `(mew-face-eof-message ((,class (:foreground ,radiant-green))))
;;    `(mew-face-eof-part ((,class (:foreground ,radiant-yellow))))

;;    ;; nav
;;    `(nav-face-heading ((,class (:foreground ,radiant-yellow))))
;;    `(nav-face-button-num ((,class (:foreground ,radiant-cyan))))
;;    `(nav-face-dir ((,class (:foreground ,radiant-green))))
;;    `(nav-face-hdir ((,class (:foreground ,radiant-red))))
;;    `(nav-face-file ((,class (:foreground ,radiant-fg))))
;;    `(nav-face-hfile ((,class (:foreground ,radiant-red-4))))

;;    ;; mumamo
;;    `(mumamo-background-chunk-major ((,class (:background ,radiant-black))))
;;    `(mumamo-background-chunk-submode1 ((,class (:background ,radiant-black))))
;;    `(mumamo-background-chunk-submode2 ((,class (:background ,radiant-bg+2))))
;;    `(mumamo-background-chunk-submode3 ((,class (:background ,radiant-bg+3))))
;;    `(mumamo-background-chunk-submode4 ((,class (:background ,radiant-bg+1))))

;;    ;; org-mode
;;    `(org-document-title ((,class (:foreground ,radiant-blue-3 :background ,radiant-black :weight bold :height 1.5))))
;;    `(org-document-info ((,class (:foreground ,radiant-blue-3 :background ,radiant-black :weight bold))))
;;    `(org-document-info-keyword ((,class (:foreground ,radiant-gray-2 :background ,radiant-black))))
;;    `(org-agenda-date-today
;;      ((,class (:foreground ,radiant-orange-2 :slant italic :weight bold))) t)
;;    `(org-agenda-structure
;;      ((,class (:inherit font-lock-comment-face))))
;;    `(org-archived ((,class (:slant italic))))
;;    `(org-checkbox ((,class (:background ,radiant-gray-2 :foreground ,radiant-black
;;                                    :box (:line-width 1 :style released-button)))))
;;    `(org-date ((,class (:foreground ,radiant-blue-7 :underline t))))
;;    `(org-done ((,class (:bold t :weight bold :foreground ,radiant-green
;;                               :box (:line-width 1 :style none)))))
;;    `(org-todo ((,class (:bold t :foreground ,radiant-orange :weight bold
;;                               :box (:line-width 1 :style none)))))
;;    `(org-level-1 ((,class (:foreground ,radiant-pink-1))))
;;    `(org-level-2 ((,class (:foreground ,radiant-yellow))))
;;    `(org-level-3 ((,class (:foreground ,radiant-blue-5))))
;;    `(org-level-4 ((,class (:foreground ,radiant-green))))
;;    `(org-level-5 ((,class (:foreground ,radiant-orange))))
;;    `(org-level-6 ((,class (:foreground ,radiant-pink))))
;;    `(org-level-7 ((,class (:foreground ,radiant-green+3))))
;;    `(org-level-8 ((,class (:foreground ,radiant-blue-1))))
;;    `(org-link ((,class (:foreground ,radiant-blue-6 :underline t))))
;;    `(org-tag ((,class (:bold t :weight bold))))
;;    `(org-column ((,class (:background ,radiant-yellow-3 :foreground ,radiant-black))))
;;    `(org-column-title ((,class (:background ,radiant-bg-1 :underline t :weight bold))))
;;    `(org-block ((,class (:foreground ,radiant-fg :background ,radiant-bg-05))))
;;    `(org-block-begin-line 
;;      ((,class (:foreground "#008ED1" :background ,radiant-bg-1))))
;;    `(org-block-background ((,class (:background ,radiant-bg-05))))
;;    `(org-block-end-line 
;;      ((,class (:foreground "#008ED1" :background ,radiant-bg-1))))

;;    ;; `(org-deadline-announce ((,class (:foreground ,radiant-red-1))))
;;    ;; `(org-scheduled ((,class (:foreground ,radiant-green+4))))
;;    ;; `(org-scheduled-previously ((,class (:foreground ,radiant-red-4))))
;;    ;; `(org-scheduled-today ((,class (:foreground ,radiant-blue+1))))
;;    ;; `(org-special-keyword ((,class (:foreground ,radiant-yellow-1))))
;;    ;; `(org-table ((,class (:foreground ,radiant-green+2))))
;;    ;; `(org-time-grid ((,class (:foreground ,radiant-orange))))
;;    ;; `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
;;    ;; `(org-warning ((,class (:bold t :foreground ,radiant-red :weight bold :underline nil))))
;;    ;; `(org-formula ((,class (:foreground ,radiant-yellow-2))))
;;    ;; `(org-headline-done ((,class (:foreground ,radiant-green+3))))
;;    ;; `(org-hide ((,class (:foreground ,radiant-bg-1))))

;;    ;; outline
;;    `(outline-8 ((,class (:inherit default))))
;;    `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
;;    `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
;;    `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
;;    `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
;;    `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
;;    `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
;;    `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

;;    ;; racket-mode
;;    `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
;;    `(racket-selfeval-face ((t (:inherit font-lock-type-face))))

   ;; ;; rainbow-delimiters
   ;; `(rainbow-delimiters-depth-1-face ((,class (:foreground ,tailstone-red-1))))
   ;; `(rainbow-delimiters-depth-2-face ((,class (:foreground ,tailstone-magenta-1))))
   ;; `(rainbow-delimiters-depth-3-face ((,class (:foreground ,tailstone-blue-1))))
   ;; `(rainbow-delimiters-depth-4-face ((,class (:foreground ,tailstone-green-1))))
   ;; `(rainbow-delimiters-depth-5-face ((,class (:foreground ,tailstone-yellow-1))))
   ;; `(rainbow-delimiters-depth-6-face ((,class (:foreground ,tailstone-red-2))))
   ;; `(rainbow-delimiters-depth-7-face ((,class (:foreground ,tailstone-magenta-2))))
   ;; `(rainbow-delimiters-depth-8-face ((,class (:foreground ,tailstone-blue-2))))
   ;; `(rainbow-delimiters-depth-9-face ((,class (:foreground ,tailstone-green-2))))
   ;; `(rainbow-delimiters-depth-10-face ((,class (:foreground ,tailstone-yellow-2))))
   ;; `(rainbow-delimiters-depth-11-face ((,class (:foreground ,tailstone-blue-1))))
   ;; `(rainbow-delimiters-depth-12-face ((,class (:foreground ,tailstone-red-1))))

;;    ;; rpm-mode
;;    `(rpm-spec-dir-face ((,class (:foreground ,radiant-green))))
;;    `(rpm-spec-doc-face ((,class (:foreground ,radiant-green))))
;;    `(rpm-spec-ghost-face ((,class (:foreground ,radiant-red))))
;;    `(rpm-spec-macro-face ((,class (:foreground ,radiant-yellow))))
;;    `(rpm-spec-obsolete-tag-face ((,class (:foreground ,radiant-red))))
;;    `(rpm-spec-package-face ((,class (:foreground ,radiant-red))))
;;    `(rpm-spec-section-face ((,class (:foreground ,radiant-yellow))))
;;    `(rpm-spec-tag-face ((,class (:foreground ,radiant-blue))))
;;    `(rpm-spec-var-face ((,class (:foreground ,radiant-red))))

;;    ;; rst-mode
;;    `(rst-level-1-face ((,class (:foreground ,radiant-orange))))
;;    `(rst-level-2-face ((,class (:foreground ,radiant-green+1))))
;;    `(rst-level-3-face ((,class (:foreground ,radiant-blue-1))))
;;    `(rst-level-4-face ((,class (:foreground ,radiant-yellow-2))))
;;    `(rst-level-5-face ((,class (:foreground ,radiant-cyan))))
;;    `(rst-level-6-face ((,class (:foreground ,radiant-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,red-400 :background ,tailstone-900))))
   `(show-paren-match ((,class (:foreground ,amber-400))))

;;    `(naeu-green-face ((,class (:foreground ,radiant-green :background ,radiant-black))))
;;    `(naeu-pink-face ((,class (:foreground ,radiant-pink-1 :background ,radiant-black))))
;;    `(naeu-blue-face ((,class (:foreground ,radiant-blue-1 :background ,radiant-black))))
;;    `(naeu-orange-face ((,class (:foreground ,radiant-yellow-1 :background ,radiant-black))))
;;    `(naeu-red-face ((,class (:foreground ,radiant-orange :background ,radiant-black))))
;;    `(naeu-grey-face ((,class (:foreground ,radiant-gray-7 :background ,radiant-black))))

;;    ;; SLIME
;;    `(slime-repl-inputed-output-face ((,class (:foreground ,radiant-red))))

;;   ;;; ansi-term
;;    `(term-color-black ((,class (:foreground ,radiant-bg
;;                                             :background ,radiant-bg-1))))
;;    `(term-color-red ((,class (:foreground ,radiant-red-2
;;                                           :background ,radiant-red-4))))
;;    `(term-color-green ((,class (:foreground ,radiant-green
;;                                             :background ,radiant-green+2))))
;;    `(term-color-yellow ((,class (:foreground ,radiant-orange
;;                                              :background ,radiant-yellow))))
;;    `(term-color-blue ((,class (:foreground ,radiant-blue-1
;;                                            :background ,radiant-blue-4))))
;;    `(term-color-magenta ((,class (:foreground ,radiant-magenta
;;                                               :background ,radiant-red))))
;;    `(term-color-cyan ((,class (:foreground ,radiant-cyan
;;                                            :background ,radiant-blue))))
;;    `(term-color-white ((,class (:foreground ,radiant-fg
;;                                             :background ,radiant-bg-1))))
;;    `(term-default-fg-color ((,class (:inherit term-color-white))))
;;    `(term-default-bg-color ((,class (:inherit term-color-black))))

;;    ;; volatile-highlights
;;    `(vhl/default-face ((,class (:background ,radiant-gray-5))))

;;    `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,radiant-pink-1 :background ,radiant-black))))

;;    ;; whitespace-mode
;;    `(whitespace-space ((,class (:background ,radiant-bg :foreground ,radiant-bg+1))))
;;    `(whitespace-hspace ((,class (:background ,radiant-bg :foreground ,radiant-bg+1))))
;;    `(whitespace-tab ((,class (:background ,radiant-bg :foreground ,radiant-red))))
;;    `(whitespace-newline ((,class (:foreground ,radiant-bg+1))))
;;    `(whitespace-trailing ((,class (:foreground ,radiant-red :background ,radiant-bg))))
;;    `(whitespace-line ((,class (:background ,radiant-bg-05 :foreground ,radiant-magenta))))
;;    `(whitespace-space-before-tab ((,class (:background ,radiant-orange :foreground ,radiant-orange))))
;;    `(whitespace-indentation ((,class (:background ,radiant-yellow :foreground ,radiant-red))))
;;    `(whitespace-empty ((,class (:background ,radiant-yellow :foreground ,radiant-red))))
;;    `(whitespace-space-after-tab ((,class (:background ,radiant-yellow :foreground ,radiant-red))))

;;    ;; wanderlust
;;    `(wl-highlight-folder-few-face ((,class (:foreground ,radiant-red-2))))
;;    `(wl-highlight-folder-many-face ((,class (:foreground ,radiant-red-1))))
;;    `(wl-highlight-folder-path-face ((,class (:foreground ,radiant-orange))))
;;    `(wl-highlight-folder-unread-face ((,class (:foreground ,radiant-blue))))
;;    `(wl-highlight-folder-zero-face ((,class (:foreground ,radiant-fg))))
;;    `(wl-highlight-folder-unknown-face ((,class (:foreground ,radiant-blue))))
;;    `(wl-highlight-message-citation-header ((,class (:foreground ,radiant-red-1))))
;;    `(wl-highlight-message-cited-text-1 ((,class (:foreground ,radiant-red))))
;;    `(wl-highlight-message-cited-text-2 ((,class (:foreground ,radiant-green+2))))
;;    `(wl-highlight-message-cited-text-3 ((,class (:foreground ,radiant-blue))))
;;    `(wl-highlight-message-cited-text-4 ((,class (:foreground ,radiant-blue+1))))
;;    `(wl-highlight-message-header-contents-face ((,class (:foreground ,radiant-green))))
;;    `(wl-highlight-message-headers-face ((,class (:foreground ,radiant-red+1))))
;;    `(wl-highlight-message-important-header-contents ((,class (:foreground ,radiant-green+2))))
;;    `(wl-highlight-message-header-contents ((,class (:foreground ,radiant-green+1))))
;;    `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,radiant-green+2))))
;;    `(wl-highlight-message-signature ((,class (:foreground ,radiant-green))))
;;    `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,radiant-fg))))
;;    `(wl-highlight-summary-answered-face ((,class (:foreground ,radiant-blue))))
;;    `(wl-highlight-summary-disposed-face ((,class (:foreground ,radiant-fg
;;                                                          :slant italic))))
;;    `(wl-highlight-summary-new-face ((,class (:foreground ,radiant-blue))))
;;    `(wl-highlight-summary-normal-face ((,class (:foreground ,radiant-fg))))
;;    `(wl-highlight-summary-thread-top-face ((,class (:foreground ,radiant-yellow))))
;;    `(wl-highlight-thread-indent-face ((,class (:foreground ,radiant-magenta))))
;;    `(wl-highlight-summary-refiled-face ((,class (:foreground ,radiant-fg))))
;;    `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

;;    ;; which-func-mode
;;    `(which-func ((,class (:foreground ,radiant-green+4))))

;;    ;; yasnippet
;;    `(yas/field-highlight-face ((,class (:background ,radiant-pink-1 :foreground ,radiant-black))))

;;    ;; enh-ruby-mode enh-ruby-op-face
;;    `(enh-ruby-op-face ((,class (:foreground ,radiant-blue-7))))
;;    `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,radiant-green+2))))
;;    `(enh-ruby-string-delimiter-face ((,class (:foreground ,radiant-green+2))))
;;    `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,radiant-blue-1))))

;;    ;; yascroll
;;    `(yascroll:thumb-text-area ((,class (:background ,radiant-bg-1))))
;;    `(yascroll:thumb-fringe ((,class (:background ,radiant-bg-1 :foreground ,radiant-bg-1))))
;;    )
)

  ;;; custom theme variables
  (custom-theme-set-variables
   'tailstone
   `(ansi-color-names-vector [,tailstone-900
                              ,red-400
                              ,lime-500
                              ,red-300
                              ,cyan-500
                              ,pink-300
                              ,cyan-300
                              ,tailstone-100])
   ;; fill-column-indicator
   `(fci-rule-color ,tailstone-700)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'tailstone)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; tailstone-theme.el ends here.

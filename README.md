# Texts

Configuration files for development, mainly focused on Clojure & emacs.

clone the config into `.emacs.d` with: `git clone https://github.com/mikepjb/texts.git ~/.emacs.d`

## Copied from original config:

;; Emacs Configuration

;; major aims:
;; to have a SWEET clojure dev environment
;; potentially to be evil. (evaluate)
;; have a portable setup you can compress/put in your own git for deps.
;; have an amazing color scheme.
;; have a workflow to switch between projects like you do with tmux tabs.
;;
;; minor aims:
;; insert git prefix into commits.
;; to be able to commit homedir repo stuff (can't remember the name)
;; possibly setup emacs server to launch under systemd (--user)
;; clean up this file after all of the above

;; Mastering Emacs Notes:
;; M-x info ~= :h for reading docs for emacs/packages
;; fyi, emacs is a tiliing window manager
;; C-x s save all files!
;; C-g will reverse the direction of C-/ undo into a redo!
;; Don't underestimate the negative bindings C-M-- & M--
;; S-exp navigation:
;; - C-M-f + C-M-b : forward/backward s-exp
;; - C-M-d + C-M-u : down/up a list (nested s-exps)
;; - C-M-k : kill-sexp (C-M-u + C-M-k combo kills current s-exp)
;; - C-M-n + C-M-p : forward/backward list
;; - M-a/e : forward/backward sentence, though it behaves similarly to C-M-f/b
;; - C-M-a/e : forward/backward defun
;; - C-M-<SPC> : mark s-exp, see also C-M-- + C-M-<SPC> to mark in reverse
;; C-x h marks the entire buffer!
;; C-s searches but C-M-s regexp-searches!
;; M-n/M-p can search through C-s/isearch history.
;; C-M-i is for Tab completion, where tab isn't available e.g C-s.
;; C-s C-w search under word (C-w adds word, C-M-y adds character)
;; M-s o : occur in a grep-like utility that comes with emacs.
;; Occur also activates inside isearch.
;; Imenu is the framework for jumping to points of interest in a buffer. It's not bound to any key.
;; >>>!!! Suggests using Helm. I've used Ivy in the past. What's best?
;;   - try Google Suggest from Helm
;; You can tell emacs to describe everything under a prefix key with help-map e.g C-x c (C-h/S-h)
;; M-s . : search under cursor
;; grep-search : grep,grep-find,lgrep,rgrep,rzgrep
;; mainly you'll want to use rgrep, you can go up/down matches with M-g,M-n/p
;; More Editing:
;;  - C-S-<backspace> : kill current line, no more 1 C-k
;;  - M-k : kill sentence
;;  - C-M-w : append kill (to clipboard) if next command is a kill.
;;  - M-y : select yank from kill-ring (previously copied/killed text)
;;  - C-t,M-t : transpose characters/words, a.k.a pulling chars/words to the right.
;;  - C-M-t : transpose s-exps
;;  - C-x C-t : transport lines
;;  - there are also transpose-paragraphs and transpose-sentences
;;  - M-q : refill the paragraph
;;  - C-u, M-q : refill & justify text
;;  - C-x f : sets the fill column width (handy for the above M-q)
;;  - C-x . : sets the fill prefix
;;  - auto-fill-mode : potentially similar to what you had in vim, perhaps just for document modes.
;;  - C-M-% (i3 conflict) : query regexp search and replace
;;  - M-% : query search and replace (can also be used inside isearch)
;;  - in search/replace: spc (replace one), ! (replace all), u (undo one), U (under all)
;;  - C-x C-u/l uppercase/lowercase region
;;  - M-c/u/l capitalize/uppercase/lowercase the next word
;;  Filter Category Editing:
;;  - You can count-lines-region, count-matches, count-words, M-= counts-words-region
;;  - You can delete-duplicate-lines (supports C-u C-u C-u for different behaviours)
;;  - You can delete lines by pattern with flush-lines and keep-lines
;;  - M-s o : list lines matching a pattern
;;  - C-x C-o : Deletes all blank lines after point
;;  - M-^ : Joins the line the point is on with the one above, removing prefixes (like vim?)
;;  - delete-trailing-whitespace, cycle-spacing
;;  - M-\ : deletes whitespace around point
;;  Macros:
;;  - C-x (/)/e : start/stop/play macros (there's a LOT more that emacs supports)
;;  Text Expansion/Auto-complete:
;;  - abbrev/dabbrev/hippie expand/skeletons
;;  - skeletons is a templating tool
;;  - YASnippet
;;  - Autoinsert - inserts templates when you create a new file that matches a certain file type.
;;  Sorting/Alignment:
;;  - sort-lines/fields/numeric-fields/columns/paragraphs/regexp-fields (+ C-u!)
;;  - sort-fields is special, M-3 M-x sort-fields would sort on the third field for example.
;;  - sorting fields must be whitespace delimited, to alter use sort-regexp-fields
;;  - align/align-current/align-regexp
;;  M-z + M-S-z : zap/up-to-char
;;  dictionary-search : localhost or dict.org dictionary search!
;;  insert literals with C-q (e.g C-q backspace)
;;  help-map i : refer to the M-x info (manual) to learn more (there's a lot to learn!)
;;  apropos + info-apropos
;;  vcs is in-built, C-x v is the prefix.
;;  C-x p p : switch between projects (part of project.el)
;;  Cross-References:
;;  - M-. : find defs at point
;;  - M-, : pop marker and return after finding a definition
;;  - M-? : find references matching pattern
;;  - C-M-. : find symbols matching pattern
;;  - C-o : show definition (inside xref def buffer)
;;  image-dired is an image thumbnail browser equivalent of dired
;;  EWW emacs web wowser
;;  Shell Commands:
;;  M-! currently blocked by i3
;;  - M-! : shell cmd into new buffer
;;  - C-u M-! : shell cmd into current buffer
;;  - M-& async shell cmd
;;  - M-| pipe region into shell command
;;  - C-u M-| pipe region and replace region with output
;;  Compilation: M-x compile/recompile + (M-g M-n/p) + g (rerun)
;;  C-x p c
;;  Shells:
;;  term is a shell wrapper, ansi-term is a terminal-emulator
;;  eshell is a freak of nature tbh.. (shell implementation written in elisp)
;;  for most cases, use shell and ansi-term for interactive programs (if you must!)
;;  C-c C-z : send stop cmd, everything else is basically readline
;;  C-c C-c : send stop cmd in ansi-term
;;  Conclusion:
;;  - The key to mastering emacs is understanding how to ask Emacs questions
;;  - Even better, have tools (e.g which-key) that will ask questions for you

;; [current. seaarch and replace, p206]


;; diffs between linux + x11 forwarded windows
;; Recursive Mono Linear Static displays extra wide, the 'code' version Rec Mono Linear works just fine though there are no italics.
;; Also make sure you select the keyboard layout want before starting XMing as that will get passed to the X11  forwarded apps and there appears to be no way to change that after the app starts.


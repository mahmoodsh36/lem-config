(in-package :lemetnal)

(lem:define-color-theme "metnal-warm" ("lem-default")
  (:display-background-mode :dark)

  ;; warm amber/brown colors
  (:base00 "#1a1410") ;; background (deep warm black)
  (:base01 "#2a2118") ;; lighter background
  (:base02 "#3d3128") ;; selection background
  (:base03 "#7a6652") ;; comments, muted
  (:base04 "#9c8b78") ;; dark foreground
  (:base05 "#d5c4a1") ;; default foreground
  (:base06 "#e8d5b5") ;; light foreground
  (:base07 "#f5e6cc") ;; lightest foreground
  (:base08 "#d4845e") ;; red/terracotta - variables, errors
  (:base09 "#c9956b") ;; orange/amber - constants, timestamps
  (:base0a "#e8a87c") ;; yellow/peach - types, titles
  (:base0b "#a6b562") ;; green (muted olive) - strings
  (:base0c "#8aab8e") ;; cyan (sage) - builtins
  (:base0d "#b5824a") ;; blue (bronze) - functions
  (:base0e "#c4956a") ;; purple (warm tan) - keywords
  (:base0f "#8b6b4a") ;; brown - special

  ;; some core colors
  (:foreground "#d5c4a1")
  (:background "#1a1410")
  (:inactive-window-background "#151110")
  (lem-core:region :foreground nil :background "#3d3128")
  (lem-core:cursor :foreground "#1a1410" :background "#e8a87c")
  (lem-core:modeline :foreground "#d5c4a1" :background "#2a2118")
  (lem-core:modeline-inactive :foreground "#7a6652" :background "#1e1812")

  ;; syntax highlighting
  (lem-core:syntax-warning-attribute :foreground "#d4845e")
  (lem/buffer/internal:syntax-string-attribute :foreground "#a6b562")
  (lem/buffer/internal:syntax-comment-attribute :foreground "#7a6652")
  (lem/buffer/internal:syntax-keyword-attribute :foreground "#c4956a")
  (lem/buffer/internal:syntax-constant-attribute :foreground "#c9956b")
  (lem/buffer/internal:syntax-function-name-attribute :foreground "#b5824a")
  (lem/buffer/internal:syntax-variable-attribute :foreground "#d4845e")
  (lem/buffer/internal:syntax-type-attribute :foreground "#e8a87c")
  (lem-core:syntax-builtin-attribute :foreground "#8aab8e")

  ;; document/markup
  (document-header1-attribute :foreground "#f5e6cc" :bold t)
  (document-header2-attribute :foreground "#e8a87c" :bold t)
  (document-header3-attribute :foreground "#c9956b" :bold t)
  (document-header4-attribute :foreground "#b5824a" :bold t)
  (document-header5-attribute :foreground "#a6b562" :bold t)
  (document-header6-attribute :foreground "#8aab8e" :bold t)
  (document-bold-attribute :bold t)
  (document-italic-attribute :foreground "#c9956b")
  (document-underline-attribute :underline t)
  (document-link-attribute :foreground "#b5824a" :underline t)
  (document-list-attribute :foreground "#c9956b")
  (document-code-block-attribute :background "#2a2118" :foreground "#d5c4a1")
  (document-inline-code-attribute :background "#2a2118" :foreground "#d4845e")
  (document-blockquote-attribute :foreground "#9c8b78")
  (document-table-attribute :foreground "#d5c4a1" :background "#2a2118")
  (document-task-list-attribute :foreground "#a6b562")
  (document-metadata-attribute :foreground "#b5824a")

  ;; organ-mode highlighting
  (organ/organ-mode::organ-header-stars-attribute
   :foreground "#8b7355")
  (organ/organ-mode::organ-header-title-attribute
   :foreground "#e8a87c" :bold t)
  (organ/organ-mode::organ-header-todo-attribute
   :foreground "#d4845e")
  (organ/organ-mode::organ-timestamp-attribute
   :foreground "#c9956b")
  (organ/organ-mode::organ-block-attribute
   :foreground "#7a6652")
  (organ/organ-mode::organ-list-bullet-attribute
   :foreground "#c9956b")
  (organ/organ-mode::organ-table-delimiter-attribute
   :foreground "#7a6652")
  (organ/organ-mode::organ-keyword-attribute
   :foreground "#7a6652")
  (organ/organ-mode::organ-latex-env-attribute
   :foreground "#b5824a")
  (organ/organ-mode::organ-block-keyword-attribute
   :foreground "#d4845e")

  ;; agenda-mode highlighting
  (organ/agenda-mode::agenda-keyword-attribute
   :foreground "#d4845e")
  (organ/agenda-mode::agenda-time-attribute
   :foreground "#c9956b")
  (organ/agenda-mode::agenda-state-attribute
   :foreground "#b5824a")
  (organ/agenda-mode::agenda-day-attribute
   :foreground "#e8a87c" :bold t))

;; activate the theme
(lem:load-theme "metnal-warm")
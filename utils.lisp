(defpackage :lemetnal/utils
  (:use :cl)
  (:export :find-nearest-rule-match :change-in-pair))

(in-package :lemetnal/utils)

(defun change-in-pair (pair-rule)
  "vim-style 'ci' for paired delimiters. PAIR-RULE is a combinator pair sexp,
e.g. '(cltpt/combinator:pair \"(\" \")\").
finds the nearest enclosing match around the cursor, deletes the inner content
(keeping the delimiters), and positions the cursor between them."
  (let ((m (find-nearest-rule-match pair-rule)))
    (when m
      (let* ((children (cltpt/combinator:match-children m))
             (opening-match (first children))
             (closing-match (car (last children)))
             ;; child positions are relative to parent, so add parent begin
             (inner-start (+ m-begin (cltpt/combinator:match-end-absolute opening-match)))
             (inner-end (+ m-begin (cltpt/combinator:match-begin-absolute closing-match))))
        (when (> inner-end inner-start)
          (organ/utils:replace-text-between-positions
           (lem:current-buffer)
           (1+ inner-start)
           (1+ inner-end)
           ""))
        (lem:move-point
         (lem:current-point)
         (organ/utils:char-offset-to-point (lem:current-buffer) inner-start))))))

(defun find-nearest-rule-match (rule &key (buffer (lem:current-buffer))
                                          (pos (organ/utils:current-pos)))
  "find the nearest match for a combinator RULE that encloses POS in BUFFER.
searches backwards from POS trying apply-rule at each position, and returns the first match
whose range contains POS. if none found backwards, searches forwards and returns the first match.
returns the combinator match, or NIL if nothing matches."
  (let* ((text (lem:buffer-text buffer))
         (reader (cltpt/combinator:reader-from-input text))
         (len (length text))
         (ctx (cltpt/combinator::make-context :rules (list rule)))
         (cltpt/reader:*reader-fast-buffer* (cltpt/reader:reader-fast-buffer reader))
         (cltpt/reader:*reader-fast-buffer-length*
           (cltpt/reader:reader-fast-buffer-length reader)))
    ;; search backwards: find a match that starts at or before pos and encloses it
    (or (loop for i from pos downto 0
              for m = (cltpt/combinator:apply-rule ctx rule reader i)
              when (and m (<= (cltpt/combinator:match-begin m) pos)
                         (>= (cltpt/combinator:match-end m) pos))
                return m)
        ;; search forwards: return first match found
        (loop for i from (1+ pos) below len
              for m = (cltpt/combinator:apply-rule ctx rule reader i)
              when m return m))))
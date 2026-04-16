(defpackage :lemetnal/utils
  (:use :cl)
  (:export :find-nearest-rule-match :change-in-pair :pair-inner-offsets :my-getenv :require-env))

(in-package :lemetnal/utils)

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
    (let ((m-before)
          (m-enclosing))
      (loop for i from pos downto 0
            for m = (cltpt/combinator:apply-rule ctx rule reader i)
            when m
              do (cond
                   ((and (null m-enclosing)
                         (<= (cltpt/combinator:match-begin m) pos)
                         (> (cltpt/combinator:match-end m) pos))
                    (setf m-enclosing m)
                    (return))
                   ((and (null m-before)
                         (<= (cltpt/combinator:match-end m) pos))
                    (setf m-before m))))
      (let ((m-after (loop for i from (1+ pos) below len
                           for m = (cltpt/combinator:apply-rule ctx rule reader i)
                           when m return m)))
        (cond
          (m-enclosing m-enclosing)
          ((and m-before m-after)
           (let ((d1 (- pos (cltpt/combinator:match-end m-before)))
                 (d2 (- (cltpt/combinator:match-begin m-after) pos)))
             (if (< d2 d1)
                 m-after
                 m-before)))
          (m-before m-before)
          (m-after m-after)
          (t nil))))))

(defun pair-inner-offsets (pair-rule)
  "find the inner range of the nearest matching pair for PAIR-RULE.
returns (values inner-start inner-end) or NIL."
  (let ((m (find-nearest-rule-match pair-rule)))
    (when m
      (let* ((children (cltpt/combinator:match-children m))
             (opening-match (first children))
             (closing-match (car (last children)))
             (inner-start (cltpt/combinator:match-end-absolute opening-match))
             (inner-end (cltpt/combinator:match-begin-absolute closing-match)))
        (values inner-start inner-end)))))

(defun change-in-pair (pair-rule)
  "vim-style 'ci' for paired delimiters. PAIR-RULE is a combinator pair sexp,
e.g. '(cltpt/combinator:pair \"(\" \")\")."
  (multiple-value-bind (inner-start inner-end)
      (pair-inner-offsets pair-rule)
    (when (and inner-start inner-end)
      (when (> inner-end inner-start)
        (organ/utils:replace-text-between-positions
         (lem:current-buffer)
         (1+ inner-start)
         (1+ inner-end)
         ""))
      (lem:move-point
       (lem:current-point)
       (organ/utils:char-offset-to-point (lem:current-buffer) inner-start)))))

(defun my-getenv (var-name)
  "get environment variable VAR-NAME falling back to reading from a login zsh shell.
first tries sb-ext:posix-getenv. if that returns nil or empty string, tries HOME as fallback
for home-directory-related vars. if still empty, executes 'zsh -l -c \"echo \\$VAR-NAME\"' and returns the output."
  (let ((value (sb-ext:posix-getenv var-name)))
    (when (and value (not (string= value "")))
      (return-from my-getenv value)))
  ;; last resort: try zsh login shell
  (handler-case
      (let* ((command (format nil "zsh -l -c 'echo $~A'" var-name))
             (output (with-output-to-string (stream)
                       (uiop:run-program command :output stream :error-output :interactive)))
             (trimmed (string-trim '(#\newline #\return #\space) output)))
        (if (and trimmed (not (string= trimmed "")))
            trimmed
            ;; if zsh also empty, try HOME from zsh as final fallback
            (when (search "HOME" var-name)
              (let* ((home-command "zsh -l -c 'echo $HOME'")
                     (home-output (with-output-to-string (stream)
                                    (uiop:run-program home-command
                                                      :output stream
                                                      :error-output :interactive))))
                (string-trim '(#\newline #\return #\space) home-output)))))
    (error (c)
      (format *error-output* "could not get ~A: ~A~%" var-name c)
      nil)))

(defun require-env (var-name)
  (or (my-getenv var-name)
      (error "couldnt get ~A env var" var-name)))

(defun from-work (path)
  (cltpt/file-utils:join-paths
   (my-getenv "WORK_DIR")
   path))

(defun from-brain (path)
  (cltpt/file-utils:join-paths
   (my-getenv "BRAIN_DIR")
   path))
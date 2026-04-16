(defpackage :lemetnal
  (:use :cl :lem :lem-elisp-mode :lem/buffer/internal :lem-core :lemetnal/utils))

(in-package :lemetnal)

;; so we dont have to use `define-command' explicitly
(defmacro cmd (&body body)
  (let ((name (gensym "lambda-command-")))
    `(progn
       (define-command ,name () () ,@body)
       ',name))) ;; return the generated command name

(setf lem-core::*default-prompt-gravity* :bottom-display)
(setf lem/prompt-window::*prompt-completion-window-gravity* :horizontally-above-window)
(setf lem/prompt-window::*fill-width* t)

;; C-h is initially bound to delete-previous-char
(defvar *help-keymap*
  (make-keymap :description '*my-help-keymap*)
  "keymap for help commands.")
(define-keys *help-keymap*
  ("b" 'describe-bindings)
  ("k" 'describe-key)
  ("A" 'lem-lisp-mode:lisp-apropos)
  ("a" 'lem-lisp-mode/internal::lisp-search-symbol)
  ("c" 'apropos-command)
  ("p" 'lem-lisp-mode:lisp-apropos-package)
  ("f" 'lem-lisp-mode:lisp-describe-symbol))

(defvar *frame-keymap*
  (make-keymap :description '*frame-multiplexer-keymap*)
  "keymap for commands related to the frame-multiplexer.")
(define-key *frame-keymap* "c" 'frame-multiplexer-create-with-new-buffer-list)
(define-key *frame-keymap* "d" 'frame-multiplexer-delete)
(define-key *frame-keymap* "p" 'frame-multiplexer-prev)
(define-key *frame-keymap* "n" 'frame-multiplexer-next)
(define-key *frame-keymap* "r" 'frame-mulitplexer-rename)

(defvar *window-keymap*
  (make-keymap :description '*my-window-keymap*)
  "keymap for window related commands.")

;; relative line numbers
(lem/line-numbers:toggle-line-numbers)
(setf lem/line-numbers:*relative-line* t)

;; buffer-related keys
(define-command my-kill-current-buffer () ()
  (kill-buffer (current-buffer)))
(define-command my-kill-current-buffer-and-window () ()
  (kill-buffer (current-buffer))
  (lem-core/commands/window:delete-active-window))

;; update completion after backspace
(define-command completion-backspace () ()
  (ignore-errors
    (delete-previous-char)
    (ignore-errors
      (lem/completion-mode::continue-completion
       lem/completion-mode::*completion-context*))))

(define-keys lem/completion-mode::*completion-mode-keymap*
  ("Backspace" 'completion-backspace)
  ("Return" (cmd
              (lem/completion-mode::completion-select)
              (when (lem/prompt-window::current-prompt-window)
                (lem/prompt-window::prompt-execute)))))

(define-command fp-find-file () ()
  "find-file with backspace bound to up-directory."
  (let ((keys (make-keymap)))
    (define-key keys "Backspace" 'fp-up-directory)
    (with-special-keymap (keys)
      (call-command 'find-file (universal-argument-of-this-command)))))
(define-command fp-up-directory () ()
  "Delete the last path segment in file prompt."
  (alexandria:when-let*
      ((pwindow (lem/prompt-window::current-prompt-window))
       (wstring (and pwindow (lem/prompt-window::get-input-string))))
    (lem/prompt-window::replace-prompt-input
     (ignore-errors
       (let* ((trimmed (str:trim-right wstring :char-bag '(#\/ )))
              (endp (1+ (position #\/ trimmed :from-end t :test #'char-equal))))
         (subseq trimmed 0 endp))))
    (lem/completion-mode::completion-end)
    (ignore-errors (lem/prompt-window::prompt-completion))))

;; keys to find files
(define-command find-config () ()
  (find-file (concatenate 'string
                          (my-getenv "HOME")
                          "/.lem/init.lisp")))

(define-command my-find-definition () ()
  (lem-lisp-mode/internal::check-connection)
  (let ((symbol-name
           (lem-lisp-mode/internal::prompt-for-symbol-name "Describe symbol: "
                                                           (or (lem/buffer/internal::symbol-string-at-point (current-point)) ""))))
    (format t "looking up ~A~%" symbol-name)
    (lem-lisp-mode/internal::find-definitions-by-name symbol-name)))

;; set font
(lem-core/commands/font::font-size-set 13)

(define-command close-all-floating-windows () ()
  "close all floating windows in the current frame."
  (let ((frame (current-frame)))
    (when frame
      (dolist (window (frame-floating-windows frame))
        (delete-window window)))))

(setf *automatic-tab-completion* t)

(define-key *global-keymap* "C-x C-e" 'lem-lisp-mode/eval::lisp-eval-at-point)
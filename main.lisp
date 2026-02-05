(defpackage :lem-user
  (:use :cl :lem :lem-elisp-mode :lem/buffer/internal :lem-core))

(in-package :lem-user)

;; so we dont have to use `define-command' explicitly
(defmacro cmd (&body body)
  (let ((name (gensym "lambda-command-")))
    `(progn
       (define-command ,name () () ,@body)
       ',name))) ;; return the generated command name

;; start in vi-mode
(lem-vi-mode:vi-mode)

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
(define-keys *window-keymap*
  ("l" 'lem-vi-mode/binds::vi-window-move-right)
  ("h" 'lem-vi-mode/binds::vi-window-move-left)
  ("j" 'lem-vi-mode/binds::vi-window-move-down)
  ("k" 'lem-vi-mode/binds::vi-window-move-up)
  ("s" 'lem-vi-mode/binds::vi-window-split-vertically)
  ("v" 'lem-vi-mode/binds::vi-window-split-horizontally)
  ("c" 'lem-vi-mode/binds::vi-close)
  ("o" 'lem-vi-mode/binds::delete-other-windows)
  )

;; relative line numbers
(lem/line-numbers:toggle-line-numbers)
(setf lem/line-numbers:*relative-line* t)

;; spc spc to run commands
(define-key lem-vi-mode:*normal-keymap*
  "Space Space"
  'execute-command)
;; s to save
(define-key lem-vi-mode:*normal-keymap*
  "s"
  'save-current-buffer)
;; r instead of C-r to redo
(define-key lem-vi-mode:*normal-keymap*
  "r"
  'lem-vi-mode/commands:vi-redo)
;; spc-h for help keys
;; (define-key *global-keymap* "C-z" *frame-keymap*)
(define-key lem-vi-mode:*normal-keymap*
  "Space h"
  *help-keymap*)
(define-key lem-vi-mode:*insert-keymap*
  "C-h"
  *help-keymap*)
(define-key lem-vi-mode:*normal-keymap*
  "Space w"
  *window-keymap*)

(defun led-key (key command)
  (define-key lem-vi-mode:*normal-keymap*
      (concatenate 'string "Space " key)
    command)
  (define-key *global-keymap*
      (concatenate 'string "C-' " key)
    command))

;; show completions in minibuffer instantly
;; (add-hook *prompt-after-activate-hook*
;;           (lambda ()
;;             (call-command 'lem/prompt-window::prompt-completion nil)))
;; (add-hook *prompt-deactivate-hook*
;;           (lambda ()
;;             (lem/completion-mode:completion-end)))

(led-key "x" 'lem-lisp-mode/eval::lisp-eval-defun)
(led-key "s" 'lem-lisp-mode/eval::lisp-eval-string)

;; buffer-related keys
(led-key "b s" 'select-buffer)
(define-command my-kill-current-buffer () ()
  (kill-buffer (current-buffer)))
(define-command my-kill-current-buffer-and-window () ()
  (kill-buffer (current-buffer))
  (lem-core/commands/window:delete-active-window))
(led-key "b k" 'my-kill-current-buffer)
(led-key "b K" 'my-kill-current-buffer-and-window)

(define-key lem-vi-mode:*normal-keymap* "g d" 'lem/language-mode::find-definitions)

;; update completion after backspace
(define-command completion-backspace () ()
  (ignore-errors
    (delete-previous-char)
    (ignore-errors
      (lem/completion-mode::continue-completion
       lem/completion-mode::*completion-context*))))

;; accept current prompt completion entry without trying to complete
(define-command prompt-execute-auto-accept () ()
  (let ((input (lem/prompt-window::get-input-string)))
    (when (or (null (lem/prompt-window::prompt-window-existing-test-function (lem/prompt-window::current-prompt-window)))
              (funcall (lem/prompt-window::prompt-window-existing-test-function (lem/prompt-window::current-prompt-window)) input))
      (lem/common/history:add-history (lem/prompt-window::prompt-window-history (lem/prompt-window::current-prompt-window)) input)
      (error 'lem/prompt-window::execute-condition :input input))))

;; (undefine-key lem/completion-mode::*completion-mode-keymap* "Return")
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
(led-key "f f" 'fp-find-file)
(define-command find-config () ()
  (find-file (concatenate 'string
                          (sb-ext:posix-getenv "HOME_DIR")
                          "/.lem/init.lisp")))
(led-key "e" 'find-config)

;; (define-command python-eval-region (start end) (:region)
;;   (unless (alive-process-p)
;;     (editor-error "Python process doesn't exist."))
;;   (lem-process:process-send-input *process* (points-to-string start end)))

(define-command my-find-definition () ()
  (lem-lisp-mode/internal::check-connection)
  (let ((symbol-name
          (lem-lisp-mode/internal::prompt-for-symbol-name "Describe symbol: "
                                                          (or (lem/buffer/internal::symbol-string-at-point (current-point)) ""))))
    (format t "looking up ~A~%" symbol-name)
    (lem-lisp-mode/internal::find-definitions-by-name symbol-name)))

(define-command my-menu () ()
  (prompt-for-string
   "test"
   :completion-function (lambda (val) (list "test1" "test2" "test3"))))

;; set font
;; (lem-if:set-font-name "iosevka")
;; (lem-core/commands/font::font-size-set 15)
(lem-core/commands/font::font-size-set 13)

;; lisp mode keybindings
(define-key lem-lisp-mode/eval::*lisp-mode-keymap*
  "Space c"
  'lem-lisp-mode/eval::lisp-eval-buffer)

(led-key
 "l"
 (cmd
  (pushnew (concatenate 'string
                        (sb-ext:posix-getenv "WORK_DIR")
                        "/cltpt/")
           asdf:*central-registry*
           :test #'equal)
  (let ((uiop:*compile-file-failure-behaviour* :warn))
    (handler-bind ((error
                     (lambda (c)
                       (format t "Ignored error: ~A~%" c)
                       (let ((r (find-restart 'continue c)))
                         (when r (invoke-restart r))))))
      (asdf:load-system "cltpt" :force t)))
  (pushnew (concatenate 'string
                        (sb-ext:posix-getenv "WORK_DIR")
                        "/organ-mode/")
           asdf:*central-registry*
           :test #'equal)
  (let ((uiop:*compile-file-failure-behaviour* :warn))
    (handler-bind ((error
                     (lambda (c)
                       (format t "Ignored error: ~A~%" c)
                       (let ((r (find-restart 'continue c)))
                         (when r (invoke-restart r))))))
      (asdf:load-system "organ-mode" :force t)))
  ))

;; (when (find-package "ORGAN")
;;   (load "organ.lisp"))

(define-command close-all-floating-windows () ()
  "close all floating windows in the current frame."
  (let ((frame (current-frame)))
    (when frame
      (dolist (window (frame-floating-windows frame))
        (delete-window window)))))

;; this isnt what org-mode does but i think its more idomatic
;; (lem:define-key *organ-mode-keymap* "C-l" *organ-mode-navigation-keymap*)
;; (lem:define-keys *organ-mode-navigation-keymap*
;;   ("n" 'organ-next-element)
;;   ("N" 'organ-prev-element)
;;   ("h" 'organ-next-header)
;;   ("H" 'organ-prev-header)
;;   ("l" 'organ-next-link)
;;   ("L" 'organ-prev-link)
;;   ("c" 'organ-next-src-block)
;;   ("C" 'organ-prev-src-block)
;;   ("b" 'organ-next-block)
;;   ("B" 'organ-prev-block))

;; (pushnew :quicklisp *features*)

;; (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                        (user-homedir-pathname))))
;;   (when (probe-file quicklisp-init)
;;     (load quicklisp-init)))

;; (setf lem-extension-manager:*packages-directory*
;;       (merge-pathnames ".lem/packages/" (user-homedir-pathname)))

;; (lem-extension-manager:lem-use-package
;;  "organ-mode"
;;  :source (:type :git
;;           :url "https://github.com/mahmoodsh36/organ-mode")
;;  :force t
;;  :dependencies (
;;                 ;; ("clingon"
;;                 ;;  :source (:type :git
;;                 ;;           :url "https://github.com/dnaeon/clingon"))
;;                 ;; ("cltpt"
;;                 ;;  :source (:type :git
;;                 ;;           :url "https://github.com/mahmoodsh36/cltpt"))
;;                 ))

;; (let* ((asdf:*central-registry*
;;          (union (lem-extension-manager::packages-list)
;;                 asdf:*central-registry*
;;                 :test #'equal)))
;;   (asdf:load-system :organ-mode))

;; (defun outline-test ()
;;   (lem-outline::open-outline-internal
;;    '(("Root Node" ("Child 1" ("Grandchild A" "Grandchild B") "Child 2")
;;       ("Another Root" "Simple Child" ("Complex Child" "Nested Item"))
;;       ("Final Root" ("Deep" ("Deeper" ("Deepest"))))))))
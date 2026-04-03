(in-package :lemetnal)

;; start in vi-mode
(lem-vi-mode:vi-mode)

;; vi-mode specific window keybindings
(define-keys *window-keymap*
  ("l" 'lem-vi-mode/binds::vi-window-move-right)
  ("h" 'lem-vi-mode/binds::vi-window-move-left)
  ("j" 'lem-vi-mode/binds::vi-window-move-down)
  ("k" 'lem-vi-mode/binds::vi-window-move-up)
  ("s" 'lem-vi-mode/binds::vi-window-split-vertically)
  ("v" 'lem-vi-mode/binds::vi-window-split-horizontally)
  ("c" 'lem-vi-mode/binds::vi-close)
  ("o" 'lem-vi-mode/binds::delete-other-windows))

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

(define-key lem-vi-mode:*normal-keymap* "g d" 'lem/language-mode::find-definitions)

;; Helper for cltpt and organ reloading
(define-command reload-config-systems () ()
  (pushnew (concatenate 'string
                        (my-getenv "WORK_DIR")
                        "/cltpt/")
           asdf:*central-registry*
           :test #'equal)
  (remhash "cltpt" asdf/system-registry:*immutable-systems*)
  (asdf:clear-system "cltpt")
  (let ((uiop:*compile-file-failure-behaviour* :warn))
    (handler-bind ((error
                     (lambda (c)
                       (format t "Ignored error: ~A~%" c)
                       (let ((r (find-restart 'continue c)))
                         (when r (invoke-restart r))))))
      (asdf:load-system "cltpt" :force t)))
  (pushnew (concatenate 'string
                        (my-getenv "WORK_DIR")
                        "/organ-mode/")
           asdf:*central-registry*
           :test #'equal)
  (remhash "organ-mode" asdf/system-registry:*immutable-systems*)
  (asdf:clear-system "organ-mode")
  (let ((uiop:*compile-file-failure-behaviour* :warn))
    (handler-bind ((error
                     (lambda (c)
                       (format t "Ignored error: ~A~%" c)
                       (let ((r (find-restart 'continue c)))
                         (when r (invoke-restart r))))))
      (asdf:load-system "organ-mode" :force t))))

;; keybindings for general commands using lead-key
(led-key "x" 'lem-lisp-mode/eval::lisp-eval-defun)
(led-key "s" 'lem-lisp-mode/eval::lisp-eval-string)
(led-key "b s" 'select-buffer)
(led-key "b k" 'my-kill-current-buffer)
(led-key "b K" 'my-kill-current-buffer-and-window)
(led-key "d d" 'lem/directory-mode::find-file-directory)
(led-key "f f" 'fp-find-file)
(led-key "e" 'find-config)
(led-key "l" 'reload-config-systems)

;; directory-mode keybindings
(undefine-key lem/directory-mode::*directory-mode-keymap* "Space")
(define-key lem/directory-mode::*directory-mode-keymap* "-" 'lem/directory-mode::directory-mode-up-directory)

;; lisp mode vi integration
(defvar *lisp-vi-normal-keymap* (lem:make-keymap :description '*lisp-vi-normal-keymap*))
(lem:define-key *lisp-vi-normal-keymap* "Space c" 'lem-lisp-mode/eval::lisp-eval-buffer)
(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode lem-lisp-mode:lisp-mode))
  (when (typep (lem-vi-mode/core:current-state) 'lem-vi-mode/states:normal)
    (list *lisp-vi-normal-keymap*)))

(defun offsets-to-range (start-offset end-offset)
  (when (and start-offset end-offset)
    (lem-vi-mode/core:make-range
     (organ/utils:char-offset-to-point (lem:current-buffer) start-offset)
     (organ/utils:char-offset-to-point (lem:current-buffer) end-offset))))

;; test 'cip'
(lem-vi-mode/commands/utils:define-text-object-command change-in-test () () ()
  (multiple-value-call
      #'offsets-to-range
    (lemetnal/utils:pair-inner-offsets '(cltpt/combinator:pair "(%" "%)"))))
(define-key lem-vi-mode:*inner-text-objects-keymap* "p" 'change-in-test)

;; override vi-change in the lem-vi-mode/commands package to fix:
;; 1. trailing whitespace bug (for all text objects)
;; 2. cursor placement bug (when cursor isnt contained within changed region)

;; completely suppress redefinition-related aborts and warnings for core override
#+sbcl
(defmacro without-redefinition-warnings (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind ((error (lambda (c) (declare (ignore c)) (continue)))
                    (warning #'muffle-warning)
                    (style-warning #'muffle-warning))
       (sb-ext:without-package-locks
         ,@body))))

#+sbcl
(without-redefinition-warnings
  (lem-vi-mode/commands::define-operator lem-vi-mode/commands:vi-change (beg end type) ("<R>")
      (:move-point nil)
    (when (lem:point= beg end)
      (return-from lem-vi-mode/commands:vi-change))
    (let ((end-with-newline (cl:char= (lem:character-at end -1) #\Newline)))
      (case type
        (:line
         (lem-vi-mode/commands::vi-delete beg end type)
         (cond
           (end-with-newline
            (lem:insert-character (lem:current-point) #\Newline)
            (lem:character-offset (lem:current-point) -1))
           (t
            (lem:insert-character (lem:current-point) #\Newline)))
         (lem:indent-line (lem:current-point)))
        (t (unless (or (typep (lem-vi-mode/core:this-motion-command)
                              'lem-vi-mode/core:vi-text-object)
                       (eql (lem:character-at (lem:current-point)) #\space))
             (lem-vi-mode/commands::skip-whitespace-backward end))
           (lem-vi-mode/commands::vi-delete beg end type)
           (lem:move-point (lem:current-point) beg))))
    (setf (lem-vi-mode/core:buffer-state) 'lem-vi-mode/states:insert)))
(in-package :lemetnal)

(defun latex-set (operator operands props)
  (with-output-to-string (out)
    (if (getf props :is-display)
        (format out "\\set{~%")
        (format out "\\set*{~%"))
    (format out
            (str:join ", "
                      (mapcar
                       (lambda (arg)
                         (lisp-to-latex arg))
                       operands)))
    (format out "~%}")))

(defun latex-set-builder (operator operands props)
  (with-output-to-string (out)
    (if (getf props :is-display)
        (format out "\\set{~%")
        (format out "\\set*{~%"))
    (format out
            (str:join ", "
                      (mapcar
                       (lambda (arg)
                         (lisp-to-latex arg))
                       (car operands))))
    (format out " \\given ")
    (format out
            (str:join ", "
                      (mapcar
                       (lambda (arg)
                         (lisp-to-latex arg))
                       (cadr operands))))
    (format out "~%}")))

(defun latex-expt (operator operands props)
  "convert an exponentiation expression to latex.
expects two operands: base and exponent."
  (if (= (length l) 2)
      (format nil
              "{~A}^{~A}"
              (lisp-to-latex (first operands))
              (lisp-to-latex (second operands)))
      (error "exponentiation expects exactly two operands.")))

(defun latex-equality (operator operands keywords)
  (str:join " = "
            (mapcar
             (lambda (arg)
               (lisp-to-latex arg))
             operands)))

(defclass math-expr (text-object)
  ()
  (:documentation "a math expression."))

(defun make-math-expr (expr)
  (let* ((obj (make-instance 'math-expr)))
    (setf (text-object-property obj :expr) expr)
    obj))

(defmethod text-object-export ((obj math-expr) backend)
  (pcase backend
    (latex
     (list :text (lisp-to-latex (text-object-property obj :expr))
           :escape nil
           :recurse nil))
    ))

;; helper to split arguments into operands and keywords
(defun split-args (args)
  (loop with i = 0
        until (or (>= i (length args)) (keywordp (elt args i)))
        do (incf i)
        finally (return (values (subseq args 0 i)
                                (subseq args i)))))

(defun latex-vector (operator operands keywords)
  (with-output-to-string (out)
    (format out "\\begin{pmatrix}~%")
    (dolist (entry operands)
      (format out "~A\\\\ " (lisp-to-latex entry)))
    (format out "~%\\end{pmatrix}")))

(defun latex-matrix (operator operands keywords)
  (declare (ignore keywords))
  (with-output-to-string (out)
    (format out "\\begin{pmatrix}~%")
    (loop for sublist in operands for i from 0
          do (loop for entry in sublist for j from 0
                   do (if (= j (1- (length sublist)))
                          (format out " ~A" (lisp-to-latex entry))
                          (format out " ~A &" (lisp-to-latex entry))))
             (unless (= i (1- (length operands)))
               (format out "\\\\~%")))
    (format out "~%\\end{pmatrix}")))

(defun latex-addition (operator operands keywords)
  (str:join " + " (mapcar (lambda (x) (lisp-to-latex x)) operands)))

(defun latex-subtraction (operator operands keywords)
  (format nil
          "~{~A~^-~}"
          (mapcar (lambda (x) (lisp-to-latex x)) operands)))

(defun latex-multiplication (operator operands keywords)
  (str:join " \\cdot " (mapcar #'(lambda (x) (lisp-to-latex x)) operands)))

(defun latex-division (operand operands keywords)
  (if (= (length operands) 2)
      (format nil (or (getf keywords :frac-style) "\\frac{~A}{~A}")
              (lisp-to-latex (first operands))
              (lisp-to-latex (second operands)))
      (error "division expects exactly two operands.")))

(defun latex-default (operator operands keywords)
  (let* ((op-name
           (cond
             ;; handle string operators directly
             ((stringp operator) operator)
             ;; default symbol-based lookup
             ((symbolp operator) (symbol-name operator))
             (t operator)))
         (infix? (getf keywords :infix)))
    (cond
      (infix?
       (format nil
               "~A ~A ~A"
               (lisp-to-latex (first operands))
               op-name
               (lisp-to-latex (second operands))))
      (t
       (format nil "\\operatorname{~A}\\left( ~A \\right)"
               op-name
               (str:join ", "
                         (mapcar (lambda (arg)
                                   (lisp-to-latex arg))
                                 operands)))))))

;; main conversion function
(defun lisp-to-latex (expr)
  (cond
    ((atom expr)
     (cond
       ((numberp expr)
        (princ-to-string expr))
       ((symbolp expr)
        (let ((remap (assoc expr *symbol-remap*)))
          (if remap
              (cdr remap)
              (format nil "\\texttt{~A}" (string-downcase (symbol-name expr))))))
       (t (format nil "~A" expr))))
    ((listp expr)
     (let ((head (car expr))
           (args (cdr expr)))
       (multiple-value-bind (operands keywords)
           (split-args args)
         (cond
           ((and (symbolp head) (assoc head *operator-dispatch* :test 'string=))
            (funcall (cdr (assoc head *operator-dispatch* :test 'string=))
                     head
                     operands
                     keywords))
           ((and (symbolp head) (fboundp head))
            (funcall head head operands keywords))
           (t (latex-default head operands keywords))))))
    (t (format nil "~A" expr))))

;; symbol remapping table
(defvar *symbol-remap*
  nil
  "alist mapping symbols to their custom latex representations.")

(defparameter *operator-dispatch*
  '((+ . latex-addition)
    (- . latex-subtraction)
    (* . latex-multiplication)
    (/ . latex-division)
    (matrix . latex-matrix)
    (vector . latex-vector)
    (set . latex-set)
    (set-builder . latex-set-builder)
    (= . latex-equality)
    (expt . latex-expt)))

;; maybe we can write like #$(+ 1 1) instead of #($ '(+ 1 1))?
;; (defun read-dollar-macro (stream char)
;;   (declare (ignore char))
;;   ;; read the next object, and quote it
;;   (let ((obj (read stream t nil t)))
;;     `(quote ,obj)))
(defun read-dollar-macro (stream char)
  (let ((obj (read stream t nil t)))
    `(make-math-expr (quote ,obj))))

;; (set-macro-character #\$ #'read-dollar-macro)
;; (set-macro-character #\m #'read-dollar-macro)

;; $(1 2 3)   ;; => (1 2 3)

;; (push '(x . "\\xi") *symbol-remap*)

;; (lisp-to-latex '(expt 2 3))
(lisp-to-latex '(/ (+ (vector 1 2 3) 1) 10))
;; (lisp-to-latex '(matrix (1 2 3) (4 5 6)))
;; (lisp-to-latex '(set (1 2 3)))
(lisp-to-latex '(/ (+ (pspace x) 1) 10))

(lisp-to-latex
 `(/ (+ (pspace x)
        ,@(loop for i from 1 to 10 collect
                   (format nil "x_{~A}" i)))
     y))

(lisp-to-latex '("\\oplus" (matrix (1 2 3) (4 5 6)) (matrix (1 2 3) (4 5 6))))
(lisp-to-latex '("s" (oplus (matrix (1 2 3) (4 5 6)) (matrix (1 2 3) (4 5 6)) :infix t) 1))
(lisp-to-latex '("s" (oplus (matrix (1 2 3) (4 5 6)) (matrix (1 2 3) (4 5 6)) :infix t) 1))
(lisp-to-latex '(+ (set 1 2 3)))
(lisp-to-latex '(set-builder (x) ((= x 2) (= y 2))))
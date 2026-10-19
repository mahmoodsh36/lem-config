(in-package :lemetnal)

;; symbol remapping table
(defvar *symbol-remap*
  nil
  "alist mapping symbols to their custom latex representations.")

(defvar *math-operator-table*
  (make-hash-table :test 'equal)
  "operator name (string) -> renderer function (lambda (operator operands props) string).")

(defun normalize-math-operator-name (op)
  "unify symbols and strings so 'matrix and \"matrix\" hit one entry."
  (string-downcase
   (typecase op
     (symbol (symbol-name op))
     (string op)
     (t (princ-to-string op)))))

(defun register-math-op (name fn)
  "register FN as the renderer for operator NAME."
  (setf (gethash (normalize-math-operator-name name) *math-operator-table*) fn)
  name)

(defun find-math-op (name)
  (gethash (normalize-math-operator-name name) *math-operator-table*))

(defmacro define-math-op (name (op-var operands-var props-var) &body body)
  "define and register a renderer for operator NAME in one form."
  `(register-math-op ',name
     (lambda (,op-var ,operands-var ,props-var)
       ,@body)))

(defvar *math-backend*
  :latex
  "active math rendering backend. bound dynamically per export.")

(defun math-render (expr &optional props)
  "render EXPR using the current *MATH-BACKEND*."
  (ecase *math-backend*
    (:latex (lisp-to-latex expr props))))

(defun math-render-each (operands &optional props)
  "render each operand with PROPS, returning a list of strings."
  (mapcar
   (lambda (x)
     (math-render x props))
   operands))

(defun math-render-join (separator operands &optional props)
  "render OPERANDS with PROPS and join the results with SEPARATOR."
  (str:join separator (math-render-each operands props)))

(defun latex-set (operator operands props)
  (with-output-to-string (out)
    (if (getf props :is-display)
        ;; \set and other custom macros are from my common.sty
        (format out "\\set{~%")
        (format out "\\set*{~%"))
    (format out (math-render-join ", " operands props))
    (format out "~%}")))

(defun latex-set-builder (operator operands props)
  (with-output-to-string (out)
    (if (getf props :is-display)
        (format out "\\set{~%")
        (format out "\\set*{~%"))
    (format out (math-render-join ", " (car operands) props))
    (format out " \\given ")
    (format out (math-render-join ", " (cadr operands) props))
    (format out "~%}")))

(defun latex-expt (operator operands props)
  (if (= (length operands) 2)
      (format nil
              "{~A}^{~A}"
              (math-render (first operands) props)
              (math-render (second operands) props))
      (error "exponentiation expects exactly two operands.")))

(defun latex-equality (operator operands props)
  (math-render-join " = " operands props))

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

(defun latex-vector (operator operands props)
  (with-output-to-string (out)
    (format out "\\begin{pmatrix}~%")
    (dolist (entry operands)
      (format out "~A\\\\ " (math-render entry props)))
    (format out "~%\\end{pmatrix}")))

(defun latex-matrix (operator operands props)
  (with-output-to-string (out)
    (format out "\\begin{pmatrix}~%")
    (loop for sublist in operands for i from 0
          do (loop for entry in sublist for j from 0
                   do (if (= j (1- (length sublist)))
                          (format out " ~A" (math-render entry props))
                          (format out " ~A &" (math-render entry props))))
             (unless (= i (1- (length operands)))
               (format out "\\\\~%")))
    (format out "~%\\end{pmatrix}")))

(defun latex-addition (operator operands props)
  (math-render-join " + " operands props))

(defun latex-subtraction (operator operands props)
  (format nil
          "~{~A~^-~}"
          (math-render-each operands props)))

(defun latex-multiplication (operator operands props)
  (math-render-join " \\cdot " operands props))

(defun latex-division (operator operands props)
  (if (= (length operands) 2)
      (format nil (or (getf props :frac-style) "\\frac{~A}{~A}")
              (math-render (first operands) props)
              (math-render (second operands) props))
      (error "division expects exactly two operands.")))

(defun latex-default (operator operands props)
  (let* ((op-name
           (cond
             ;; handle string operators directly
             ((stringp operator) operator)
             ;; default symbol-based lookup
             ((symbolp operator) (symbol-name operator))
             (t operator)))
         (infix? (getf props :infix)))
    (cond
      (infix?
       (format nil
               "~A ~A ~A"
               (math-render (first operands) props)
               op-name
               (math-render (second operands) props)))
      (t
       (format nil "\\operatorname{~A}\\left( ~A \\right)"
               op-name
               (math-render-join ", " operands props))))))

(defun latex-render-number (n props)
  (princ-to-string n))

(defun latex-render-symbol (sym props)
  (let ((remap (assoc sym *symbol-remap*)))
    (if remap
        (cdr remap)
        (format nil "\\texttt{~A}" (string-downcase (symbol-name sym))))))

(defun latex-render-atom (expr props)
  (cond
    ((numberp expr) (latex-render-number expr props))
    ((symbolp expr) (latex-render-symbol expr props))
    ((stringp expr) expr)
    (t (format nil "~A" expr))))

(defun math-render-atom (backend expr props)
  "render atom EXPR for BACKEND."
  (ecase backend
    (:latex (latex-render-atom expr props))))

;; main conversion function
;; node-level keywords take precedence over INHERITED-PROPS from parents.
(defun lisp-to-latex (expr &optional inherited-props)
  (cond
    ((atom expr)
     (math-render-atom :latex expr inherited-props))
    ((listp expr)
     (let ((head (car expr))
           (args (cdr expr)))
       (multiple-value-bind (operands keywords)
           (split-args args)
         (let ((props (append keywords inherited-props)))
           (let ((fn (find-math-op head)))
             (if fn
                 (funcall fn head operands props)
                 (latex-default head operands props)))))))
    (t (format nil "~A" expr))))

(register-math-op '+      #'latex-addition)
(register-math-op '-      #'latex-subtraction)
(register-math-op '*      #'latex-multiplication)
(register-math-op '/      #'latex-division)
(register-math-op 'matrix #'latex-matrix)
(register-math-op 'vector #'latex-vector)
(register-math-op 'set    #'latex-set)
(register-math-op '=      #'latex-equality)
(register-math-op 'expt   #'latex-expt)
(register-math-op 'set-builder #'latex-set-builder)

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
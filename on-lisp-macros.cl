(member "2" '("1" "2" "3"))
; => NIL

(member "2" '("1" "2" "3") :test #'string=)
; => (2 3)

(defmacro string-member (e es)
  `(member ,e ,es :test ,#'string=))

(string-member "2" '("1" "2" "3"))

(macroexpand '(string-member "x" '("x")))
; => (MEMBER "x" '("x") :TEST #<FUNCTION STRING=>)

(macroexpand '(defmacro x (y) `(+ 1 y)))

(defmacro the-expander (name) `(get ,name 'expander))

(defmacro the-defmacro (name params &body body)
  (let ((g (gensym)))
    `(progn
      (setf (the-expander ',name)
       #'(lambda (,g)
           (block ,name
             (destructuring-bind ,params (cdr ,g)
               ,@body))))
      ',name)))

(defun the-macroexpand-1 (expr)
  (if (and (consp expr) (the-expander (car expr)))
      (funcall (the-expander (car expr)) expr)
      expr))

(the-defmacro string-member1 (e es)
  `(member ,e ,es :test ,#'string=))

(the-macroexpand-1 '(string-member1 "x" '("x" "2")))
;; => (MEMBER "x" '("x" "2") :TEST #<FUNCTION STRING=>)

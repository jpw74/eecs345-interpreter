; Helper function to assist evaluate-expr
; Applies the appropriate mathematical operator
(define atom-to-func
  (lambda (atom)
    (cond
      ((eq? '* atom) *)
      ((eq? '/ atom) quotient)
      ((eq? '% atom) modulo)
      ((eq? '+ atom) +)
      ((eq? '- atom) -)
      ((eq? '< atom) <)
      ((eq? '<= atom) <=)
      ((eq? '> atom) >) 
      ((eq? '>= atom) >=) 
      ((eq? '== atom) equal?))))

(define class-def
  (lambda (static-env instance-env method-env parent)
    (list static-env instance-env method-env parent)))

; Returns the operator of a statement
; Takes a statement
(define operator
  (lambda (stmt)
    (car stmt)))

; Returns the first operand of a statement
; Takes a statement
(define operand1
  (lambda (stmt)
    (if (null? (cdr stmt))
      '()
      (car (cdr stmt)))))

; Returns the second operand of a statement
; Takes a statement
(define operand2
  (lambda (stmt)
    (if (null? (cdr (cdr stmt)))
      '()
      (car (cdr (cdr stmt))))))

; Returns the third operand of a statement
; Takes a statement
(define operand3
  (lambda (stmt)
    (if (null? (cdr (cdr (cdr stmt))))
      '()
      (car (cdr (cdr (cdr stmt)))))))

; Returns the formal arguments of the function closure
(define closure.formal
  (lambda (closure)
    (car closure)))

; Returns the body of the function closure
(define closure.body
  (lambda (closure)
    (car (cdr closure))))

; Returns the environment of the function closure
(define closure.environ
  (lambda (closure)
    (car (cdr (cdr closure)))))

(define class.static-env
  (lambda (class)
    (car class)))

(define class.instance-env
  (lambda (class)
    (cadr class)))

(define class.method-env
  (lambda (class)
    (caddr class)))

(define class.parent
  (lambda (class)
    (cadddr class)))

(define box
  (lambda (v)
    (list v)))

(define unbox
  (lambda (b)
    (car b)))

(define set-box!
  (lambda (b v)
    (set-car! b v)))

; A dummy function to be placed as a generic parameter
(define identity
  (lambda (v)
    v))

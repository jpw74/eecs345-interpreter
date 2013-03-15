; Timothy Sesler
; Jason Walsh
; EECS 345 - Interpreter Project, Part I
; 24 February 2013

; An interpreter for a simple Java-like language that handles variables, assignment 
; statements, mathematical expressions, comparison operators, boolean operators, simple 
; if statements, and return statements.

(load "verySimpleParser.scm")

; The main interpret function
; Takes a filename
(define interpret
  (lambda (filename)
    (lookup 'return (interpret-stmt-list (parser filename) (new-environ)))))

; Interprets a list of statements
; Takes a statement list and an environment
(define interpret-stmt-list
  (lambda (stmt-list environ)
    (cond
      ((null? stmt-list) '())
      ((null? (cdr stmt-list)) (interpret-stmt (car stmt-list) environ))
      (else (interpret-stmt-list (cdr stmt-list) (interpret-stmt (car stmt-list) environ))))))

; Interprets a general statement and calls the appropriate function
; Takes a statement and an environment
(define interpret-stmt
  (lambda (stmt environ)
      ; if there is a nested assignment statement, create a new environment by interpreting it 
      ; and pass the resulting environment to the parent statement
      (let ((ass (find-assignment stmt)))
        (if (null? ass)
            (cond
              ((null? stmt) environ) ; hack for now, not sure why/where interpret-stmt is getting called with ()
              ((eq? 'var (operator stmt)) (interpret-decl stmt environ))
              ((eq? '= (operator stmt)) (interpret-assign stmt environ))
              ((eq? 'return (operator stmt)) (interpret-return stmt environ))
              ((eq? 'if (operator stmt)) (interpret-if stmt environ)))
            (cond
              ((null? stmt) (interpret-assign ass environ))
              ((eq? 'var (operator stmt)) (interpret-decl stmt (interpret-assign ass environ)))
              ((eq? '= (operator stmt)) (interpret-assign stmt (interpret-assign ass environ)))
              ((eq? 'return (operator stmt)) (interpret-return stmt (interpret-assign ass environ)))
              ((eq? 'if (operator stmt)) (interpret-if stmt (interpret-assign ass environ))))))))

; Interprets variable declarations
; Takes a statement and an environment
(define interpret-decl
  (lambda (stmt environ)
    (lookup (operand1 stmt) environ)
    (if (null? (operand2 stmt))
      (add (operand1 stmt) '() environ)
      (add (operand1 stmt) (evaluate-expr (operand2 stmt) environ) environ))))

; Interprets assignment statements
; Takes a statement and an environment
; Works for arbitrarily deep nested assignments (i.e. a = b = c = 5), but it cannot return assignments inside expressions.
(define interpret-assign
  (lambda (stmt environ)
    (lookup (operand1 stmt) environ)
    (if (and (list? (operand2 stmt)) (eq? '= (operator (operand2 stmt))))
      (let ((new-env (interpret-assign (operand2 stmt) environ)))
        (add (operand1 stmt) (car (cdr (car new-env))) new-env))
      (add (operand1 stmt) (evaluate-expr (operand2 stmt) environ) environ))))

; Interprets return statements
; Takes a statement and an environment
(define interpret-return
  (lambda (stmt environ)
    (add 'return (evaluate-expr (operand1 stmt) environ) environ)))

; Interprets if statements
; Takes a statement and an environment
(define interpret-if
  (lambda (stmt environ)
    (if (evaluate-expr (operand1 stmt) environ)
        (interpret-stmt (operand2 stmt) environ)
        (interpret-stmt (operand3 stmt) environ))))

; Evaluates expressions and handles all mathematical operators in order of precedence
; Takes an expression and an environment
(define evaluate-expr
  (lambda (expr environ)
    (cond
      ((null? expr) environ)
      ((number? expr) expr)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((not (list? expr)) (lookup expr environ))
      ((and (eq? '- (operator expr)) (null? (operand2 expr))) (evaluate-expr (* -1 (lookup (operand1 expr) environ)) environ))      ; Unary Minus -
      ((eq? '!= (operator expr)) (not (equal? (evaluate-expr (operand1 expr) environ) (evaluate-expr (operand2 expr) environ))))    ; Not equal !=
      ((eq? '&& (operator expr)) (and (evaluate-expr (operand1 expr) environ) (evaluate-expr (operand2 expr) environ)))             ; Logical AND &&
      ((eq? '|| (operator expr)) (or (evaluate-expr (operand1 expr) environ) (evaluate-expr (operand2 expr) environ)))              ; Logical OR || 
      ((eq? '= (operator expr)) (car (cdr (car (interpret-assign expr environ)))))                  
      (else ((atom-to-func (operator expr)) (evaluate-expr (operand1 expr) environ) (evaluate-expr (operand2 expr) environ))))))


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

; Finds an assignment statement inside an expression if it exists
; Takes a statement
(define find-assignment
  (lambda (stmt)
    (cond
      ((null? stmt) '())
      ((and (list? (car stmt)) (eq? '= (operator (car stmt)))) (car stmt))
      (else (find-assignment (cdr stmt))))))
      
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

; Creates a new environment
; Takes no input
(define new-environ
  (lambda ()
    '()))

; Adds an element to the environment
; Takes a variable name, a value for that variable, and an environment
(define add
  (lambda (variable value environ)
    (cons (list variable value) environ)))

; Finds a specified element in the environment and returns its bound value
; Takes a variable name and an environment
(define lookup
  (lambda (variable environ)
    (cond
      ((null? environ) (error "Usage before declaring"))
      ((eq? variable (car (car environ))) (car (cdr (car environ))))
      (else (lookup variable (cdr environ))))))

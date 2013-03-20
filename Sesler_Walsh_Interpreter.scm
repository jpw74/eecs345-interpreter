; Timothy Sesler
; Jason Walsh
; EECS 345 - Interpreter Project, Part I
; 24 February 2013

; An interpreter for a simple Java-like language that handles variables, assignment 
; statements, mathematical expressions, comparison operators, boolean operators, simple 
; if statements, and return statements.

(load "loopSimpleParser.scm")

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
    (cond
      ((null? stmt) environ) ; hack for now, not sure why/where interpret-stmt is getting called with ()
      ((eq? 'var (operator stmt)) (interpret-decl stmt environ))
      ((eq? '= (operator stmt)) (interpret-assign stmt environ))
      ((eq? 'return (operator stmt)) (interpret-return stmt environ))
      ((eq? 'if (operator stmt)) (interpret-if stmt environ))
      ((eq? 'begin (operator stmt)) (interpret-block stmt environ)))))

; Interprets variable declarations
; Takes a statement and an environment
(define interpret-decl
  (lambda (stmt environ)
    (if (not (eq? 'null (shallow-lookup (operand1 stmt) environ))) (error "Redeclaring variable")
      (if (null? (operand2 stmt))
        (shallow-add (operand1 stmt) '() environ)
        (if (and (list? (operand2 stmt)) (eq? '= (operator (operand2 stmt))))
          (let ((new-env (interpret-assign (operand2 stmt) environ)))
            (shallow-add (operand1 stmt) (lookup (operand1 (operand2 stmt)) new-env) new-env))
          (shallow-add (operand1 stmt) (evaluate-expr (operand2 stmt) environ) environ))))))

; Interprets assignment statements
; Takes a statement and an environment
; Works for arbitrarily deep nested assignments (i.e. a = b = c = 5)
(define interpret-assign
  (lambda (stmt environ)
    (if (eq? 'null (lookup (operand1 stmt) environ)) (error "Using before declaring")
    (if (and (list? (operand2 stmt)) (eq? '= (operator (operand2 stmt))))
      (let ((new-env (interpret-assign (operand2 stmt) environ)))
        (add (operand1 stmt) (lookup (operand1 (operand2 stmt)) new-env) new-env))
      (add (operand1 stmt) (evaluate-expr (operand2 stmt) environ) environ)))))

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

(define interpret-block
  (lambda (stmt environ)
    (cdr (interpret-stmt-list (cdr stmt) (cons (new-environ) environ)))))

; Evaluates expressions and handles all mathematical operators in order of precedence
; Takes an expression and an environment
(define evaluate-expr
  (lambda (expr environ)
    (cond
      ((null? expr) environ)
      ((number? expr) expr)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((not (list? expr)) (cond
                            ((eq? 'null (lookup expr environ)) (error "Using before declaring"))
                            ((eq? '() (lookup expr environ)) (error "Using before assigning"))
                            (else (lookup expr environ))))
      ((and (eq? '- (operator expr)) (null? (operand2 expr))) (evaluate-expr (* -1 (evaluate-expr (operand1 expr) environ)) environ))      ; Unary Minus -
      ((eq? '!= (operator expr)) (not (equal? (evaluate-expr (operand1 expr) environ) (evaluate-expr (operand2 expr) environ))))    ; Not equal !=
      ((eq? '&& (operator expr)) (and (evaluate-expr (operand1 expr) environ) (evaluate-expr (operand2 expr) environ)))             ; Logical AND &&
      ((eq? '|| (operator expr)) (or (evaluate-expr (operand1 expr) environ) (evaluate-expr (operand2 expr) environ)))              ; Logical OR || 
      ((eq? '= (operator expr)) (lookup (operand1 expr) (interpret-assign expr environ)))                  
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
    '(()())))

; Adds an element to the environment
; Takes a variable name, a value for that variable, and an environment
(define add
  (lambda (variable value environ)
    (let ((b (get-box variable environ)))
      (if (eq? 'null b)
          (list (cons variable (vars environ)) (cons (box value) (vals environ)))
          (begin
            (set-box! b value) 
            environ)))))

(define shallow-add
  (lambda (variable value environ)
    (if (> (length environ) 2)
      (cons (add variable value (car environ)) (cdr environ))
      (add variable value environ))))

; Finds a specified element in the environment and returns its bound value
; Takes a variable name and an environment
(define lookup
  (lambda (variable environ)
    (if (> (length environ) 2)
      (let ((val (lookup variable (car environ))))
        (if (eq? val 'null)
          (lookup variable (cdr environ))
          val))
      (cond
        ((and (null? (vars environ)) (null? (vals environ))) 'null)
        ((eq? variable (car (vars environ))) (unbox (car (vals environ))))
        (else (lookup variable (list (cdr (vars environ)) (cdr (vals environ)))))))))

; only lookup within the current block - needed to check against redeclaring
(define shallow-lookup
  (lambda (variable environ)
    (if (> (length environ) 2)
      (lookup variable (car environ))
      (cond
        ((and (null? (vars environ)) (null? (vals environ))) 'null)
        ((eq? variable (car (vars environ))) (unbox (car (vals environ))))
        (else (lookup variable (list (cdr (vars environ)) (cdr (vals environ)))))))))

(define vars
  (lambda (environ)
    (car environ)))

(define vals
  (lambda (environ)
    (car (cdr environ))))

(define get-box
  (lambda (variable environ)
    (if (> (length environ) 2)
      (let ((b (get-box variable (car environ))))
        (if (eq? b 'null)
          (get-box variable (cdr environ))
          b))
      (cond
        ((and (null? (vars environ)) (null? (vals environ))) 'null)
        ((eq? variable (car (vars environ))) (car (vals environ)))
        (else (get-box variable (list (cdr (vars environ)) (cdr (vals environ)))))))))

(define shallow-get-box
  (lambda (variable environ)
    (if (> (length environ) 2)
      (get-box variable (car environ))
      (cond
        ((and (null? (vars environ)) (null? (vals environ))) 'null)
        ((eq? variable (car (vars environ))) (car (vals environ)))
        (else (get-box variable (list (cdr (vars environ)) (cdr (vals environ)))))))))

(define box
  (lambda (v)
    (list v)))

(define unbox
  (lambda (b)
    (car b)))

(define set-box!
  (lambda (b v)
    (set-car! b v)))

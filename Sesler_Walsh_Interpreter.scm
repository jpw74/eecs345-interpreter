; Timothy Sesler
; Jason Walsh
; EECS 345 - Interpreter Project, Part II
; 20 March 2013

; An interpreter for a simple Java-like language that handles variables, assignment 
; statements, mathematical expressions, comparison operators, boolean operators, simple 
; if statements, and return statements.

(load "functionParser.scm")

; The main interpret function
; Takes a filename
(define interpret
  (lambda (filename)
    (interpret-funcall '(funcall main) (interpret-stmt-list (parser filename) (new-environ) identity identity identity) identity identity identity)))

; Interprets a list of statements
; Takes a statement list and an environment
(define interpret-stmt-list
  (lambda (stmt-list environ return continue break)
    (cond
      ((null? stmt-list) '())
      ((null? (cdr stmt-list)) (interpret-stmt (car stmt-list) environ return continue break))
      (else (interpret-stmt-list (cdr stmt-list) (interpret-stmt (car stmt-list) environ return continue break) return continue break)))))

; Interprets a general statement and calls the appropriate function
; Takes a statement and an environment
(define interpret-stmt
  (lambda (stmt environ return continue break)
               (cond
                 ((null? stmt) environ) ; hack for now, not sure why/where interpret-stmt is getting called with ()
                 ((eq? 'var (operator stmt)) (interpret-decl stmt environ))
                 ((eq? '= (operator stmt)) (interpret-assign stmt environ))
                 ((eq? 'return (operator stmt)) (interpret-return stmt environ return))
                 ((eq? 'if (operator stmt)) (interpret-if stmt environ return continue break))
                 ((eq? 'begin (operator stmt)) (interpret-block stmt environ return continue break))
                 ((eq? 'while (operator stmt)) (interpret-while stmt environ return))
                 ((eq? 'function (operator stmt)) (interpret-fundef stmt environ))
                 ((eq? 'funcall (operator stmt)) (interpret-funcall stmt environ return continue break))
                 ((eq? 'break (operator stmt)) (break environ))
                 ((eq? 'continue (operator stmt)) (continue environ)))))

; Interprets variable declarations
; Takes a statement and an environment
; For a declaration, we want to use shallow-add - we are allowed to redeclare
;   variables within a block
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
; For an assignment, we want to deeply add - reassigning within a block affects
;   containing blocks
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
  (lambda (stmt environ return)
    (let ((ret-val (evaluate-expr (operand1 stmt) environ)))
      (cond
        ((eq? #t ret-val) (return 'true))
        ((eq? #f ret-val) (return 'false))
        (else (return ret-val))))))
    

; Interprets if statements
; Takes a statement and an environment
(define interpret-if
  (lambda (stmt environ return continue break)
    (if (evaluate-expr (operand1 stmt) environ)
        (interpret-stmt (operand2 stmt) environ return continue break)
        (interpret-stmt (operand3 stmt) environ return continue break))))

; Interprets while loops
; Takes a statement, an environment, and a return
(define interpret-while
  (lambda (stmt environ return)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condition body environ)
                                (if (evaluate-expr condition environ)
                                  (loop condition body (interpret-stmt body environ return (lambda (env) (loop condition body env)) break))
                                  (break environ)))))
                 (loop (operand1 stmt) (operand2 stmt) environ))))))
  
(define interpret-fundef
  (lambda (stmt environ)
    (let ((name (operand1 stmt)) (args (operand2 stmt)) (body (operand3 stmt)))
      (add name (list args body environ) environ))))

(define interpret-funcall
  (lambda (stmt environ return continue break)
    (call/cc (lambda (funreturn)
               (let* ((name (operand1 stmt)) 
                      (closure (lookup name environ)) 
                      (formal (closure-formal closure)) 
                      (body (closure-body closure)) 
                      (def-env (closure-environ closure)) 
                      (actual (cdr (cdr stmt)))
                      (call-env (bind-actual-formal actual formal (layer def-env))))
                 (interpret-stmt-list body call-env funreturn continue break))))))


(define bind-actual-formal
  (lambda (actual formal environ)
    (cond
      ((and (null? actual) (null? formal)) environ)
      (else (add (car formal) (evaluate-expr (car actual) environ) (bind-actual-formal (cdr actual) (cdr formal) environ))))))

(define interpret-block
  (lambda (stmt environ return continue break)
    (unlayer (interpret-stmt-list (cdr stmt) (layer environ) return (lambda (env) (continue (cdr env))) (lambda (env) (break (cdr env))))))) ; pass in new continue and break that pop the current layer

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
      ((eq? '! (operator expr)) (not (evaluate-expr (operand1 expr) environ)))
      ((eq? '= (operator expr)) (lookup (operand1 expr) (interpret-assign expr environ)))                  
      ((eq? 'funcall (operator expr)) (interpret-funcall expr environ identity identity identity))
      (else ((atom-to-func (operator expr)) (evaluate-expr (operand1 expr) environ) (evaluate-expr (operand2 expr) environ))))))

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

(define closure-formal
  (lambda (closure)
    (car closure)))

(define closure-body
  (lambda (closure)
    (car (cdr closure))))

(define closure-environ
  (lambda (closure)
    (car (cdr (cdr closure)))))

; Creates a new environment
; Takes no input
(define new-environ
  (lambda ()
    '(()())))

(define layer
  (lambda (environ)
    (cons (new-environ) environ)))

(define unlayer
  (lambda (environ)
    (cdr environ)))

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

(define identity
  (lambda (v)
    v))

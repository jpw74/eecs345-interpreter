; Timothy Sesler
; Jason Walsh
; EECS 345 - Interpreter Project, Part IV
; 17 April 2013

; An interpreter for a simple Java-like language that handles variables, assignment 
;   statements, function declaration, function execution (call-by-value and call-by-reference), 
;   mathematical expressions, comparison operators, boolean operators, while loops, simple if 
;   statements, and return statements.

(load "classParser.scm")

; The main interpret function
; Takes a filename
(define interpret
  (lambda (filename class)
    (interpret-funcall '(funcall main) (interpret-class-list (parser filename) (new-environ)) identity identity identity (string->symbol class) identity)))
    ;(interpret-funcall '(funcall main) (interpret-stmt-list (parser filename) (new-environ) identity identity identity) identity identity identity)))

(define interpret-class-list
  (lambda (class-list environ)
    (cond
      ((null? class-list) '())
      ((null? (cdr class-list)) (interpret-class (car class-list) environ))
      (else (interpret-class-list (cdr class-list) (interpret-class (car class-list) environ))))))

; adds the given class to the environment
(define interpret-class
  (lambda (class environ)
    (let* ((name (operand1 class))
          (parent (if (null? (operand2 class)) 'null (cadr (operand2 class))))
          (body (operand3 class))
          (class-env (list (new-environ) (new-environ) (new-environ) parent)))
      (add name (interpret-class-body body class-env) environ))))

(define interpret-class-body
  (lambda (body class-env)
      (cond
        ((null? body) '())
        ((null? (cdr body)) (interpret-class-body-stmt (car body) class-env))
        (else (interpret-class-body (cdr body) (interpret-class-body-stmt (car body) class-env))))))

(define interpret-class-body-stmt
  (lambda (body-stmt class-env)
    (let ((static-env (class.static-env class-env))
          (instance-env (class.instance-env class-env))
          (method-env (class.method-env class-env))
          (parent (class.parent class-env)))
      (cond
        ((eq? 'static-var (operator body-stmt)) (list (interpret-decl body-stmt static-env class-env instance-env) instance-env method-env parent))
        ((eq? 'static-function (operator body-stmt)) (list static-env instance-env (interpret-fundef body-stmt method-env) parent))))))


; Interprets a list of statements
; Takes a statement list and an environment
(define interpret-stmt-list
  (lambda (stmt-list environ return continue break class instance)
    (cond
      ((null? stmt-list) '())
      ((null? (cdr stmt-list)) (interpret-stmt (car stmt-list) environ return continue break class instance))
      (else (interpret-stmt-list (cdr stmt-list) (interpret-stmt (car stmt-list) environ return continue break class instance) return continue break class instance)))))

; Interprets a general statement and calls the appropriate function
; Takes a statement and an environment
(define interpret-stmt
  (lambda (stmt environ return continue break class instance)
    (cond
      ((null? stmt) environ) ; hack for now, not sure why/where interpret-stmt is getting called with ()
      ((eq? 'var (operator stmt)) (interpret-decl stmt environ))
      ((eq? '= (operator stmt)) (interpret-assign stmt environ))
      ((eq? 'return (operator stmt)) (interpret-return stmt environ return class instance))
      ((eq? 'if (operator stmt)) (interpret-if stmt environ return continue break))
      ((eq? 'begin (operator stmt)) (interpret-block stmt environ return continue break))
      ((eq? 'while (operator stmt)) (interpret-while stmt environ return))
      ((eq? 'function (operator stmt)) (interpret-fundef stmt environ))
      ((eq? 'funcall (operator stmt)) (interpret-funcall stmt environ return continue break class instance))
      ((eq? 'super (operator stmt)) (class.parent class))
      ;((eq? 'dot (operator stmt)) (interpret-dot stmt class instance))      
      ((eq? 'break (operator stmt)) (break environ))
      ((eq? 'continue (operator stmt)) (continue environ)))))

; Interprets variable declarations
; Takes a statement and an environment
; For a declaration, we want to use shallow-add - we are allowed to redeclare
;   variables within a block
(define interpret-decl
  (lambda (stmt environ class instance)
    (if (not (eq? 'null (shallow-lookup (operand1 stmt) environ))) (error "Redeclaring variable")
      (if (null? (operand2 stmt))
        (shallow-add (operand1 stmt) '() environ)
        (if (and (list? (operand2 stmt)) (eq? '= (operator (operand2 stmt))))
          (let ((new-env (interpret-assign (operand2 stmt) environ class instance)))
            (shallow-add (operand1 stmt) (lookup (operand1 (operand2 stmt)) new-env) new-env))
          (shallow-add (operand1 stmt) (evaluate-expr (operand2 stmt) environ class instance) environ))))))

; Interprets assignment statements
; Takes a statement and an environment
; Works for arbitrarily deep nested assignments (i.e. a = b = c = 5)
; For an assignment, we want to deeply add - reassigning within a block affects
;   containing blocks
(define interpret-assign
  (lambda (stmt environ class instance)
    (if (eq? 'null (lookup (operand1 stmt) environ)) (error "Using before declaring")
    (if (and (list? (operand2 stmt)) (eq? '= (operator (operand2 stmt))))
      (let ((new-env (interpret-assign (operand2 stmt) environ)))
        (add (operand1 stmt) (lookup (operand1 (operand2 stmt)) new-env) new-env))
      (add (operand1 stmt) (evaluate-expr (operand2 stmt) environ) environ)))))

; Interprets return statements
; Takes a statement and an environment
(define interpret-return
  (lambda (stmt environ return class instance)
    (let ((ret-val (evaluate-expr (operand1 stmt) environ class instance)))
      (cond
        ((eq? #t ret-val) (return 'true))
        ((eq? #f ret-val) (return 'false))
        (else (return ret-val))))))
    

; Interprets if statements
; Takes a statement and an environment
(define interpret-if
  (lambda (stmt environ return continue break class instance)
    (if (evaluate-expr (operand1 stmt) environ)
        (interpret-stmt (operand2 stmt) environ return continue break)
        (interpret-stmt (operand3 stmt) environ return continue break))))

; Interprets while loops
; Takes a statement, an environment, and a return
(define interpret-while
  (lambda (stmt environ return class instance)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condition body environ)
                                (if (evaluate-expr condition environ)
                                  (loop condition body (interpret-stmt body environ return (lambda (env) (loop condition body env)) break))
                                  (break environ)))))
                 (loop (operand1 stmt) (operand2 stmt) environ))))))

; Defines functions in the active environment
; Takes a statement and an environment
(define interpret-fundef
  (lambda (stmt environ)
    (let ((name (operand1 stmt)) (formal (operand2 stmt)) (body (operand3 stmt)))
      (add name (list formal body (recursive-closure name formal body)) environ))))

; Creates the closure of a function
; Takes the function name, formal arguments, and body
(define recursive-closure
  (lambda (name formal body)
    (lambda (environ)
      (add name (list formal body (recursive-closure name formal body)) environ))))

; Executes functions when they are called
; Takes a statement, an environment, return, continue, and break
(define interpret-funcall
  (lambda (stmt environ return continue break class instance)
    (call/cc (lambda (funreturn)               
               ;(if (eq? (lookup (operand1 stmt) environ) 'null)
                 ;(error "Undefined function")              
               ;(begin (display "Statement: ") (display stmt) (newline) (display "Environ: ") (display environ) (newline) (display "Class: ") (display class) (newline)
       (let* (;(class (if (and (pair? (operand1 stmt)) (eq? 'dot (operator (operand1 stmt))))
               ;         (operand1 (operand1 stmt))))
                ;        (name (if (and (pair? (operand1 stmt)) (eq? 'dot (operator (operand1 stmt))))
                 ;                 (operand2 (operand1 stmt))
                  ;                (operand1 stmt)))
              (name (operand1 stmt))
                        (closure (cls-lookup name class environ)) 
                        (formal (closure-formal closure))
                        (body (closure-body closure))
                        (def-env ((closure-environ closure) environ))
                        (actual (cdr (cdr stmt)))
                        (call-env (bind-actual-formal actual formal (layer def-env) class instance)))
                   (interpret-stmt-list body call-env funreturn continue break class instance))))))

;(define interpret-dot
 ; (lambda (stmt environ)
  ;  (let ((class (operand1 stmt))
   ;       (name (operand2 stmt)))
    ;  (interpret-funcall name environ return continue break class instance))))

; Binds the actual parameters of the function to the formal parameters used inside
; Takes the actual parameters, formal parameters, and an environment
(define bind-actual-formal
  (lambda (actual formal environ class instance)
    (cond
      ((and (null? actual) (null? formal)) environ)
      ((eq? (car formal) '&) (add-box (car (cdr formal)) (get-box (car actual) environ) (bind-actual-formal (cdr actual) (cdr (cdr formal)) environ)))
      (else (add (car formal) (evaluate-expr (car actual) environ class instance) (bind-actual-formal (cdr actual) (cdr formal) environ class instance))))))

; Interprets blocks of code
; Takes a statement, environment, return, continue, and break
(define interpret-block
  (lambda (stmt environ return continue break class instance)
    (unlayer (interpret-stmt-list (cdr stmt) (layer environ) return (lambda (env) (continue (cdr env))) (lambda (env) (break (cdr env))) class instance)))) ; pass in new continue and break that pop the current layer

; Evaluates expressions and handles all mathematical operators in order of precedence
; Takes an expression and an environment
(define evaluate-expr
  (lambda (expr environ class instance)
    (cond
      ((null? expr) environ)
      ((number? expr) expr)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((not (list? expr)) (cond
                            ((eq? 'null (cls-lookup expr class environ)) (error "Using before declaring"))
                            ((eq? '() (cls-lookup expr class environ)) (error "Using before assigning"))
                            (else (cls-lookup expr class environ))))
      ((and (eq? '- (operator expr)) (null? (operand2 expr))) (evaluate-expr (* -1 (evaluate-expr (operand1 expr) environ class instance)) environ class instance))      ; Unary Minus -
      ((eq? '!= (operator expr)) (not (equal? (evaluate-expr (operand1 expr) environ class instance) (evaluate-expr (operand2 expr) environ class instance))))    ; Not equal !=
      ((eq? '&& (operator expr)) (and (evaluate-expr (operand1 expr) environ class instance) (evaluate-expr (operand2 expr) environ class instance)))             ; Logical AND &&
      ((eq? '|| (operator expr)) (or (evaluate-expr (operand1 expr) environ class instance) (evaluate-expr (operand2 expr) environ class instance)))              ; Logical OR || 
      ((eq? '! (operator expr)) (not (evaluate-expr (operand1 expr) environ class instance)))
      ((eq? 'dot (operator expr)) (cls-lookup (operand2 expr) (operand1 expr) environ))
      ((eq? '= (operator expr)) (lookup (operand1 expr) (interpret-assign expr environ)))                  
      ((eq? 'funcall (operator expr)) (interpret-funcall expr environ identity identity identity class instance))
      (else ((atom-to-func (operator expr)) (evaluate-expr (operand1 expr) environ class instance) (evaluate-expr (operand2 expr) environ class instance))))))

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

; Returns the formal arguments of the function closure
(define closure-formal
  (lambda (closure)
    (car closure)))

; Returns the body of the function closure
(define closure-body
  (lambda (closure)
    (car (cdr closure))))

; Returns the environment of the function closure
(define closure-environ
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

; Creates a new environment
; Takes no input
(define new-environ
  (lambda ()
    '(()())))

; Adds a layer onto the specified environment
; Takes an environment to add a layer on
(define layer
  (lambda (environ)
    (cons (new-environ) environ)))

; Pops the first layer off of the specified environment
; Takes an environment
(define unlayer
  (lambda (environ class instance)
    (cdr environ)))

; Adds a box to the environment
; Takes a name, a box, and an envirnoment
(define add-box
  (lambda (name b environ)
    (if (> (length environ) 2)
      (cons (add-box name b (car environ)) (cdr environ))
      (list (cons name (vars environ)) (cons b (vals environ))))))

; Adds an element to the environment
; Takes a variable name, a value for that variable, and an environment
(define add
  (lambda (variable value environ)
    (let ((b (get-box variable environ)))
      (if (eq? 'null b)
        (if (> (length environ) 2)
          (cons (add variable value (car environ)) (cdr environ))
          (list (cons variable (vars environ)) (cons (box value) (vals environ))))
        (begin
          (set-box! b value) 
          environ)))))

; Adds an element to a 
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

(define cls-lookup
  (lambda (name class environ)
    (let* ((class-env (lookup class environ))
           (static-var (lookup name (class.static-env class-env)))
           (method (lookup name (class.method-env class-env)))
           (parent (class.parent class-env)))
      (if (eq? static-var 'null)
        (if (eq? method 'null)
          (cls-lookup name parent environ)
          method)
        static-var))))

; Checks for redeclaration of variables
; Takes a variable and an environment
(define shallow-lookup
  (lambda (variable environ)
    (if (> (length environ) 2)
      (lookup variable (car environ))
      (cond
        ((and (null? (vars environ)) (null? (vals environ))) 'null)
        ((eq? variable (car (vars environ))) (unbox (car (vals environ))))
        (else (lookup variable (list (cdr (vars environ)) (cdr (vals environ)))))))))

; Returns the car of the specified environment (the variable name)
(define vars
  (lambda (environ)
    (car environ)))

; Returns the car of the cdr of the specfied environment (the value bound to the variable)
(define vals
  (lambda (environ)
    (car (cdr environ))))

; Returns the box containing the specified variable
; Takes a variable and an environment
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

; Returns the box containing the specified variable
; Takes a variable and an environment
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

; A dummy function to be placed as a generic parameter
(define identity
  (lambda (v)
    v))

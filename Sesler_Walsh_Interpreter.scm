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
                        (formal (closure.formal closure))
                        (body (closure.body closure))
                        (def-env ((closure.environ closure) environ))
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

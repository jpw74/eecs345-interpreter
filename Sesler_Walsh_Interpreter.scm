; Timothy Sesler
; Jason Walsh
; EECS 345 - Interpreter Project, Part V
; 29 April 2013

; An interpreter for a simple Java-like language that handles variables, assignment 
;   statements, function declaration, function execution (call-by-value and call-by-reference), 
;   mathematical expressions, comparison operators, boolean operators, while loops, simple if 
;   statements, and return statements.

(load "classParser.scm")
;(load "environment.scm")
;(load "lib.scm")

; As you've probably noticed, this section of the interpreter is incredibly incomplete.  We simply do not know how to translate the ideas explained in class into workable code.
;
;The ideas as we would have implemented them are explained below:
;=============================================================================================================================================================================
;- The 'new' operator
;	: interpret-new should evaluate the class name and look it up in the environment
;	: it should look for a constructor in that class
;	: it must then create an instance (list of variables and a class)
;	: it then should initialize the class and run the constructor
;	: if dealing with a subclass, the parent class's constructor should be called instead
;		= check the first line of the constructor for a constructor call
;		= if it exists, use it
;		= if not, use the parent constructor by default
;		= initialize all instance field values by using the initial expression or assign values of 0
;		= then execute the remainder of the construction body
;	: return the instance
;
;- Throwing and catching exceptions
;	: There are four ways to exit from a try / catch block
;		1. Execution reaches the end of the block
;		2. An exception is thrown
;		3. Return statements
;		4. Break and continue
;	: when a catch block is interpreted, set up a continuation using call/cc to jump to that point in the code
;	: interpret-catch has this initial structure: (lambda () (interpret-block body-of-catch (lambda (v) (add exception-name v current-environ)) return break continue class instance)
;	: the previous is the continuation to be passed when catch is interpreted	
;	: in interpret-stmt we would include the line ((eq? (operator stmt) 'throw (throw (evaluate-expr (operand1 stmt)))
;	: interpret-stmt would also be changed to include a 'throw' in its input parameters






; ====================================================================================================================================
; Interpreter starts here
; ====================================================================================================================================

; The main interpret function
; Takes a filename
(define interpret
  (lambda (filename class-name)
    (let* ((environ (interpret-class-list (parser filename) (new-environ) (lambda (v) v)))
           (class (lookup-main (string->symbol class-name) environ)))
    (interpret-funcall '(funcall main) environ identity identity identity class identity))))

(define interpret-class-list
  (lambda (class-list environ k)
    (cond
      ((null? class-list) (k environ))
      ((null? (cdr class-list)) (k (interpret-class (car class-list) environ)))
      ((list? (car class-list)) (interpret-class-list (car class-list) environ (lambda (env) (interpret-class-list (cdr class-list) env (lambda (v) v)))))
      (else (k (interpret-class class-list environ))))))
      ;(else (interpret-class-list (cdr class-list) (interpret-class (car class-list) environ))))))

; adds the given class to the environment
(define interpret-class
  (lambda (class environ)
    (let* ((name (operand1 class))
          (parent (if (null? (operand2 class)) 'null (cadr (operand2 class))))
          (body (operand3 class))
          (class-env (class-def (new-environ) (new-environ) (new-environ) parent)))
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
        ((eq? 'static-var (operator body-stmt)) (class-def (interpret-decl body-stmt static-env class-env instance-env) instance-env method-env parent))
        ((eq? 'static-function (operator body-stmt)) (class-def static-env instance-env (interpret-fundef body-stmt method-env) parent))))))


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
      ((eq? 'var (operator stmt)) (interpret-decl stmt environ class instance))
      ((eq? '= (operator stmt)) (interpret-assign stmt environ class instance))
      ((eq? 'return (operator stmt)) (interpret-return stmt environ return class instance))
      ((eq? 'if (operator stmt)) (interpret-if stmt environ return continue break class instance))
      ((eq? 'begin (operator stmt)) (interpret-block stmt environ return continue break class instance))
      ((eq? 'while (operator stmt)) (interpret-while stmt environ return class instance))
      ((eq? 'function (operator stmt)) (interpret-fundef stmt environ class instance))
      ((eq? 'funcall (operator stmt)) (interpret-funcall stmt environ return continue break class instance))
      ((eq? 'break (operator stmt)) (break environ))
      ((eq? 'continue (operator stmt)) (continue environ))
      ;((eq? (operator stmt) 'throw (throw (evaluate-expr (operand1 stmt)))))
      ((eq? 'new (operator stmt)) (error "New doesn't exist. Why are you using  this?")))))

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
            (shallow-add (operand1 stmt) (lookup (operand1 (operand2 stmt)) new-env) name class instance new-env))
          (shallow-add (operand1 stmt) (evaluate-expr (operand2 stmt) environ class instance) environ))))))

; Interprets assignment statements
; Takes a statement and an environment
; Works for arbitrarily deep nested assignments (i.e. a = b = c = 5)
; For an assignment, we want to deeply add - reassigning within a block affects
;   containing blocks
(define interpret-assign
  (lambda (stmt environ class instance)
    (if (eq? 'null (lookup (operand1 stmt) class instance environ)) (error "Using before declaring")
    (if (and (list? (operand2 stmt)) (eq? '= (operator (operand2 stmt))))
      (let ((new-env (interpret-assign (operand2 stmt) environ)))
        (add (operand1 stmt) (lookup (operand1 (operand2 stmt)) class instance new-env) new-env))
      (add (operand1 stmt) (evaluate-expr (operand2 stmt) environ class instance) environ)))))

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
    (if (evaluate-expr (operand1 stmt) environ class instance)
        (interpret-stmt (operand2 stmt) environ return continue break class instance)
        (interpret-stmt (operand3 stmt) environ return continue break class instance))))

; Interprets while loops
; Takes a statement, an environment, and a return
(define interpret-while
  (lambda (stmt environ return class instance)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condition body environ)
                                (if (evaluate-expr condition environ class instance)
                                  (loop condition body (interpret-stmt body environ return (lambda (env) (loop condition body env)) break class instance))
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
               (if (list? (operand1 stmt))
                 (let* ((name (operand2 (operand1 stmt)))
                        ; TODO this sucks and probably won't work
                        (closure (interpret-dot (operand1 stmt) environ class instance))
                        (formal (closure.formal closure))
                        (body (closure.body closure))
                        (def-env ((closure.environ closure) environ))
                        (actual (cdr (cdr stmt)))
                        (call-env (bind-actual-formal actual formal (layer def-env) class instance)))
                   (interpret-stmt-list body call-env funreturn continue break class instance))
               ;(if (eq? (lookup-in-class (operand1 stmt) class instance environ) 'null)
                 ;(error "Undefined function")              
                 (let* ((name (operand1 stmt))
                        (closure (lookup name class instance environ)) 
                        (formal (closure.formal closure))
                        (body (closure.body closure))
                        (def-env ((closure.environ closure) environ))
                        (actual (cdr (cdr stmt)))
                        (call-env (bind-actual-formal actual formal (layer def-env) class instance)))
                   (interpret-stmt-list body call-env funreturn continue break class instance)))))))

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

(define interpret-dot
  (lambda (stmt environ class instance)
    (if (eq? (operand1 stmt) 'super)
      (let ((parent (lookup-main (class.parent class) environ)))
            (lookup-in-class (operand2 stmt) parent instance environ))
      (lookup-in-class (operand2 stmt) class instance environ))))
      
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
                            ((eq? 'null (lookup expr class instance environ)) (error "Using before declaring"))
                            ((eq? '() (lookup expr class instance environ)) (error "Using before assigning"))
                            (else (lookup expr class instance environ))))
      ((and (eq? '- (operator expr)) (null? (operand2 expr))) (evaluate-expr (* -1 (evaluate-expr (operand1 expr) environ class instance)) environ class instance))      ; Unary Minus -
      ((eq? '!= (operator expr)) (not (equal? (evaluate-expr (operand1 expr) environ class instance) (evaluate-expr (operand2 expr) environ class instance))))    ; Not equal !=
      ((eq? '&& (operator expr)) (and (evaluate-expr (operand1 expr) environ class instance) (evaluate-expr (operand2 expr) environ class instance)))             ; Logical AND &&
      ((eq? '|| (operator expr)) (or (evaluate-expr (operand1 expr) environ class instance) (evaluate-expr (operand2 expr) environ class instance)))              ; Logical OR || 
      ((eq? '! (operator expr)) (not (evaluate-expr (operand1 expr) environ class instance)))
      ((eq? 'dot (operator expr)) (interpret-dot expr environ class instance))
      ((eq? '= (operator expr)) (lookup (operand1 expr) class instance (interpret-assign expr environ)))                  
      ((eq? 'funcall (operator expr)) (interpret-funcall expr environ identity identity identity class instance))
      (else ((atom-to-func (operator expr)) (evaluate-expr (operand1 expr) environ class instance) (evaluate-expr (operand2 expr) environ class instance))))))

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
  (lambda (environ)
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

; takes a name and an environment and returns a value
; environment should be structured as a list of variables and a list of values
(define lookup-main
  (lambda (variable environ)
    (if (> (length environ) 2)
      (let ((val (lookup-main variable (car environ))))
        (if (eq? val 'null)
          (lookup-main variable (cdr environ))
          val))
      (cond
        ((and (null? (vars environ)) (null? (vals environ))) 'null)
        ((eq? variable (car (vars environ))) (unbox (car (vals environ))))
        (else (lookup-main variable (list (cdr (vars environ)) (cdr (vals environ)))))))))

(define lookup-in-class 
  (lambda (name class instance environ)
    (let* ((static-var (lookup-main name (class.static-env class)))
           (method (lookup-main name (class.method-env class)))
           (parent (lookup-main (class.parent class) environ)))
      (if (eq? static-var 'null)
        (if (eq? method 'null)
          (lookup-in-class name parent instance environ)
          method)
        static-var))))

(define lookup
  (lambda (name class instance environ)
    (if (eq? (lookup-main name environ) 'null)
      (lookup-in-class name class instance environ)
      (lookup-main name environ))))

; Checks for redeclaration of variables
; Takes a variable and an environment
(define shallow-lookup
  (lambda (variable environ)
    (if (> (length environ) 2)
      (lookup-main variable (car environ))
      (cond
        ((and (null? (vars environ)) (null? (vals environ))) 'null)
        ((eq? variable (car (vars environ))) (unbox (car (vals environ))))
        (else (lookup-main variable (list (cdr (vars environ)) (cdr (vals environ)))))))))

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

;(define box
 ; (lambda (v)
  ;  (list v)))

;(define unbox
 ; (lambda (b)
  ;  (car b)))

;(define set-box!
 ; (lambda (b v)
  ;  (set-car! b v)))

; A dummy function to be placed as a generic parameter
;(define identity
 ; (lambda (v)
  ;  v))

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
    (let* ((class-env (lookup-main class environ))
           (static-var (lookup-main name (class.static-env class-env)))
           (method (lookup-main name (class.method-env class-env)))
           (parent (class.parent class-env)))
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

(load "Sesler_Walsh_Interpreter.scm")

(define runtest
  (lambda (file result)
    (display file)
    (display ": ")
    (if (eq? (interpret file) result)
      (display "Passed")
      (display "Failed"))
    (newline)))

(runtest "tests/test1.x" 150)
(runtest "tests/test2.x" -4)
(runtest "tests/test3.x" 10)
(runtest "tests/test4.x" 16)
(runtest "tests/test5.x" 220)
(runtest "tests/test6.x" 5)
(runtest "tests/test7.x" 6)
(runtest "tests/test8.x" 10)
(runtest "tests/test9.x" 5)
(runtest "tests/test10.x" -39)
; test11, test12, test13, and test14 should error

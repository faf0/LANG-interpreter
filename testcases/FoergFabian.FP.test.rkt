#lang eopl

(require "../FoergFabian.FP.rkt")
(require "../FoergFabian.FP.expansion.rkt")
(require "../FoergFabian.FP.eval.rkt")

; TEST CASES

(define myinstrs
  (list (labeled 'A (add1 'X))
        (unlabeled (if-goto 'Z 'A))
        (labeled 'C (skip 'Z2))
        (unlabeled (sub1 'Y))))

(define myinstrs2
  (list (labeled 'A (goto-L 'L))
        (unlabeled (zero-V 'X))
        (unlabeled (assign 'X1 'X2))))

(define myprog (a-program myinstrs))

(define myprog2 (a-program myinstrs2))

(define myprog-string
  "(A) X <- X + 1\n\tIF Z =/= 0 GOTO A\n(C) Z2 <- Z2\n\tY <- Y - 1\n")

(define myprog2-string
  "(A) GOTO L\n\tX <- 0\n\tX1 <- X2\n")

(define myprog3-string
  "(A3) Z4 <- Z4 + 1\n\tIF Z6 =/= 0 GOTO A3\n\tZ7 <- Z7 + 1\n\tIF Z7 =/= 0 GOTO A4\n\tZ5 <- Z5 + 1\n(A4) Z3 <- Z3 - 1\n")

(define myprog4-string
  "(A) Z2 <- Z2 + 1\n\tIF Z2 =/= 0 GOTO B\n")

(define myprog5-string
  "\tGOTO A7\n(A6) Z4 <- Z4 - 1\n(A7) IF Z4 =/= 0 GOTO A6\n(A) GOTO A9\n(A8) Z5 <- Z5 - 1\n(A9) IF Z5 =/= 0 GOTO A8\n")

(define myprog6-string-b
  (string-append "X10 <- 0\n\tZ12 <- 0\n\tIF X11 =/= 0 GOTO A14\n\tGOTO A15\n(A14) X11 <- X11 - 1\n\t"
                 "X10 <- X10 + 1\n\tZ12 <- Z12 + 1\n\tIF X11 =/= 0 GOTO A14\n(A15) X10 <- X10\n\t"
                 "X11 <- 0\n\tZ13 <- 0\n\tIF Z12 =/= 0 GOTO A16\n\tGOTO A17\n(A16) Z12 <- Z12 - 1\n\t"
                 "X11 <- X11 + 1\n\tZ13 <- Z13 + 1\n\tIF Z12 =/= 0 GOTO A16\n(A17) X11 <- X11\n"))

(define myprog6-string-unlabeled
  (string-append "\t" myprog6-string-b))

(define myprog6-string-labeled
  (string-append "(A) " myprog6-string-b))

(define my-s-list
'(
(A) X <- X + 1
    IF Z =/= 0 GOTO A
(C) Z2 <- Z2
    Y <- Y - 1
))

(define my-s-list2
'(
(A) GOTO L
    X <- 0
    X1 <- X2
))

(define my-s-list3
'(
(A3) Z4 <- Z4 + 1
     IF Z6 =/= 0 GOTO A3
     GOTO A4
     Z5 <- Z5 + 1
(A4) Z3 <- Z3 - 1
))

(define my-s-list4
'(
(A) GOTO B
))

(define my-s-list5
'(
    Z4 <- 0
(A) Z5 <- 0
))

(define my-s-list7
'(
(A) Z26 <- Z26 + 1
    IF Z26 =/= 0 GOTO A19
(A18) X10 <- X10 - 1
(A19) IF X10 =/= 0 GOTO A18
    Z27 <- Z27 + 1
    IF Z27 =/= 0 GOTO A21
(A20) Z12 <- Z12 - 1
(A21) IF Z12 =/= 0 GOTO A20
    IF X11 =/= 0 GOTO A14
    Z28 <- Z28 + 1
    IF Z28 =/= 0 GOTO A15
(A14) X11 <- X11 - 1
    X10 <- X10 + 1
    Z12 <- Z12 + 1
    IF X11 =/= 0 GOTO A14
(A15) X10 <- X10
    Z29 <- Z29 + 1
    IF Z29 =/= 0 GOTO A23
(A22) X11 <- X11 - 1
(A23) IF X11 =/= 0 GOTO A22
    Z30 <- Z30 + 1
    IF Z30 =/= 0 GOTO A25
(A24) Z13 <- Z13 - 1
(A25) IF Z13 =/= 0 GOTO A24
    IF Z12 =/= 0 GOTO A16
    Z31 <- Z31 + 1
    IF Z31 =/= 0 GOTO A17
(A16) Z12 <- Z12 - 1
    X11 <- X11 + 1
    Z13 <- Z13 + 1
    IF Z12 =/= 0 GOTO A16
(A17) X11 <- X11
))

(define my-s-list8
'(
    Z26 <- Z26 + 1
    IF Z26 =/= 0 GOTO A19
(A18) X10 <- X10 - 1
(A19) IF X10 =/= 0 GOTO A18
    Z27 <- Z27 + 1
    IF Z27 =/= 0 GOTO A21
(A20) Z12 <- Z12 - 1
(A21) IF Z12 =/= 0 GOTO A20
    IF X11 =/= 0 GOTO A14
    Z28 <- Z28 + 1
    IF Z28 =/= 0 GOTO A15
(A14) X11 <- X11 - 1
    X10 <- X10 + 1
    Z12 <- Z12 + 1
    IF X11 =/= 0 GOTO A14
(A15) X10 <- X10
    Z29 <- Z29 + 1
    IF Z29 =/= 0 GOTO A23
(A22) X11 <- X11 - 1
(A23) IF X11 =/= 0 GOTO A22
    Z30 <- Z30 + 1
    IF Z30 =/= 0 GOTO A25
(A24) Z13 <- Z13 - 1
(A25) IF Z13 =/= 0 GOTO A24
    IF Z12 =/= 0 GOTO A16
    Z31 <- Z31 + 1
    IF Z31 =/= 0 GOTO A17
(A16) Z12 <- Z12 - 1
    X11 <- X11 + 1
    Z13 <- Z13 + 1
    IF Z12 =/= 0 GOTO A16
(A17) X11 <- X11
))

(define my-s-list9
'(
    Z28 <- Z28 + 1
    IF Z28 =/= 0 GOTO A19
(A18) X10 <- X10 - 1
(A19) IF X10 =/= 0 GOTO A18
    Z29 <- Z29 + 1
    IF Z29 =/= 0 GOTO A21
(A20) Z12 <- Z12 - 1
(A21) IF Z12 =/= 0 GOTO A20
    IF X11 =/= 0 GOTO A14
    Z30 <- Z30 + 1
    IF Z30 =/= 0 GOTO A15
(A14) X11 <- X11 - 1
    X10 <- X10 + 1
    Z12 <- Z12 + 1
    IF X11 =/= 0 GOTO A14
(A15) X10 <- X10
    Z31 <- Z31 + 1
    IF Z31 =/= 0 GOTO A23
(A22) X11 <- X11 - 1
(A23) IF X11 =/= 0 GOTO A22
    Z32 <- Z32 + 1
    IF Z32 =/= 0 GOTO A25
(A24) Z13 <- Z13 - 1
(A25) IF Z13 =/= 0 GOTO A24
    IF Z12 =/= 0 GOTO A16
    Z33 <- Z33 + 1
    IF Z33 =/= 0 GOTO A17
(A16) Z12 <- Z12 - 1
    X11 <- X11 + 1
    Z13 <- Z13 + 1
    IF Z12 =/= 0 GOTO A16
(A17) X11 <- X11
    Z34 <- Z34 + 1
    IF Z34 =/= 0 GOTO L
    Z35 <- Z35 + 1
    IF Z35 =/= 0 GOTO A27
(A26) V <- V - 1
(A27) IF V =/= 0 GOTO A26
))

(define myprog4
  (a-program
   (list (labeled 'A12 (add1 'X)) (unlabeled (sub1 'Y4)))))

(define add2-body
  (list (unlabeled (add1 'Y))
        (unlabeled (add1 'Y))
        (unlabeled (if-goto 'Y 'B))
        (labeled 'A (add1 'Y))
        (unlabeled (sub1 'X))
        (labeled 'B (if-goto 'X 'A))))

(define addXandY-body
  (list (unlabeled (assign 'C2 'Y))
        (unlabeled (assign 'Y 'X))
        (labeled 'B3 (if-goto 'C2 'R))
        (unlabeled (goto-L 'E))
        (labeled 'R (sub1 'C2))
        (unlabeled (add1 'Y))
        (unlabeled (goto-L 'B3))
        (labeled 'E (zero-V 'X))
        (unlabeled (skip 'X))))

(define mystate1
  (extend-state 'X 3 (extend-state 'Y 0 (empty-state))))

(define mystate2
  (extend-state 'Y 3 (extend-state 'X 25 (empty-state))))

(define mysnp1
  (a-snapshot 1 mystate1))

(define mysnp2
  (a-snapshot 1 mystate2))

(define add2-prog
  (a-program add2-body))

(define addXandY-prog
  (a-program addXandY-body))

; check the result of every test case in the list
(define test-run
  (lambda (l i)
    (if (null? l) "Tests completed successfully!"
        (if (not (car l))
            (string-append "Test case "  (number->string i) " failed!")
            (test-run (cdr l) (+ i 1))))))

; all test cases in a list
(define tests
  (list
   (equal? (pretty-print myprog) myprog-string)
   (equal? (parse-s-list my-s-list) myprog)
   (equal? (parse (pretty-print (parse-s-list my-s-list))) myprog)
   (equal? (pretty-print myprog2) myprog2-string)
   (equal? (parse-s-list my-s-list2) myprog2)
   (equal? (parse (pretty-print (parse-s-list my-s-list2))) myprog2)
   (eq? (max-index myprog4) 12)
   (equal? (pretty-print (expand-GOTOS (parse-s-list my-s-list3))) myprog3-string)
   (equal? (pretty-print (expand-GOTOS (parse-s-list my-s-list4))) myprog4-string)
   (equal? (pretty-print (expandV<-0 (parse-s-list my-s-list5))) myprog5-string)
   (equal? (pretty-print (expand-assign (a-program (list (unlabeled (assign 'X10 'X11))))))
           myprog6-string-unlabeled)
   (equal? (pretty-print (expand-assign (a-program (list (labeled 'A (assign 'X10 'X11))))))
           myprog6-string-labeled)
   (equal? (parse-s-list my-s-list7) (clean-up (a-program (list (labeled 'A (assign 'X10 'X11))))))
   (equal? (parse-s-list my-s-list8) (clean-up (a-program (list (unlabeled (assign 'X10 'X11))))))
   (equal? (parse-s-list my-s-list9)
           (clean-up (a-program (list
                                 (unlabeled (assign 'X10 'X11))
                                 (unlabeled (goto-L 'L))
                                 (unlabeled (zero-V 'V))))))
   (equal? (parse (pretty-print (parse-s-list my-s-list9))) (parse-s-list my-s-list9))
   (equal? (eval-program add2-prog mysnp1) "Y = 5\n")
   (equal? (parse (pretty-print addXandY-prog)) addXandY-prog)
   (equal? (eval-program addXandY-prog mysnp2) "Y = 28\n")
   (equal? (eval-program (clean-up addXandY-prog) mysnp2) "Y = 28\n")
   ))

; show the test results
(display (test-run tests 1))

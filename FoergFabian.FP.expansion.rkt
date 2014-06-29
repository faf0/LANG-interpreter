#lang eopl

(require "FoergFabian.FP.rkt")

; MODULE EXPORTS

(provide expand-GOTOS)
(provide expandV<-0)
(provide expand-assign)
(provide clean-up)

; HELPER FUNCTIONS

; fresh-label : vector -> symbol
(define fresh-label
  (lambda (i)
    (begin
      (vector-set! i 0 (+ (vector-ref i 0) 1))
      (string->symbol (string-append "A" (number->string (vector-ref i 0)))))))

; fresh-var : vector -> symbol
(define fresh-var
  (lambda (i)
    (begin
      (vector-set! i 0 (+ (vector-ref i 0) 1))
      (string->symbol (string-append "Z" (number->string (vector-ref i 0)))))))

; EXPAND-GOTOS

; expand-GOTOS : program -> program
(define expand-GOTOS
  (lambda (p)
    (cases program p
      (a-program (instrs)
                 (let ((n (vector (max-index p))))
                   (a-program (expand-GOTOS-instrs instrs n))))
      )))

; expand-GOTOS-instrs : (list-of instruction?) x vector -> (list-of instruction?)
(define expand-GOTOS-instrs
  (lambda (i n)
    (if (null? i) '()
        (append (expand-GOTOS-i (car i) n) (expand-GOTOS-instrs (cdr i) n)))))

; expand-GOTOS-i : instruction x vector -> (list-of instruction?)
(define expand-GOTOS-i
  (lambda (i n)
    (if (null? i)
        '()
        (cases instruction i
          (labeled (L s)
                   (cases statement s
                     (goto-L (l)
                             (let ((fv (fresh-var n)))
                               (list (labeled L (add1 fv)) (unlabeled (if-goto fv l)))))
                     (else (list i))))
          (unlabeled (s)
                     (cases statement s
                       (goto-L (l)
                               (let ((fv (fresh-var n)))
                                 (list (unlabeled (add1 fv)) (unlabeled (if-goto fv l)))))
                       (else (list i))))))))

; EXPANDV<-0

; expandV<-0 : program -> program
(define expandV<-0
  (lambda (p)
    (cases program p  
      (a-program (instrs)
                 (let ((n (vector (max-index p))))
                   (a-program (expandV<-0-instrs instrs n)))))))

; expandV<-0-instrs : (list-of instruction?) x vector -> (list-of instruction?)
(define expandV<-0-instrs
  (lambda (i n)
    (if (null? i) '()
        (append (expandV<-0-i (car i) n) (expandV<-0-instrs (cdr i) n)))))

; expandV<-0-i : instruction x vector -> (list-of instruction?)
(define expandV<-0-i
  (lambda (i n)
    (if (null? i)
        '()
          (cases instruction i
            (labeled (L s)
                     (cases statement s
                       (zero-V (vp)
                               (expandV<-0-i-h L vp n))
                       (else (list i))))
            (unlabeled (s)
                       (cases statement s
                         (zero-V (vp)
                                 (expandV<-0-i-h NO_LABEL vp n))
                         (else (list i))))))))

; expandV<-0-i-h : symbol x symbol x vector -> (list-of instruction?)
(define expandV<-0-i-h
  (lambda (l v n)
    (let* ((fl1 (fresh-label n))
           (fl2 (fresh-label n))
           (prefix (if (eq? l NO_LABEL)
                       (unlabeled (goto-L fl2))
                       (labeled l (goto-L fl2)))))
      (list prefix (labeled fl1 (sub1 v)) (labeled fl2 (if-goto v fl1))))))

; EXPAND-ASSIGN

; expand-assign : program -> program
(define expand-assign
  (lambda (p)
    (cases program p  
      (a-program (instrs)
                 (let ((n (vector (max-index p))))
                   (a-program (expand-assign-instrs instrs n)))))))

; expand-assign-instrs : (list-of instruction?) x vector -> (list-of instruction?)
(define expand-assign-instrs
  (lambda (i n)
    (if (null? i) '()
        (append (expand-assign-i (car i) n) (expand-assign-instrs (cdr i) n)))))

(define NO_LABEL '0)

; expand-assign-i : instruction x vector -> (list-of instruction?)
(define expand-assign-i
  (lambda (i n)
    (if (null? i)
        '()
          (cases instruction i
            (labeled (L s)
                     (cases statement s
                       (assign (V1 V2)
                               (expand-assign-i-outer L V1 V2 n))
                       (else (list i))))
            (unlabeled (s)
                       (cases statement s
                         (assign (V1 V2)
                                 (expand-assign-i-outer NO_LABEL V1 V2 n))
                         (else (list i))))))))

; expand-assign-i-outer : symbol x symbol x symbol x vector -> (list-of instruction?)
(define expand-assign-i-outer
  (lambda (L V1 V2 n)
    (let ((fv (fresh-var n))
          (fv2 (fresh-var n)))
      (append (expand-assign-i-inner L V1 V2 n fv) (expand-assign-i-inner NO_LABEL V2 fv n fv2)))))

; expand-assign-i-inner : symbol x symbol x symbol x vector x symbol -> (list-of instruction?)
(define expand-assign-i-inner
  (lambda (L V1 V2 n fv)
    (if (equal? NO_LABEL L)
        (cons (unlabeled (zero-V V1)) (expand-assign-sec V1 V2 fv n))
        (cons (labeled L (zero-V V1)) (expand-assign-sec V1 V2 fv n))
        )))

; expand-assign-sec : symbol x symbol x symbol x symbol -> (list-of instruction?)
(define expand-assign-sec
  (lambda (V1 V2 fv n)
    (let ((fl1 (fresh-label n))
          (fl2 (fresh-label n)))
      (list (unlabeled (zero-V fv)) (unlabeled (if-goto V2 fl1)) (unlabeled (goto-L fl2))
            (labeled fl1 (sub1 V2)) (unlabeled (add1 V1)) (unlabeled (add1 fv))
            (unlabeled (if-goto V2 fl1)) (labeled fl2 (skip V1))))))

; CLEAN-UP

; clean-up : program -> program
(define clean-up
  (lambda (p)
    ; the order of the function calls is important!
    ; expand-assign produces V<-0 and GOTO instructions
    ; expandV<-0 produces GOTO instructions
    ; expand-GOTOS produces only V<-V+1 and IF-GOTO instructions
    (expand-GOTOS (expandV<-0 (expand-assign p)))))

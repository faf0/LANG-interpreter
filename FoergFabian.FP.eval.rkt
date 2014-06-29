#lang eopl

(require "FoergFabian.FP.rkt")

; MODULE EXPORTS

(provide state state? empty-state extend-state)
(provide pc?)
(provide snapshot snapshot? a-snapshot)
(provide eval-program)

; DATASTRUCTURES

(define-datatype state state?
  (empty-state)
  (extend-state (V symbol?) (m number?) (s state?)))

(define pc?
  (lambda (i) (and (number? i) (> i 0))))

(define-datatype snapshot snapshot?
  (a-snapshot (i pc?) (s state?)))

; EVALUATION

; eval-program : program x snapshot -> value
; may or may not halt
(define eval-program
  (lambda (p s)
    (cases program p
      (a-program (instrs)
                 (cases snapshot s
                   (a-snapshot (pc state)
                               (let ((value (eval-i instrs pc state)))
                                 (string-append "Y = " (number->string value) "\n"))))))))

; eval-i : (list-of instruction?) x pc x state -> value
(define eval-i
  (lambda (instrs pc state)
    (if (> pc (length instrs))
        (apply-state state 'Y)
        (let ((i (list-ref instrs (- pc 1))))
          (cases instruction i
            (labeled (L S)
                     (eval-i-h instrs (eval-s instrs pc state S)))
            (unlabeled (S)
                       (eval-i-h instrs (eval-s instrs pc state S))))))))

; eval-i-h : (list-of instruction?) x snapshot -> value
(define eval-i-h
  (lambda (instrs s)
    (cases snapshot s
      (a-snapshot (pc state)
                  (eval-i instrs pc state)))))

; eval-s : (list-of instruction?) x pc x state x statement -> snapshot
(define eval-s
  (lambda (instrs pc state stmnt)
    (cases statement stmnt
      (add1 (V)
            (let* ((new-state (extend-state V (+ (apply-state state V) 1) state))
                   (snapshot (a-snapshot (+ pc 1) new-state)))
              snapshot))
      (sub1 (V)
            (let* ((new-state (extend-state V (- (apply-state state V) 1) state))
                   (snapshot (a-snapshot (+ pc 1) new-state)))
              snapshot))
      (skip (V)
            (a-snapshot (+ pc 1) state))
      (if-goto (V L)
               (let ((val (apply-state state V)))
                 (if (zero? val)
                     (a-snapshot (+ pc 1) state)
                     (let ((res (find-instr instrs L)))
                       (if (eq? NO_LABEL res)
                           (eopl:error "unexisting if-goto label")
                           (a-snapshot res state))))))
      (goto-L (L)
              (let ((res (find-instr instrs L)))
                (if (eq? NO_LABEL res)
                    (eopl:error "unexisting goto-L label")
                    (a-snapshot res state))))
      (zero-V (V)
              (let* ((new-state (extend-state V 0 state))
                     (snapshot (a-snapshot (+ pc 1) new-state)))
                snapshot))
      (assign (V1 V2)
              (let* ((new-state (extend-state V1 (apply-state state V2) state))
                     (snapshot (a-snapshot (+ pc 1) new-state)))
                snapshot))
      )))

(define INIT_VALUE 0)

; apply-state : state x symbol -> value
(define apply-state
  (lambda (st sy)
    (cases state st
      (empty-state ()
                   INIT_VALUE)
      (extend-state (sy2 val next-state)
                    (if (eq? sy sy2)
                        val
                        (apply-state next-state sy))))))

(define NO_LABEL -1)

; find-instr : (list-of instruction?) x symbol -> number
(define find-instr
  (lambda (instrs label)
    (find-instr-n instrs 1 label)))

; find-instr-n : (list-of instruction?) x pc x symbol -> number
(define find-instr-n
  (lambda (instrs pc label)
    (if (null? instrs)
        NO_LABEL
        (let ((next (find-instr-n (cdr instrs) (+ pc 1) label)))
          (cases instruction (car instrs)
            (labeled (L I)
                     (if (eq? L label)
                         pc
                         next))
            (unlabeled (I)
                       next))))))

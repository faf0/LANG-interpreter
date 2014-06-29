#lang eopl

; MODULE EXPORTS

(provide statement statement? add1 sub1 skip if-goto goto-L zero-V assign)
(provide instruction instruction? labeled unlabeled)
(provide program program? a-program)
(provide pretty-print)
(provide parse parse-s-list)
(provide max-index)

; DATASTRUCTURES

(define-datatype statement statement?
  (add1 (V symbol?))
  (sub1 (V symbol?))
  (skip (V symbol?))
  (if-goto (V symbol?)
           (l symbol?))
  (goto-L (l symbol?))
  (zero-V (V symbol?))
  (assign (V1 symbol?)
          (V2 symbol?)))

(define-datatype instruction instruction?
  (labeled (l symbol?)
           (i statement?))
  (unlabeled (i statement?)))

(define-datatype program program?
  (a-program (l (list-of instruction?))))

; PRETTY-PRINT

; pretty-print : program -> string
(define pretty-print
  (lambda (p)
    (cases program p
      (a-program (l)
                 (pretty-print-l l))
      (else (eopl:error "p is not a program")))))

; pretty-print-l : (list-of instruction?) -> string
(define pretty-print-l
  (lambda (l)
    (if (null? l) ""
        (if (pair? l)
            (string-append (pretty-print-i (car l)) "\n" (pretty-print-l (cdr l)))
            (eopl:error "l is not a list of instructions")))
    ))

; pretty-print-i : instruction? -> string
(define pretty-print-i
  (lambda (i)
    (cases instruction i
      (labeled (l i) (string-append "(" (symbol->string l) ") " (pretty-print-s i)))
      (unlabeled (i) (string-append "\t" (pretty-print-s i))))))

; pretty-print-s : statement? -> string
(define pretty-print-s
  (lambda (s)
    (cases statement s
      (add1 (V)
            (string-append (symbol->string V) " <- " (symbol->string V) " + 1"))
      (sub1 (V)
            (string-append (symbol->string V) " <- " (symbol->string V) " - 1"))
      (skip (V)
            (string-append (symbol->string V) " <- " (symbol->string V)))
      (if-goto (V L)
               (string-append "IF " (symbol->string V) " =/= 0 GOTO " (symbol->string L)))
      (goto-L (L)
              (string-append "GOTO " (symbol->string L)))
      (zero-V (V)
              (string-append (symbol->string V) " <- 0"))
      (assign (V1 V2)
              (string-append (symbol->string V1) " <- " (symbol->string V2))))))

; PARSE (S-list -> program)

; parse : S-list -> program
(define parse-s-list
  (lambda (s)
    (if (null? s)
        (eopl:error "s must not be null")
        (a-program (parse-s-list-i s)))))

; parse-s-list-i : S-list -> (list-of instruction?)
(define parse-s-list-i
  (lambda (s)
    (if (null? s) '()
        (if (labeled? s)
            (cons (labeled (parse-s-list-label s) (parse-s-list-s (cdr s))) (parse-s-list-i (next-instruction s)))
            (cons (unlabeled (parse-s-list-s s)) (parse-s-list-i (next-instruction s))))
        )))

; parse-s-list-s : S-list -> statement
(define parse-s-list-s
  (lambda (s)
    (cond
      [(add1? s) (add1 (car s))]
      [(sub1? s) (sub1 (car s))]
      [(skip? s) (skip (car s))]
      [(if-goto? s) (if-goto (cadr s) (caddr (cdddr s)))]
      [(goto-L? s) (goto-L (cadr s))]
      [(zero-V? s) (zero-V (car s))]
      [(assign? s) (assign (car s) (caddr s))]
      [else (eopl:error "unknown instruction")]
      )))

; next-instruction : S-list -> S-list
(define next-instruction
  (lambda (s)
    (if (null? s) '()
        (if (labeled? s)
            (next-instruction (cdr s))
            (cond
              [(add1? s) (cddr (cdddr s))]
              [(sub1? s) (cddr (cdddr s))]
              [(skip? s) (cdddr s)]
              [(if-goto? s) (cdddr (cdddr s))]
              [(goto-L? s) (cddr s)]
              [(zero-V? s) (cdddr s)]
              [(assign? s) (cdddr s)]
              [else (eopl:error "unknown instruction")]
              )))))

; labeled? : symbol -> bool
(define labeled?
  (lambda (s)
    (and (not (null? s)) (list? (car s)))))

; add1? : S-list -> bool
(define add1?
  (lambda (s)
    (and (>= (length s) 5) (eq? (car s) (caddr s)) (eq? (cadr s) '<-) (eq? (cadddr s) '+) (eq? (caddr (cddr s)) '1))))

; sub1? : S-list -> bool
(define sub1?
  (lambda (s)
    (and (>= (length s) 5) (eq? (car s) (caddr s)) (eq? (cadr s) '<-) (eq? (cadddr s) '-) (eq? (caddr (cddr s)) '1))))

; skip? : S-list -> bool
(define skip?
  (lambda (s)
    (and (>= (length s) 3) (eq? (car s) (caddr s)) (eq? (cadr s) '<-) (not (add1? s)) (not (sub1? s)))))

; if-goto? : S-list -> bool
(define if-goto?
  (lambda (s)
    (and (>= (length s) 6) (eq? (car s) 'IF) (eq? (caddr s) '=/=) (zero? (cadddr s)) (eq? (caddr (cddr s)) 'GOTO))))

; goto-L? : S-list -> bool
(define goto-L?
  (lambda (s)
    (and (>= (length s) 2) (eq? (car s) 'GOTO))))

; zero-V? : S-list -> bool
(define zero-V?
  (lambda (s)
    (and (>= (length s) 3) (eq? (cadr s) '<-) (eq? (caddr s) 0))))

; assign? : S-list -> bool
(define assign?
  (lambda (s)
    (and (>= (length s) 3) (eq? (cadr s) '<-) (not (add1? s)) (not (sub1? s)) (not (skip? s)) (not (zero-V? s)))))

; parse-label : S-list -> symbol
(define parse-s-list-label
  (lambda (s)
    (caar s)))

; PARSE (string -> program)

(define parse
  (lambda (s)
    (a-program (parse-i s))))

; parse-i : string -> (list-of instruction?)
(define parse-i
  (lambda (s)
    (if (equal? "" s) '()
        (let ((instr (next-line s))
              (next-instrs (substring s (next-line-start s)))) 
        (if (labeled-s? s)
            (cons (labeled (parse-label instr) (parse-s instr)) (parse-i next-instrs))
            (cons (unlabeled (parse-s instr)) (parse-i  next-instrs)))))))

; parse-s : string -> statement
(define parse-s
  (lambda (s)
    (cond
      [(add1-s? s) (add1 (extract-first-sym s))]
      [(sub1-s? s) (sub1 (extract-first-sym s))]
      [(skip-s? s) (skip (extract-first-sym s))]
      [(if-goto-s? s) (if-goto (extract-if-var s) (extract-goto-label s))]
      [(goto-L-s? s) (goto-L (extract-goto-label s))]
      [(zero-V-s? s) (zero-V (extract-first-sym s))]
      [(assign-s? s) (assign (extract-first-sym s) (extract-second-sym s))]
      [else (eopl:error "unknown instruction")]
      )))

; labeled-s? : string -> bool
(define labeled-s?
  (lambda (s)
    (zero? (next-pos s "("))))

; add1-s? : string -> bool
(define add1-s?
  (lambda (s)
    (occurs s " + ")))

; sub1-s? : string -> bool
(define sub1-s?
  (lambda (s)
    (occurs s " - ")))

; skip-s? : string -> bool
(define skip-s?
  (lambda (s)
    (and (occurs s " <- ") (equal? (extract-first-sym s) (extract-second-sym s)) (not (add1-s? s)) (not (sub1-s? s)) (not (zero-V-s? s)))))

; if-goto-s? : string -> bool
(define if-goto-s?
  (lambda (s)
    (eq? (next-pos (trim-start s) "IF ") 0)))

; goto-L-s? : string -> bool
(define goto-L-s?
  (lambda (s)
    (zero? (next-pos (trim-start s) "GOTO "))))

; zero-V-s? : string -> bool
(define zero-V-s?
  (lambda (s)
    (occurs s " <- 0")))

; assign-s? : string -> bool
(define assign-s?
  (lambda (s)
    (and (occurs s " <- ") (not (add1-s? s)) (not (sub1-s? s)) (not (skip-s? s)) (not (zero-V-s? s)))))

; parse-label : string -> symbol
(define parse-label
  (lambda (s)
    (string->symbol (substring s (+ 1 (next-pos s "(")) (next-pos s ")")))))

; trim-start : string -> string
(define trim-start
  (lambda (s)
    (if (labeled-s? s)
        (substring s (+ 2 (next-pos s ") ")))
        (substring s 1))))

; extract-first-sym: string -> symbol
(define extract-first-sym
  (lambda (s)
    (let* ((trimmed (trim-start s))
           (end (next-pos trimmed " <- ")))
      (string->symbol (substring trimmed 0 end)))))

; extract-second-sym: string -> symbol
(define extract-second-sym
  (lambda (s)
    (let ((start (+ 4 (next-pos s " <- "))))
      (string->symbol (substring s start)))))

; extract-if-var: string -> symbol
(define extract-if-var
  (lambda (s)
    (let ((start (+ 3 (next-pos s "IF ")))
          (end (next-pos s " =/= 0 GOTO ")))
      (string->symbol (substring s start end)))))

; extract-goto-label: string -> symbol
(define extract-goto-label
  (lambda (s)
    (let ((start (+ 5 (next-pos s "GOTO "))))
      (string->symbol (substring s start)))))

; STRING HELPER FUNCTIONS

; occurs : string x string -> bool
(define occurs
  (lambda (input search)
    (not (eq? (next-pos input search) (string-length input)))))

; next-string : string x string -> number
(define next-string
  (lambda (input search)
    (substring input (next-pos input search))))

; next-pos : string x string -> number
(define next-pos
  (lambda (input search)
    (next-pos-h input search 0)))

; next-pos-h : string x string -> number
(define next-pos-h
  (lambda (input search pos)
    (let ((input-length (string-length input))
          (search-length (string-length search)))
      (if (> (+ pos search-length) input-length)
          input-length
          (if (equal? (substring input pos (+ pos search-length)) search)
              pos
              (next-pos-h input search (+ pos 1)))))))

; next-line: string -> string
(define next-line
  (lambda (input)
    (let ((end (next-pos input "\n")))
      (substring input 0 end))))

; next-line-start : string -> number
(define next-line-start
  (lambda (input)
    (min (string-length input) (+ 1 (next-pos input "\n")))))

; MAX-INDEX

; max-index : program -> number
(define max-index
  (lambda (prog)
    (cases program prog
      (a-program (instrs) (max-index-i instrs 0)))))

; max-index-i : (list-of instruction?) -> number
(define max-index-i
  (lambda (i m)
    (if (null? i) m
        (max m (max-index-instr (car i)) (max-index-i (cdr i) m)))))

; max-index-instr : instruction? -> number
(define max-index-instr
  (lambda (i)
    (cases instruction i
      (labeled (L I) (max (extract-index L) (max-index-s I)))
      (unlabeled (I) (max-index-s I)))))

; max-index-s : statement? -> number
(define max-index-s
  (lambda (s)
    (cases statement s
      (add1 (V)
            (extract-index V))
      (sub1 (V)
            (extract-index V))
      (skip (V)
            (extract-index V))
      (if-goto (V L)
               (max (extract-index V) (extract-index L)))
      (goto-L (L)
              (extract-index L))
      (zero-V (V)
              (extract-index V))
      (assign (V1 V2)
              (max (extract-index V1) (extract-index V2))))))

; extract-index : symbol -> number
(define extract-index
  (lambda (sym)
    (let* ((str (symbol->string sym))
           (str-len (string-length str)))
      (if (> str-len 1)
          (string->number (substring str 1 str-len))
          1))))

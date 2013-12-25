;;---------------------------------
;; Syntax Definitions
;;---------------------------------
(define-syntax for
  (syntax-rules (from to do with step in)
    ((for <var> from <start> to <end> do <body> ...)
     (let loop((<var> <start>)) (if (<= <var> <end>) (begin <body> ... (loop (+ <var> 1))))))
     
    ((for <var> from <start> to <end> with step <step> do <body> ...)
     (let loop((<var> <start>)) (if (<= <var> <end>) (begin <body> ... (loop (+ <var> <step>))))))
     
    ((for <var> in <list> do <body> ...)
     (let loop((<var> (car <list>)) (l (cdr <list>))) (begin <body> ... (if (not (equal? l '())) (loop (car l) (cdr l))))))
  )
)

(define-syntax while
  (syntax-rules ()
    ((while <statement> <body> ...) (let loop() (if <statement> (begin <body> ... (loop)))))
  )
)

;;---------------------------------
;; Debug and Trace
;;---------------------------------
(define-syntax DEBUG
  (syntax-rules ()
    ((DEBUG) #f)
  )
)

(define (print-delim delim . args)
  (if (equal? args '())
    (newline)
    (begin
      (display (car args))
      (if (> (length args) 1) (display delim))
      (apply print-delim (cons delim (cdr args)))
    )
  )
)

(define (print . args)
  (apply print-delim (cons ", " args))
)

(define-syntax assert
  (syntax-rules ()
    ((assert statement more ...)
      (if (not statement)
        (begin 
          (print-delim ": " "Assertion Failure" 'statement)
          (raise "OH DEAR!")
        )
        (assert more ...)
      )
    )
    ((assert) (display ""))
  )
)

(define-syntax trc
  (if (DEBUG)
    (syntax-rules ()
      ((trace stuff ...) (print stuff ...))
    )
    (syntax-rules ()
      ((trace stuff ...) (display ""))
    )
  )
)
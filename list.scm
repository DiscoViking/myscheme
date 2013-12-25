;;---------------------------------
;; General List Functions
;;---------------------------------
(define (filter-rec f l t)
  (if (equal? l '())
    t
    (filter-rec f (cdr l) (if (f (car l)) (cons (car l) t) t))
  )
)

(define (filter f l)
  (reverse (filter-rec f l '()))
)

(define (fold-left p x l)
  (if (equal? l '())
    x
    (fold-left p (p x (car l)) (cdr l))
  )
)

(define make-list 
    (lambda (n)         
        (if (= n 0) 
            '()
            (cons n (make-list (- n 1)))
        )  
    )
)

(define cat-rec
    (lambda (l m)
        (if (equal? l '())
            m
            (cat-rec (cdr l) (cons (car l) m))
        )
    )
)

(define (cat l m)
    (cat-rec (reverse l) m)
)

(define (in? x l)
  (cond 
    ((equal? l '()) #f)
    ((eqv? (car l) x) #t)
    (else (in? x (cdr l)))
  )
)

(define (index x l)
  (letrec ((f (lambda (x l i)
    (cond
      ((equal? l '()) #f)
      ((eqv? x (car l)) i)
      (else (f x (cdr l) (+ i 1)))
    ))))
    (f x l 0)
  )
)

(define (count-rec x l n)
    (if (equal? l '())
        n
        (if (= x (car l))
            (count-rec x (cdr l) (+ n 1))
            (count-rec x (cdr l) n)
        )
    )
)

(define (count x l)
    (count-rec x l 0)
)

(define (union-rec l m t)
  (if (equal? t '())
    m
    (let ((x (car t)))
      (if (> (count x l) (count x m))
        (union-rec l (cons x m) (cdr t))
        (union-rec l m (cdr t))
      )
    )
  )
)

(define (union l m)
  (union-rec l m l)
)

(define (cons-if-unique x l)
  (if (in? x l)
    l
    (cons x l)
  )
)

(define (remove-duplicates-rec l t)
  (if (equal? l '())
    (reverse t)
    (remove-duplicates-rec (cdr l) (cons-if-unique (car l) t))
  )
)

(define (remove-duplicates l)
  (remove-duplicates-rec l '())
)

(define (remove x l)
  (filter (lambda (y) (not (eqv? x y))) l)
)
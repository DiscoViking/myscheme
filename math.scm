;;---------------------------------
;; Constants
;;---------------------------------
(define phi (/ (+ 1 (sqrt 5)) 2))
(define pi 3.14159265359)

;;---------------------------------
;; General Math Functions
;;---------------------------------
(define (div x y)
    (floor (/ x y))
)

(define (mod x)
  (if (< x 0) (- 0 x) x)
)

(define (divisible-by? x n)
    (= 0 (modulo x n))
)

(define pow-rec
    (lambda (x n i)
        (if (= n 0)
            x
            (pow-rec (* x i) (- n 1) i)
        )
    )
)

(define (pow x n)
    (pow-rec 1 n x)
)

(define (factorial n)
  (letrec ((fact-rec (lambda (x t) 
          (cond 
          ((= x 0) 1)
          ((= x 1) t)
          (else (fact-rec (- x 1) (* t x)))))))
    (fact-rec n 1)
  )
)

(define (factors2 n)
  (filter (lambda (x) (divisible-by? n x)) (make-list n))
)

(define (factors n)
  (if (= n 0)
    '()
    (all-products (prime-factors n))
  )
)

(define (proper-factors n)
  (let ((l (factors n)))
    (if (equal? l '())
      l
      (cdr (quick-sort (factors n) >))
    )
  )
)

(define (all-products-rec l t)
  (cond
    ((equal? l '()) (remove-duplicates t))
    ((equal? t '()) (all-products-rec (cdr l) (cons (car l) t)))
    (else (all-products-rec (cdr l) 
                        (union (map (lambda (x) (* x (car l))) t) t)))
  )
)

(define (all-products l)
  (all-products-rec l '(1))
)

(define get-digit
    (lambda (n i)
        (modulo (div n (pow 10 i)) 10)
    )
)

(define num-digits-rec
    (lambda (n i)
        (if (< n (pow 10 i))
            i
            (num-digits-rec n (+ i 1))
        )
    )
)

(define (num-digits n)
    (num-digits-rec n 1)
)

(define (sum-digits-rec n t)
  (if (= n 0)
    t
    (sum-digits-rec (div n 10) (+ t (modulo n 10)))
  )
)

(define (sum-digits n)
  (sum-digits-rec n 0)
)

;;---------------------------------
;; Finding Primes
;;---------------------------------
(define sieve
    (lambda (l n)
        (filter (lambda (x) 
            (or (not (= 0 (modulo x n))) (= x n)))
            l
        )
    )
)

(define (primes-rec l u M)
    (let ((s (sqrt M)))
        (if (>= (car l) s)
            (cat (reverse u) l)
            (primes-rec 
                (cdr (sieve l (car l)))
                (cons (car l) u)
                M
            )
        )
    )
)

(define (primes n)
    (primes-rec (cdr (reverse (make-list n))) '() n)
)

(define prime-factors-rec
    (lambda (n l f)
        (if (= n 1)
            l
            (if (= 0 (modulo n f))
                (prime-factors-rec (/ n f) (cons f l) 2)
                (prime-factors-rec n l (+ f 1))
            )
        )
    )
)

(define (prime-factors n)
    (prime-factors-rec n '() 2)
)

(define (prime? n)
  (let ((s (sqrt n)))
    (letrec ((f (lambda (i)
      (cond
        ((> i s) #t)
        ((divisible-by? n i) #f)
        (else (f (+ i 1)))
      ))))
      (if (even? n) #f (f 3))
    )
  )
)

(define (nth-prime n)
  (letrec ((f (lambda (i x)
    (if (= i n)
      (- x 2)
      (f (if (prime? x) (+ i 1) i) (+ x 2))
    ))))
    (if (= n 1) 2 (f 1 3))
  )
)

;;---------------------------------
;; Sequences
;;---------------------------------
(define fib-rec 
    (lambda (l n m Z)
        (if (< m Z)
            (fib-rec (cons m l) m (+ n m) Z)
            l
        )
    )
)

(define (fib-below n)
    (fib-rec '() 0 1 n)
)

(define (fib n)
  (floor (+ 0.5 (/ (pow phi n) (sqrt 5))))
)

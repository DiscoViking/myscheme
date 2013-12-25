;;---------------------------------
;; General Math Functions
;;---------------------------------
(define div
    (lambda (x y)
        (if (> (round (/ x y)) (/ x y))
            (- (round (/ x y)) 1)
            (round (/ x y))
        )
    )
)

(define modulo
    (lambda (x y)
        (- x (* y (div x y)))
    )
)

(define (divisible-by? x n)
    (eqv? 0 (modulo x n))
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

(define factors
    (lambda (n)
        (filter 
            (lambda (x) (= (modulo n x) 0))
            (make-list n)
        )
    )
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

(define primes-rec
    (lambda (l n N)
        (if (>= n (/ N 2))
            l
            (if (in? n l)
                (primes-rec 
                    (sieve l n)
                    (+ n 1)
                    N
                )
                (primes-rec l (+ n 1) N)
            )
        )
    )
)

(define (primes n)
    (filter (lambda (x) (not (= x 1)))
        (primes-rec (make-list n) 2 n)
    )
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
    (equal? (prime-factors n) (list n))
)

;;---------------------------------
;; Sequences
;;---------------------------------
(define fib-rec 
    (lambda (l n m N)
        (if (< m N)
            (fib-rec (cons m l) m (+ n m) N)
            l
        )
    )
)

(define (fib n)
    (fib-rec '() 0 1 n)
)

;;---------------------------------
;; General List Functions
;;---------------------------------
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

(define (reverse l)
    (cat-rec l '())
)

(define (cat l m)
    (cat-rec (reverse l) m)
)

(define in?
    (lambda (n l)
        (if (= (length (filter (lambda (x) (= x n))
                            l
                    )
            ) 0)
            #f
            #t
        )
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

(define (merge-rec l m t)
    (if (equal? t '())
        m
        (let ((x (car t)))
            (if (> (count x l) (count x m))
                (merge-rec l (cons x m) (cdr t))
                (merge-rec l m (cdr t))
            )
        )
    )
)

(define (merge l m)
    (merge-rec l m l)
)

;;---------------------------------
;; Sorts
;;---------------------------------
(define (quick-sort l)
    (if (equal? l '())
        l
        (let ((p (car l)))
            (cat
                (quick-sort (filter (lambda (x) (< x p)) l))
                (cons p (quick-sort 
                    (filter (lambda (x) (>= x p)) (cdr l))))
            )
        )
    )
)

;;---------------------------------
;; Euler Problem #4
;;---------------------------------
(define palindrome-rec?
    (lambda (n low high)
        (if (< high low)
            #t
            (if 
            (not (eqv? (get-digit n low) (get-digit n high)))
                #f
                (palindrome-rec? n (+ low 1) (- high 1))
            )
        )
    )
)

(define (palindrome? n)
    (palindrome-rec? n 0 (- (num-digits n) 1))
)

(define (3dmuls l n m N)
    (if (< m N)
        l
        (if (<= n m)
            (3dmuls (cons (* n m) l) 999 (- m 1) N)
            (3dmuls (cons (* n m) l) (- n 1) m N)
        )
    )
)

;;---------------------------------
;; Euler Problem #5
;;---------------------------------
(define (common-prime-factors l)
    (fold-left (lambda (l n) (merge l (prime-factors n))) '() l)
)

;;---------------------------------
;; Euler Problem #6
;;---------------------------------
(define (sum-of-squares n)
    (fold-left (lambda (t x) (+ t (pow x 2))) 0 (make-list n))
)

(define (square-of-sum n)
    (pow (/ (* n (+ n 1)) 2) 2)
)

;;---------------------------------
;; Euler Problem #7
;;---------------------------------
(define (divisible-by-any? x P)
    (if (equal? P '())
        #f
        (if (divisible-by? x (car P))
            #t
            (divisible-by-any? x (cdr P))
        )
    )
)
(define (nth-prime-rec n x P)
    (if (= 0 n)
        (- x 1)
        (if (divisible-by-any? x P)
            (nth-prime-rec n (+ x 1) P)
            (nth-prime-rec (- n 1) (+ x 1) (cons x P))
        )
    )
)

(define (nth-prime n)
    (nth-prime-rec n 2 '())
)

;;---------------------------------
;; Euler Problem #8
;;---------------------------------
(define (chars->ints-rec s i)
    (if (= 0 (length s))
        i
        (chars->ints-rec 
            (cdr s) 
            (cons (- (char->integer (car s)) 48) i))
    )
)

(define (chars->ints s)
    (reverse (chars->ints-rec s '()))
)

(define (append x l)
    (reverse (cons x (reverse l)))
)

(define (max5sum-rec ints my5 max)
    (if (= (length ints) 0)
        max
        (let ((c (fold-left * 1 my5)))
        (max5sum-rec 
            (cdr ints) 
            (append (car ints) (cdr my5))
            (if (> c max) c max))
        )
    )
)

(define (max5sum ints)
    (max5sum-rec ints '(0 0 0 0 0) 0)
)

;;---------------------------------
;; Euler Problem #9
;;---------------------------------
(define (euler9-rec a b)
    (let ((c (sqrt (+ (* a a) (* b b)))))
        (if (and (integer? c) (= (+ a b c) 1000))
            (* a b c)
            (if (> b 999)
                (euler9-rec (+ a 1) (+ a 1))
                (euler9-rec a (+ b 1))
            )
        )
    )
)

;;---------------------------------
;; Euler Problem #14
;;---------------------------------
(define (collatz-step x)
  (if (even? x)
    (/ x 2)
    (+ (* x 3) 1)
  )
)

(define (collatz-len-rec x n)
  (if (eqv? x 1)
    n
    (collatz-len-rec (collatz-step x) (+ n 1))
  )
)

(define (collatz-len x)
  (collatz-len-rec x 0)
)

(define (longest-collatz-rec N x b bl)
  (if (>= x N)
    b
    (let ((l (collatz-len x)))
      (if (> l bl)
        (longest-collatz-rec N (+ x 1) x  l)
        (longest-collatz-rec N (+ x 1) b bl)
      )
    )
  )
)

(define (longest-collatz N)
  (longest-collatz-rec N 1 0 0)
)

(load "list")
(load "math")
(load "sort")
(load "type")

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

(define (3dmuls l n m Z)
    (if (< m Z)
        l
        (if (<= n m)
            (3dmuls (cons (* n m) l) 999 (- m 1) Z)
            (3dmuls (cons (* n m) l) (- n 1) m Z)
        )
    )
)

;;---------------------------------
;; Euler Problem #5
;;---------------------------------
(define (common-prime-factors l)
    (fold-left (lambda (l n) (union l (prime-factors n))) '() l)
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
;; Euler Problem #12
;;---------------------------------
(define (euler12-rec x dx N)
  (let ((f (length (factors x))))
    (if (>= f N)
      x
      (euler12-rec (+ x dx) (+ dx 1) N)
    )
  )
)

(define (euler12 N)
  (euler12-rec 1 2 N)
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

(define (collatz-len-rec x n l)
  (if (and (not (equal? l '())) (odd? x) (< x (* 2 (length l))))
    (+ (list-ref l (- (length l) (/ (+ x 1) 2))) n)
    (if (eqv? x 1)
      n
      (collatz-len-rec (collatz-step x) (+ n 1) l)
    )
  )
)

(define (collatz-len x)
  (collatz-len-rec x 0 '())
)

(define (longest-collatz-rec N x b bl L)
  (if (>= x N)
    b
    (let ((l (collatz-len x)))
      (if (> l bl)
        (longest-collatz-rec N (+ x 1) x  l (if (odd? x) (cons l L) L))
        (longest-collatz-rec N (+ x 1) b bl (if (odd? x) (cons l L) L))
      )
    )
  )
)

(define (longest-collatz N)
  (longest-collatz-rec N 1 0 0 '())
)

;;---------------------------------
;; Euler Problem #17
;;---------------------------------
(define (num-letters n)
  (cond
    ((= n 0) 0)
    ((= n 1) 3) ;one
    ((= n 2) 3) ;two
    ((= n 3) 5) ;three
    ((= n 4) 4) ;four
    ((= n 5) 4) ;five
    ((= n 6) 3) ;six
    ((= n 7) 5) ;seven
    ((= n 8) 5) ;eight
    ((= n 9) 4) ;nine
    ((= n 10) 3);ten
    ((= n 11) 6);eleven
    ((= n 12) 6);twelve
    ((= n 13) 8);thirteen
    ((= n 14) 8);fourteen
    ((= n 15) 7);fifteen
    ((= n 16) 7);sixteen
    ((= n 17) 9);seventeen
    ((= n 18) 8);eighteen
    ((= n 19) 8);nineteen
    ((= n 20) 6);twenty
    ((= n 30) 6);thirty
    ((= n 40) 5);forty
    ((= n 50) 5);fifty
    ((= n 60) 5);sixty
    ((= n 70) 7);seventy
    ((= n 80) 6);eighty
    ((= n 90) 6);ninety
    ((>= n 1000) (+ 8 
                    (num-letters (div n 1000)) 
                    (num-letters (modulo n (* 1000 (div n 1000))))))
    ((>= n 100) (+ 7
                  (num-letters (div n 100))
                  (num-letters (modulo n (* 100 (div n 100))))
                  (if (= (modulo n 100) 0) 0 3)))
    ((>= n 20) (+ (num-letters (* 10 (div n 10)))
                  (num-letters (modulo n (* 10 (div n 10))))))
  )
)

;;---------------------------------
;; Euler Problem #21
;;---------------------------------
(define (find-amicable-rec Z n am ig)
  (if (> n Z)
    am
    (if (in? n ig)
     (find-amicable-rec Z (+ n 1) am ig)
      (let ((s (fold-left + 0 (proper-factors n))))
        (if (and (not (= s n)) (= (fold-left + 0 (proper-factors s)) n))
          (find-amicable-rec Z (+ n 1) (cons n (cons s am)) (cons s ig))
          (find-amicable-rec Z (+ n 1) am ig)
        )
      )
    )
  )
)

(define (find-amicable N)
  (filter (lambda (x) (<= x N)) (find-amicable-rec N 1 '() '()))
)

(define (sum-amicable N)
  (fold-left + 0 (remove-duplicates (find-amicable N)))
)

;;---------------------------------
;; Euler Problem #23
;;---------------------------------
(define (abundant? x)
  (> (fold-left + 0 (proper-factors x)) x)
)

(define (find-abundant N)
  (letrec ((f (lambda (M n ab)
    (cond
      ((> n M) ab)
      ((in? (/ n 2) ab) (f M (+ n 1) (cons n ab)))
      ((abundant? n) (f M (+ n 1) (cons n ab)))
      (else (f M (+ n 1) ab))
    ))))
    (f M 1 '())
  )
)

(define (is-sum-of-2? x l)
  (cond
    ((equal? l '()) #f)
    ((in? (- x (car l)) l) #t)
    (else (is-sum-of-2 x (cdr l)))
  )
)

(define (not-sum-of-2-abundant N)
  (let ((l (find-abundant N)))
    (filter (lambda (x) (not (is-sum-of-2? x l))) (make-list N))
  )
)

(define (euler23 N)
  (fold-left + 0 (not-sum-of-2-abundant N))
)

;;---------------------------------
;; Euler Problem 24
;;---------------------------------
(define (first-digit-of-nth-perm n l)
  (let ((f (factorial (- (length l) 1))))
    (let loop((i 0) (t f))
      (if (> t n)
        (list-ref l i)
        (loop (+ i 1) (+ t f))
      )
    )
  )
)

(define (perm n l)
  (letrec ((perm-rec (lambda (n l p)
    (if (equal? l '())
      (reverse p)
      (let ((c (first-digit-of-nth-perm n l)))
       (perm-rec (modulo n (factorial (- (length l) 1))) 
                 (remove c l) 
                 (cons c p))
      )
    ))))
    (perm-rec n (merge-sort l <) '())
  )
)

;;---------------------------------
;; Euler Problem 25
;;---------------------------------
(define big-fib
  (let ((big-phi (number->bignum phi)) (big-sqrt5 (number->bignum (sqrt 5))) (big-0.5 (number->bignum 0.5)))
    (lambda (n)
      (bignum+ big-0.5 (bignum/ (bignum-pow big-phi n) big-sqrt5))
    )
  )
)

(define (first-n-digit-fib n)
  (letrec ((f (lambda (n low high)
    (let* ((mid (floor (+ low (/ (- high low) 2)))) (d (bignum-e (big-fib mid))))
      (cond
        ((<= (- high low) 1) high)
        ((>= d n) (f n low mid))
        (else (f n mid high))
      )
    )))
  (g (lambda (n i)
    (if (>= (bignum-e (big-fib i)) n)
      i
      (g n (* i 2))
    ))))
    (f (- n 1) 1 (g (- n 1) 1))
  )
)
   
;;---------------------------------
;; Euler Problem 26
;;---------------------------------   
(define (unit-frac-cycle-len d)
  (letrec ((f (lambda (d rems r)
    (cond 
      ((in? r rems) (+ 1 (index r rems)))
      ((= r 0) 0)
      (else (f d (cons r rems) (modulo (* r 10) d)))
    ))))
    (f d '() 1)
  )
)

(define (largest func start end)
  (letrec ((f (lambda (i l lv)
    (let ((v (func i)))
      (cond
        ((> i end) l)
        ((> v lv) (f (+ i 1) i v))
        (else (f (+ i 1) l lv))
      )
    ))))
    (f start (func start) start)
  )
)

;;---------------------------------
;; Euler Problem 27
;;---------------------------------
(define (quadratic a b x)
  (+ (* x x) (* a x) b)
)

(define (consecutive-primes a b)
  (letrec ((consp (lambda(a b n)
    (if (prime? (quadratic a b n))
      (consp a b (+ n 1))
      n
    ))))
    (consp a b 0)
  )
)

(define (euler27)
  (letrec ((solve (lambda (a b besta bestb most)
    (let ((n (consecutive-primes a b)))
      (cond
        ((> a 1000) (solve -1000 (+ b 1) besta bestb most))
        ((> b 1000) (* besta bestb))
        ((> n most) (solve (+ a 1) b a b n))
        (else (solve (+ a 1) b besta bestb most))
      )
    ))))
    (solve 980 900 -1000 -1000 0)
  )
)
    
;;---------------------------------
;; Euler Problem 31
;;---------------------------------
(define (sum-from n l)
  (cond
    ((equal? l '()) 0)
    ((< n 0) 0)
    ((= n 0) 1)
    (else (+ (sum-from n (cdr l)) (sum-from (- n (car l)) l)))
  )
)

;;---------------------------------
;; Euler Problem 46
;;---------------------------------
(define (goldbach-check p n)
  (if (equal? p '())
    #f
    (if (integer? (sqrt (/ (- n (car p)) 2)))
      #t
      (goldbach-check (cdr p) n)
    )
  )
)

(define (euler46)
  (letrec ((f (lambda (p n)
    (if (divisible-by-any? n p)
      (if (goldbach-check n)
        (f p (+ n 2))
        n
      )
      (f (cons n p) (+ n 2))
    ))))
    (f '(2) 3)
  )
)

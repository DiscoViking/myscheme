;;---------------------------------
;; Type Conversions
;;---------------------------------
(define (string->integer s)
  (letrec ((f (lambda (chars x m)
    (if (equal? chars '())
      x
      (f (cdr chars) 
        (+ x (* m (- (char->integer (car chars)) 48))) 
        (* m 10)
      )
    ))))
    (f (reverse (string->list s)) 0 1)
  )
)

(define (integer->chars i)
  (letrec ((f (lambda (x chars)
    (if (= x 0)
      chars
      (f (div x 10) (cons (integer->char (+ 48 (modulo x 10))) chars))
    ))))
    (f i '())
  )
)

(define (integer->string i)
  (list->string (integer->chars i))
)

(define (number->bignum i)
  (bignum-normalise (make-bignum i 0))
)

;;---------------------------------
;; Data Types
;;---------------------------------
(define-structure stack items)
(define (new-stack) (make-stack '()))
(define-structure queue items)
(define (new-queue) (make-queue '()))

(define (push x l)
  (cond
    ((stack? l) (stack-items-set! l (cons x (stack-items l))))
    ((queue? l) (queue-items-set! l (cons x (queue-items l))))
  )
)

(define (pop l)
  (cond
    ((stack? l) (let ((x (car (stack-items l))))
      (begin 
        (stack-items-set! l (cdr (stack-items l)))
        x
      ))
    )
    ((queue? l) (let ((is (reverse (queue-items l))))
      (let ((x (car is)))
        (begin
          (queue-items-set! l (reverse (cdr is)))
          x
        )
      ))
    )
  )
)

;;---------------------------------
;; Bignum
;;---------------------------------
(define-structure bignum m e)
(define (convert-exponent b ex)
  (let ((m (bignum-m b)) (e (bignum-e b)))
    (letrec ((f (lambda (m e1 e2)
      (cond
        ((= e2 e1) (make-bignum m e1))
        ((> e2 e1) (f (/ m 10) (+ e1 1) e2))
        ((< e2 e1) (f (* m 10) (- e1 1) e2))
      ))))
      (f m e ex)
    )
  )
)

(define (bignum-normalise b)
  (let ((m (bignum-m b)) (e (bignum-e b)))
    (letrec ((f (lambda (m e)
      (cond
        ((or (>= m 10) (<= m -10)) (f (/ m 10) (+ e 1)))
        ((and (< m 1) (> m -1)) (f (* m 10) (- e 1)))
        (else (make-bignum m e))
      ))))
      (f m e)
    )
  )
)

(define (bignum+ a b)
  (let ((m1 (bignum-m a)) (m2 (bignum-m b)) (e1 (bignum-e a)) (e2 (bignum-e b)))
    (cond 
      ((> e1 e2) (bignum+ a (convert-exponent b e1)))
      ((< e1 e2) (bignum+ (convert-exponent a e2) b))
      (else (bignum-normalise (make-bignum (+ m1 m2) e1)))
    )
  )
)

(define (bignum- a b)
  (let ((m1 (bignum-m a)) (m2 (bignum-m b)) (e1 (bignum-e a)) (e2 (bignum-e b)))
    (cond 
      ((> e1 e2) (bignum- a (convert-exponent b e1)))
      ((< e1 e2) (bignum- (convert-exponent a e2) b))
      (else (bignum-normalise (make-bignum (+ m1 m2) e1)))
    )
  )
)

(define (bignum* a b)
  (let ((m1 (bignum-m a)) (m2 (bignum-m b)) (e1 (bignum-e a)) (e2 (bignum-e b)))
    (bignum-normalise (make-bignum (* m1 m2) (+ e1 e2)))
  )
)

(define (bignum/ a b)
  (let ((m1 (bignum-m a)) (m2 (bignum-m b)) (e1 (bignum-e a)) (e2 (bignum-e b)))
    (bignum-normalise (make-bignum (/ m1 m2) (- e1 e2)))
  )
)

(define (bignum-pow b n)
  (letrec ((f (lambda (b n t)
    (if (= n 0)
      t
      (f b (- n 1) (bignum* b t))
    ))))
    (f b n (number->bignum 1))
  )
)
;;---------------------------------
;; Sorts
;;---------------------------------
(define (comparison func pred)
  (lambda (x y)
    (pred (func x) (func y))
  )
)

(define (quick-sort l pred)
    (if (equal? l '())
        l
        (let ((p (car l)))
            (cat
                (quick-sort (filter (lambda (x) (pred x p)) l) pred)
                (cons p (quick-sort 
                    (filter (lambda (x) (not (pred x p))) (cdr l)) pred))
            )
        )
    )
)

(define (merge left right pred)
  (letrec ((f (lambda (l r m)
    (cond
      ((and (equal? l '()) (equal? m '())) m)
      ((equal? l '()) (cat (reverse r) m))
      ((equal? r '()) (cat (reverse l) m))
      (else (let ((a (car l)) (b (car r)))
        (if (pred a b) (f (cdr l) r (cons a m)) (f l (cdr r) (cons b m)))
      ))
    ))))
    (reverse (f left right '()))
  )
)

(define (trunc-start l s)
  (if (> s 0) (trunc-start (cdr l) (- s 1)) l)
)

(define (trunc-end l s)
  (reverse (trunc-start (reverse l) s))
)

(define (sublist l start end)
  (trunc-start (trunc-end l (- (length l) end)) start)
)

(define (merge-sort l pred)
  (if (<= (length l) 1)
    l
    (let ((mid (floor (/ (length l) 2))))
      (merge (merge-sort (sublist l 0 mid) pred) (merge-sort (sublist l mid (length l)) pred) pred)
    )
  )
)

(define (draw-line w i c)
  (cond
    ((<= i 0) (display "\n"))
    ((<= w 0) (begin (display c) (draw-line w (- i 1) c)))
    (else (begin (display " ") (draw-line (- w 1) i c)))
  )
)

(define (draw-leaves w i c)
  (if (<= i w)
    (begin
      (draw-line (/ (- w i) 2) i c)
      (draw-leaves w (+ i 2) c)
    )
  )
)

(define (draw-stem w i h c)
  (if (> h 0)
    (begin
      (draw-line (/ (- w i) 2) i c)
      (draw-stem w i (- h 1) c)
    )
  )
)

(define (xmas-tree w sw sh l b)
  (draw-leaves w 1 l)
  (draw-stem w sw sh b)
) 

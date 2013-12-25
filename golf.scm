(define(p n m c)(display(if(> n(- 19 m))c " "))(if(< n m)(p(+ n 1)m c)(newline)))
(let l((n 9))(p 0 n '*)(if(< n 20)(l(+ n 1))(p 0 11 '=)))
(define(p w n c)(let l((i(+ n(/(- w n)2))))(display(if(> i n)" " c))(if(> i 0)(l(- i 1))(newline))))
(let l((i 0))(p 30 i '*)(if(> 30 i)(l(+ i 2))(p 30 2 '=)))

;(define(p w n c)(display(if(< w 0)c " "))(if(> n 0)(p(- w 1)(- n 1)c)(newline)))
;(let l((w 8)(n 9))(p w n '*)(if(> w 0)(l(- w 1)(+ n 1))))
;(p 7 10 '=)

(define(p n c)(display c)(if(> n 0)(p(- n 1)c)))
(let l((w 8)(n 0))(p w " ")(p n '*)(newline)(if(> w 0)(l(- w 1)(+ n 2))))
(p 7 " ")(p 2 '=)
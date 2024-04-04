#lang racket
;Zachary Nelson
;Ethan Bylek

;initial rod v
;length n
(define (rodCut v n)
  (if (equal? n 0)0
      {let*
      [(r -1)]
        ;iterate through potential max revenues
        [for ([i n])
          ;check each recursive call
          [set! r (max r (+ (vector-ref v (+ i 1)) (rodCut v (- n (+ i 1)))))
          ]
        ]
        ;outputed revenue
        r}))
;test function
(define (testing)
  (rodCut(make-vector 3 3)2)
)







;Output maximal revenue

;Output optimal length
;optional length is represented by vecotr of sequences of rectangles
;Each rectangle is has a 1-inch length.
;May also want running time.
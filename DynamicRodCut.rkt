#lang racket
;Zachary Nelson
;Ethan Bilyk

;initial rod v
;length n
(define (rodCut v n)
      {let
       ([r -1]
        ;empty results vector v2
        [v2 (make-vector (add1 n) -1)]) 
          ;check each recursive call
        [set! r (rodCutAux v n v2)]
        ;output the revenue
        r}
  )

(define (rodCutAux v n v2)
  (define r 0)
  (if (> (vector-ref v2 n) -1)
      (set! r (vector-ref v2 n))
      ;like if but we're skipping the else case.      
      (when (> n 0)
        (set! r -1)
        ;iterate through potential max revenues
          [for ([i (in-range 0 (min n 10))])
          ;check each recursive call
            (define sum (+ (vector-ref v (add1 i))(rodCutAux v (- n (add1 i)) v2)))
          [set! r (max r sum)]
          ]    
      )
  )
  ;save result, key part of dynammic programming.
  (when (> r (vector-ref v2 n))
    (vector-set! v2 n r)
  )
  ;output revenue.
  r
)


; test function
; this function should be much faster than Naïve version.
(define (testing)
  ; define prices array
  (define prices #(0 1 5 8 9 10 17 17 20 24 30))

  ; define test cases
  (define tests
    `(
      ("Test case 1: Length 1" ,(rodCut prices 1) 1)
      ("Test case 2: Length 5" ,(rodCut prices 5) 13)
      ("Test case 1: Length 10" ,(rodCut prices 10) 30)
      ("Test case 2: Length 11" ,(rodCut prices 11) 31)
      ("Test case 2: Length 20" ,(rodCut prices 20) 60)
      ("Test case 3: Length 30" ,(rodCut prices 30) 90)
      ("Test case 4: Length 40" ,(rodCut prices 40) 120)
      ("Test case 5: Length 50" ,(rodCut prices 50) 150)
      ))

  ; for each test case, run the test
  (for-each
   (lambda (test)
     (let ((name (car test))
           (result (cadr test))
           (expected (caddr test)))
       (display name)
       ; if it passes the test, display "passed"
       (if (= result expected)
           (displayln ": Passed")
           (begin
             ; else, display "failed" along with the incorrect result
             (display ": Failed, expected ")
             (display expected)
             (display " but got ")
             (displayln result)))))
   tests))






;Output maximal revenue

;Output optimal length
;optional length is represented by vecotr of sequences of rectangles
;Each rectangle is has a 1-inch length.
;May also want running time.
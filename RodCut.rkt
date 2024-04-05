#lang racket
;Zachary Nelson
;Ethan Bilyk

;initial rod v
;length n
(define (rodCut v n)
  (if (zero? n)
      0
      {let
       ([r -1])
        ;iterate through potential max revenues
        [for ([i (in-range 1 (add1 n))])
          (if (> i 10)
              (set! i 1)
          ;check each recursive call
          [set! r (max r (+ (vector-ref v i) (rodCut v (- n i))))
          ])
         ]
        ;outputed revenue
        r}))


; test function
; this fuction takes a long time to run
; so use (testing) at your own risk
(define (testing)
  ; define prices array
  (define prices #(0 1 5 8 9 10 17 17 20 24 30))

  ; define test cases
  (define tests
    `(("Test case 1: Length 10" ,(rodCut prices 10) 30)
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

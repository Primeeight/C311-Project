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
        [for ([i (in-range 1 (min n 10))])
          ;check each recursive call
          [set! r (max r (+ (vector-ref v  (add1 i)) (rodCut v (- n (+ i 1)))))
          ]
         ]
        ;outputed revenue
        r}))


; test function
; this function takes a long time to run
; so use (run-test) at your own risk
(define prices #(0 1 5 8 9 10 17 17 20 24 30))

(define (run-test test)
  (let* ((name (car test))
         (expected (caddr test))
         (testing (cadr test)))
    (let ((result (time (testing))))
      (display name)
      ; if this passes
      (if (= result expected)
          (displayln ": Passed\n")
          ; else, print it failed
           (begin
            (display ": Failed, expected ")
            (display expected)
            (display " but got ")
            (displayln result))))))

(define (run-tests tests)
  (for-each run-test tests))

; add test cases here in this form to get runtime
(define tests
  `(
    ("Test case 1: Length 10" ,(lambda () (rodCut prices 10)) 30)
    ("Test case 2: Length 20" ,(lambda () (rodCut prices 20)) 60)
    ("Test case 3: Length 30" ,(lambda () (rodCut prices 30)) 90)
    ("Test case 4: Length 40" ,(lambda () (rodCut prices 40)) 120)
    ("Test case 5: Length 50" ,(lambda () (rodCut prices 50)) 150)
    ))

; run the tests
(display "Running tests...\n")
(run-tests tests)



;Output optimal length
;optional length is represented by vecotr of sequences of rectangles
;Each rectangle is has a 1-inch length.

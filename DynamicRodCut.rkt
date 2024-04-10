#lang slideshow
;Zachary Nelson
;Ethan Bilyk

;initial rod v
;length n
(define (rodCut v n)
      {let
       ([r -1]
        ;empty results vector v2
        [v2 (make-vector (add1 n) -1)]
        ;empty optimal lengths vector v3
        [v3 (make-vector (add1 n) -1)]
       ) 
        ;recursive helper function
        [set! r (rodCutAux v n v2 v3)]
        ;print optimal length
        [printSolution n v3]
        ;output the revenue
       r
        }
  )

(define (rodCutAux v n v2 v3)
  (define r 0)
  (define l 0)
  (if (> (vector-ref v2 n) -1)
      (set! r (vector-ref v2 n))
      ;like if statement but skips the else case.      
      (when (> n 0)
        (set! r -1)
        ;iterate through potential max revenues
          [for ([i (in-range 0 (min n 10))])
          ;check each recursive call
            (define sum (+ (vector-ref v (add1 i))(rodCutAux v (- n (add1 i)) v2 v3))) 
            [when (> sum r)
            [set! r (max r sum)]
            [set! l (add1 i)]
              ]
          ]    
      )
  )
  ;save result, key part of dynammic programming.
  (when (> r (vector-ref v2 n))
    (vector-set! v2 n r)
  )
  ;save optimal length of result.
  (when (> l (vector-ref v3 n))
    (vector-set! v3 n l)
  )
  ;output revenue.
  r
)

(define (rodpiece n)
  ;representation of a 1-inch rod.
(let* ([unitt (colorize (rectangle 20 15) "red")])
(apply hc-append (vector->list(make-vector n unitt)))
  )
  )

(define (printSolution n s)
  (print (printSolutionAux n s))
  (display "\n")
  )

(define (printSolutionAux n s)
  (if (> n 0)
  (let* ([element (vector-ref s n)])
    ;Converts the picture given to a vector
    (vector-append (vector (rodpiece element)) (printSolutionAux(- n element) s)))
  #()
  )
  )


; test function
; this function should be much faster than Naïve version.
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
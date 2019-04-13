#lang racket

(require rackunit)
(require rackunit/text-ui)

;; test ... -> void
;; Execute various tests
(define (run-much-tests . tests)
  (run-tests (test-suite "tests" tests))
  (void))

;; SUM
;; binary-tree -> number
;; retrieves the distance from the root to the longest node
(define sum-tests
  (test-suite
   "sum tests"
   (check-equal? (sum 0) 0)
   (check-equal? (sum 1) 1)
   (check-equal? (sum 2) 3)
   (check-equal? (sum 3) 6)
   (check-equal? (sum 4) 10)))1
(define (sum n)
  (cond [(zero? n) 0]
        [else (+ n
                 (sum (sub1 n)))]))

;; LISTN
;; natural -> list of naturals
;; retrieves a list containing all naturals from given to 1
(define listn-tests
  (test-suite
   "listn tests"
   (check-equal? (listn 0) empty)
   (check-equal? (listn 1) (list 1))
   (check-equal? (listn 2) (list 2 1))
   (check-equal? (listn 3) (list 3 2 1))))
(define (listn n)
  (if (zero? n)
      empty
      (cons n (listn (sub1 n)))))

;; HAS-DIV
;; natural natural -> boolean
;; retrieves if there's any divisor for the second number between 2 and the first argument.
(define has-div-tests
  (test-suite
   "has-div tests"
   (check-equal? (has-div 1 5) #f)
   (check-equal? (has-div 2 2) #t)
   (check-equal? (has-div 5 5) #t)
   (check-equal? (has-div 5 10) #t)
   (check-equal? (has-div 7 8) #t)
   (check-equal? (has-div 2 9) #f)))
(define (has-div i x)
  (if (<= i 1)
      #f
      (or (zero? (remainder x i))
          (has-div (sub1 i) x))))

(run-much-tests sum-tests
                listn-tests
                has-div-tests)
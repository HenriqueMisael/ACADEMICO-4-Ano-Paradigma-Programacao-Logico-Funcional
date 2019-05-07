#lang racket

(require rackunit)
(require rackunit/text-ui)

;; test ... -> void
;; Execute various tests
(define (run-much-tests . tests)
  (run-tests (test-suite "tests" tests))
  (void))

;;EXERCÍCIO 4
(define primeira-tests
  (test-suite
   "primeira tests"
   (check-equal? (primeira 1 2) 6)))
(define (primeira a b)
  (let ([exp (* 3 a)])
    (+ (- exp b) (+ exp b))))

;; EXERCÍCIO 8
;; (predicate? number) list -> number
;; retrieves 0 if the list is empty. If not, retrieves the number of elements where the given predicate is true
(define count-match-tests
  (test-suite
   "count-match tests"
   (check-equal? (count-match zero? empty) 0)
   (check-equal? (count-match zero? (list 1 -1 2 3 -2 5)) 0)
   (check-equal? (count-match negative? (list 1 -1 2 3 -2 5)) 2)
   (check-equal? (count-match positive? (list 1 -1 2 3 -2 5)) 4)))
(define (count-match predicate? lst)
  (if (empty? lst)
      0
      (+ (if (predicate? (first lst))
             1
             0)
         (count-match predicate? (rest lst)))))

;; EXERCÍCIO 9
;; (function n) -> (function n)
;; retrieves a function that apply the given function to itself
(define double-tests
  (test-suite
   "double tests"
   (check-equal? ((double add1) 1) 3)
   (check-equal? ((double sub1) 3) 1)
   (check-equal? ((double sub1) 0) -2)
   (check-equal? ((double sqrt) 1) 1)
   (check-equal? ((double sqrt) 16) 2)))
(define (double func)
  (lambda (x) (func (func x))))


(run-much-tests primeira-tests
                count-match-tests
                double-tests)
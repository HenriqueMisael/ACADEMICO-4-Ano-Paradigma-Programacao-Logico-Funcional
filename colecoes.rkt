#lang racket

(require rackunit)
(require rackunit/text-ui)

;; test ... -> void
;; Execute various tests
(define (run-much-tests . tests)
  (run-tests (test-suite "tests" tests))
  (void))

;; REMOVE-FROM-LIST
;; list number -> list
;; Return a new list with the same elements of the given list but the first ocurrence of the given number
(define remove-tests
  (test-suite
   "split-list-test"
   (check-equal? (remove empty 0) empty)
   (check-equal? (remove (list 4) 0) (list 4))
   (check-equal? (remove (list 4) 4) empty)
   (check-equal? (remove (list 0 1) 0) (list 1))
   (check-equal? (remove (list 0 1) 1) (list 0))
   (check-equal? (remove (list 0 1) 4) (list 0 1))
   (check-equal? (remove (list 0 1 0) 0) (list 1 0))
   (check-equal? (remove (list 0 1 0 2) 2) (list 0 1 0))))
(define (remove l x)
  (cond
    [(empty? l) empty]
    [(= (first l) x) (rest l)]
    [else (cons (first l)
                (remove (rest l) x))]))

;; SUM-NESTED-LIST
;; nested-list -> number
;; sum all elements found in the nested-list
(define sum-nested-list-tests
  (test-suite
   "split-list-test"
   (check-equal? (sum-nested-list empty) 0)
   (check-equal? (sum-nested-list (list 1)) 1)
   (check-equal? (sum-nested-list (list 1 2)) 3)
   (check-equal? (sum-nested-list (list (list 1) 2)) 3)
   (check-equal? (sum-nested-list (list (list 3 1) 2)) 6)
   (check-equal? (sum-nested-list (list (list 3 1) (list 1))) 5)
   (check-equal? (sum-nested-list (list (list 3 1) (list 1 1))) 6)))
(define (sum-nested-list l)
  (cond [(empty? l) 0]
        [(list? (first l)) (+ (sum-nested-list (first l))
                              (sum-nested-list (rest l)))]
        [else (+ (first l)
                 (sum-nested-list (rest l)))]))

(run-much-tests remove-tests
                sum-nested-list-tests)
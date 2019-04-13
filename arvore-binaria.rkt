#lang racket

(require rackunit)
(require rackunit/text-ui)

;; test ... -> void
;; Execute various tests
(define (run-much-tests . tests)
  (run-tests (test-suite "tests" tests))
  (void))

(struct binary-tree (v left right) #:transparent)
;; could be:
;;  - empty; or
;;  - (v left right), where v is a value, left and right are binary-tree

;; HEIGHT
;; binary-tree -> number
;; retrieves the distance from the root to the longest node
(define t0 (binary-tree 10 empty empty))
(define t1 (binary-tree 3 t0 empty))
(define t2 (binary-tree 5 t1 empty))
(define t3 (binary-tree 6 t2 (binary-tree 4 empty empty)))
(define t4 (binary-tree 9 t2 t3))
(define height-tests
  (test-suite
   "height tests"
   (check-equal? (height empty) -1)
   (check-equal? (height t0) 0)
   (check-equal? (height t1) 1)
   (check-equal? (height t2) 2)
   (check-equal? (height t3) 3)
   (check-equal? (height t4) 4)))

(define (height t)
  (cond [(empty? t) -1]
        [else (+ 1
                 (max (height (binary-tree-left t))
                      (height (binary-tree-right t))))]))

(run-much-tests height-tests)
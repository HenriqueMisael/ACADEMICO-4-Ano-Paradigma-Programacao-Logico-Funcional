#lang racket

(require rackunit)
(require rackunit/text-ui)

;; test ... -> void
;; Execute various tests
(define (run-much-tests . tests)
  (run-tests (test-suite "tests" tests))
  (void))

;; -----------------------------------------------------
;; GREATEST
;; number number -> number
;; retrieves the greatest number between two given ones.
(define greatest-tests
  (test-suite
   "greatest tests"
   (check-equal? (greatest 1 0) 1)
   (check-equal? (greatest 1 1) 1)
   (check-equal? (greatest 1 2) 2)))
(define (greatest a b)
  (if(< a b)
     b
     a))

;; -----------------------------------------------------
;; SQUARE
;; number -> number
;; retrieves square of given number
(define square-tests
  (test-suite
   "square tests"
   (check-equal? (square 1) 1)
   (check-equal? (square 2) 4)
   (check-equal? (square -2) 4)))
(define (square a)
  (* a a))
   
;; -----------------------------------------------------
;; PLUS-SQUARES
;; number number -> number
;; retrieves the sum of the square of given two numbers
(define plus-squares-tests
  (test-suite
   "plus-squares tests"
   (check-equal? (plus-squares 1 2) 5)
   (check-equal? (plus-squares 2 1) 5)))
(define (plus-squares a b)
  (+ (square a)
     (square b)))

;; -----------------------------------------------------
;; SUM-SQUARE-2-GREATEST
;; number number number -> number
;; retrieves the sum of the square of the two greatest numbers
(define sum-square-2-greatest-tests
  (test-suite
   "sum-square-2-greatest tests"
   (check-equal? (sum-square-2-greatest 1 2 3) 13)
   (check-equal? (sum-square-2-greatest 3 5 4) 41)
   (check-equal? (sum-square-2-greatest 5 4 6) 61)
   (check-equal? (sum-square-2-greatest 3 5 1) 34)
   (check-equal? (sum-square-2-greatest 5 1 3) 34)
   (check-equal? (sum-square-2-greatest 4 2 0) 20)))
(define (sum-square-2-greatest a b c)
  (if(= (greatest a b) a)
     (if(= (greatest b c) b)
        (plus-squares a b)
        (plus-squares a c))
     (if(= (greatest a c) a)
        (plus-squares b a)
        (plus-squares b c))))

;; -----------------------------------------------------
;; DISTANCE
;; number number -> number
;; retrieves the distance from the point (x,y) to the origin (0,0)
(define distance-tests
  (test-suite
   "distance tests"
   (check-equal? (distance 0 0) 0)
   (check-equal? (distance 3 4) 5)
   (check-equal? (distance 12 16) 20)))
(define (distance x y)
  (sqrt (+ (square x)
           (square y))))

;; -----------------------------------------------------
;; TRIANGLE
;; number number number -> string
;; return kind of triangle that has given arests lengths
(define triangle-tests
  (test-suite
   "triangle tests"
   (check-equal? (triangle 3 4 5) "escaleno")
   (check-equal? (triangle 3 3 3) "equilater")
   (check-equal? (triangle 5 3 5) "isoceles")
   (check-equal? (triangle 2 4 6) "escaleno")))
(define (triangle a b c)
  (if(= a b)
     (if(= a c)
        "equilater"
        "isoceles")
     (if(= a c)
        "isoceles"
        "escaleno")))
  
(run-much-tests greatest-tests
                square-tests
                plus-squares-tests
                sum-square-2-greatest-tests
                distance-tests
                triangle-tests)

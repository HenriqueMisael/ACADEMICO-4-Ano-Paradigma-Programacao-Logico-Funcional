#lang racket

(require rackunit)
(require rackunit/text-ui)

;; test ... -> void
;; Execute various tests
(define (run-much-tests . tests)
  (run-tests (test-suite "tests" tests))
  (void))

;; number -> number
;; calculate twice the value
(define double-tests
  (test-suite
   "double tests"
   (check-equal? (double 2) 4)
   (check-equal? (double -3.0) -6.0)
   (check-equal? (double 1.2) 2.4)))
(define (double x)
  (* 2 x))

;; natural -> natural
;; check if the given number is pair (multiple of 2)
(define pair?-tests
  (test-suite
   "pair? tests"
   (check-equal? (pair? 2) #t)
   (check-equal? (pair? 3) #f)))
(define (pair? n)
  (= 0 (remainder n 2)))

;; string string -> string
;; retrieves the greatest word between two given words. In case of doubt, it will retrieve the first
(define greatest-string-tests
  (test-suite
   "greatest-string tests"
   (check-equal? (greatest-string "house" "banana") "banana")
   (check-equal? (greatest-string "mouse" "par") "mouse")
   (check-equal? (greatest-string "" "par") "par")
   (check-equal? (greatest-string "he" "it") "he")))
(define (greatest-string a b)
  (if(< (string-length a) (string-length b))
  b
  a))
  
(run-much-tests double-tests
                pair?-tests
                greatest-string-tests)

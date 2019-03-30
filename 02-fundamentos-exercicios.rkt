#lang racket

(require rackunit)
(require rackunit/text-ui)

;; test ... -> void
;; Execute various tests
(define (run-much-tests . tests)
  (run-tests (test-suite "tests" tests))
  (void))

;; ====EX=6=============================================
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

;; ====EX=7=============================================
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

;; ====EX=8=============================================
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

;; ====EX=9=============================================
;; -----------------------------------------------------
;; TRIANGLE-TYPE
;; number number number -> string
;; return kind of triangle that has given arests lengths. If all three are equal, then it is "equilater"; if two of them are equal, then it is "isoceles"; in wich case them are all different, it is a "escaleno" triangle
(define triangle-type-tests
  (test-suite
   "triangle tests"
   (check-equal? (triangle-type 3 4 5) "escaleno")
   (check-equal? (triangle-type 5 3 5) "isoceles")
   (check-equal? (triangle-type 5 5 3) "isoceles")
   (check-equal? (triangle-type 3 5 5) "isoceles")
   (check-equal? (triangle-type 3 3 3) "equilater")))
(define (triangle-type a b c)
  (if(= a b)
     (if(= a c)
        "equilater"
        "isoceles")
     (if(or(= a c)
           (= b c))
        "isoceles"
        "escaleno")))

;; ====EX=10============================================
;; -----------------------------------------------------
;; TRANSLATE-IMC-INDEX
;; number -> string
;; return obesity descrition given IMC index
(define translate-imc-index-tests
  (test-suite
   "how-obese-tests"
   (check-equal? (translate-imc-index 24.69) "not obese")
   (check-equal? (translate-imc-index 27.77) "overweight")
   (check-equal? (translate-imc-index 30.86) "obesity I")
   (check-equal? (translate-imc-index 37.04) "obesity II")
   (check-equal? (translate-imc-index 46.296) "obesity III")))
(define (translate-imc-index i)
  (cond
    [(< i 25) "not obese"]
    [(< i 30) "overweight"]
    [(< i 35) "obesity I"]
    [(< i 40) "obesity II"]
    [else "obesity III"]))

;; -----------------------------------------------------
;; HOW-OBESE
;; number number -> string
;; return obesity degree given weight and height of a person. It could be "overweight", "obesity I", "obesity II" or "obesity III". If there's no obesity degree, then it would return "no obesity"
(define how-obese-tests
  (test-suite
   "how-obese-tests"
   (check-equal? (how-obese 80.0 1.80) "not obese")
   (check-equal? (how-obese 90.0 1.80) "overweight")
   (check-equal? (how-obese 100.0 1.80) "obesity I")
   (check-equal? (how-obese 120.0 1.80) "obesity II")
   (check-equal? (how-obese 150.0 1.80) "obesity III")))
(define (how-obese w h)
  (translate-imc-index (/ w
                          (* h h))))

;; ====EX=11=============================================
;; INCREASE-RATE
;; number -> number (%)
;; retrieves the incresce rate for given current salary value
(define increase-rate-tests
  (test-suite
   "increse-rate-tests"
   (check-equal? (increse-rate 1199) 0.1)
   (check-equal? (increse-rate 1200) 0.1)
   (check-equal? (increse-rate 1201) 0.07)
   (check-equal? (increse-rate 2999) 0.07)
   (check-equal? (increse-rate 3000) 0.07)
   (check-equal? (increse-rate 3001) 0.03)
   (check-equal? (increse-rate 7999) 0.03)
   (check-equal? (increse-rate 8000) 0.03)
   (check-equal? (increse-rate 8001) 0)))
(define (increse-rate x)
  (cond [(<= x 1200) 0.10]
        [(<= x 3000) 0.07]
        [(<= x 8000) 0.03]
        [else 0]))

;; INCREASED-SALARY
;; number -> number
;; retrieves the incresed salary based on the current salary and the increse rate respecting: <= 1200, 10%; 1201-3000, 7%; 3001-8000, 3%; otherwise, 0%)
(define increased-salary-tests
  (test-suite
   "incresed-salary-tests"
   (check-equal? (increased-salary 0) 0)
   (check-equal? (increased-salary 1200.0) 1320.0)
   (check-equal? (increased-salary 3000.0) 3210.0)
   (check-equal? (increased-salary 8000.0) 8240.0)
   (check-equal? (increased-salary 8001.0) 8001.0)))
(define (increased-salary x)
  (* (+ 1 (increse-rate x))
     x))
   
(run-much-tests greatest-tests
                square-tests
                plus-squares-tests
                sum-square-2-greatest-tests
                distance-tests
                triangle-type-tests
                translate-imc-index-tests
                how-obese-tests
                increase-rate-tests
                increased-salary-tests)

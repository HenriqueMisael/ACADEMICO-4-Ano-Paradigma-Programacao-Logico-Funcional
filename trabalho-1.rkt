#lang racket

(require rackunit)
(require rackunit/text-ui)

;; test ... -> void
;; Execute various tests
(define (run-much-tests . tests)
  (run-tests (test-suite "tests" tests))
  (void))

;; ======= LISTA FUNDAMENTOS =======
;; ====EX=9=========================
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

;; ===== LISTA DADOS COMPOSTOS =====
;; ====EX=2=========================
;; REMOVE-ALL
;; list number -> list
;; retrieves a list with the same elements of given list but the ocurrences of given number
(define remove-all-tests
  (test-suite
   "remove-all tests"
   (check-equal? (remove-all empty 0) empty)
   (check-equal? (remove-all (list 0) 0) empty)
   (check-equal? (remove-all (list 0 0) 0) empty)
   (check-equal? (remove-all (list 1) 0) (list 1))
   (check-equal? (remove-all (list 0 1) 0) (list 1))))
(define (remove-all lst n)
  (cond [(empty? lst) empty]
        [(= (first lst) n) (remove-all (rest lst) n)]
        [else (cons (first lst) (remove-all (rest lst) n))]))

;; ====EX=8=========================
;; TAIL
;; list -> number
;; retrieves the last element of given list. It will throw an error if the list is empty
(define tail-tests
  (test-suite
   "tail tests"
   (check-exn exn:fail? (thunk (tail empty)))
   (check-equal? (tail (list 0)) 0)
   (check-equal? (tail (list 0 1)) 1)
   (check-equal? (tail (list 0 3 1)) 1)
   (check-equal? (tail (list 0 3 1 4 14)) 14)))
(define (tail lst)
  (cond [(empty? lst) (error "empty list")]
        [(empty? (rest lst)) (first lst)]
        [else (tail (rest lst))]))

;; ====EX=12========================
;; REMOVE-1ST-EQUALS
;; list number -> list
;; retrieves a list with all elements of given list but the first elements that are equal to given argument
(define remove-1st-equals-tests
  (test-suite
   "remove-1st-equals tests"
   (check-equal? (remove-1st-equals empty 1) empty)
   (check-equal? (remove-1st-equals (list 1) 1) empty)
   (check-equal? (remove-1st-equals (list 1 1) 1) empty)
   (check-equal? (remove-1st-equals (list 2 1) 1) (list 2 1))
   (check-equal? (remove-1st-equals (list 2 1) 2) (list 1))))
(define (remove-1st-equals lst n)
  (cond [(empty? lst) empty]
        [(= (first lst) n) (remove-1st-equals (rest lst) n)]
        [else lst]))
 
;; REMOVE-DUPLICATES
;; list -> list
;; retrieves a list with all the elements of the given list but without consecutive duplicated elements
(define remove-duplicates-tests
  (test-suite
   "remove-duplicates tests"
   (check-equal? (remove-duplicates empty) empty)
   (check-equal? (remove-duplicates (list 1)) (list 1))
   (check-equal? (remove-duplicates (list 1 1)) (list 1))
   (check-equal? (remove-duplicates (list 1 1 2)) (list 1 2))
   (check-equal? (remove-duplicates (list 1 2 1)) (list 1 2 1))
   (check-equal? (remove-duplicates (list 1 2 2 1)) (list 1 2 1))
   (check-equal? (remove-duplicates (list 1 1 1 1 2 3 3 4 4 5 5 5)) (list 1 2 3 4 5))))
(define (remove-duplicates lst)
  (cond [(empty? lst) empty]
        [else (cons (first lst)
                    (remove-duplicates (remove-1st-equals (rest lst)
                                                          (first lst))))]))
;; ====EX=16========================
(struct b-tree (v left right) #:transparent)
;; could be:
;;  - empty; or
;;  - (v left right), where v is a value, left and right are binary-tree

;; ROOT-B-TREE
;; number -> binary-tree
;; retrieves a b-tree only with root value that is the given number
(define (root-b-tree n)
  (b-tree n empty empty))

;; IS-SEARCH
;; b-tree -> boolean
;; retrieves if the binary-tree is search-binary-tree or not
(define t0 (root-b-tree 0))
(define t1 (b-tree 0 (root-b-tree -1) empty))
(define t2 (b-tree 0 empty (root-b-tree 1)))
(define t3 (b-tree 0 empty (root-b-tree -1)))
(define t4 (b-tree 0 (root-b-tree 1) empty))
(define t5 (b-tree 0 (b-tree -10 (root-b-tree -11) (root-b-tree -1)) (b-tree 10 (root-b-tree 1) (root-b-tree 11))))
(define is-search-tests
  (test-suite
   "is-search tests"
   (check-equal? (is-search empty) #t)
   (check-equal? (is-search t0) #t)
   (check-equal? (is-search t1) #t)
   (check-equal? (is-search t2) #t)
   (check-equal? (is-search t3) #f)
   (check-equal? (is-search t4) #f)
   (check-equal? (is-search t5) #t)))
(define (is-search t)
  (cond [(empty? t) #t]
        [(and (not (empty? (b-tree-left t))) ((b-tree-v (b-tree-left t)) . > . (b-tree-v t))) #f]
        [(and (not (empty? (b-tree-right t))) ((b-tree-v (b-tree-right t)) . < . (b-tree-v t))) #f]
        [else (and (is-search (b-tree-left t))
                   (is-search (b-tree-right t)))]))

;; ======== LISTA NATURAIS =========
;; ====EX=2=========================
;; SUM
;; number number -> number
;; retrieves the sum of the given numbers
(define sum-tests
  (test-suite
   "sum tests"
   (check-equal? (sum 0 1) 1)
   (check-equal? (sum 0 2) 2)
   (check-equal? (sum 1 0) 1)
   (check-equal? (sum 2 0) 2)
   (check-equal? (sum 2 1) 3)
   (check-equal? (sum 1 3) 4)
   (check-equal? (sum 2 3) 5)
   (check-equal? (sum 3 2) 5)
   (check-equal? (sum 3 -1) 2)
   (check-equal? (sum -2 3) 1)
   (check-equal? (sum 0 -1) -1)))
(define (sum x y)
  (cond [(zero? y) x]
        [(< y 0) (sub1 (sum x (add1 y)))]
        [else (add1 (sum x (sub1 y)))]))

(define sub-tests
  (test-suite
   "sub tests"
   (check-equal? (sub 0 1) -1)
   (check-equal? (sub 0 2) -2)
   (check-equal? (sub 1 0) 1)
   (check-equal? (sub 2 0) 2)
   (check-equal? (sub 2 1) 1)
   (check-equal? (sub 1 3) -2)
   (check-equal? (sub 2 3) -1)
   (check-equal? (sub 3 2) 1)
   (check-equal? (sub 3 -1) 4)
   (check-equal? (sub -2 3) -5)
   (check-equal? (sub 0 -1) 1)))
(define (sub x y)
  (cond [(zero? y) x]
        [(< y 0) (add1 (sub x (add1 y)))]
        [else (sub1 (sub x (sub1 y)))]))

;; MULT
;; number number -> number
;; retrieves the product of the given numbers
(define mult-tests
  (test-suite
   "mult tests"
   (check-equal? (mult 0 1) 0)
   (check-equal? (mult 0 2) 0)
   (check-equal? (mult 1 0) 0)
   (check-equal? (mult 2 0) 0)
   (check-equal? (mult 2 1) 2)
   (check-equal? (mult 1 3) 3)
   (check-equal? (mult 2 3) 6)
   (check-equal? (mult 3 2) 6)
   (check-equal? (mult 3 -1) -3)
   (check-equal? (mult -2 3) -6)
   (check-equal? (mult 0 -1) 0)))
(define (mult x y)
  (cond [(zero? y) 0]
        [(< y 0) (sub (mult x (add1 y)) x)]
        [else (sum x (mult x (sub1 y)))]))

;; ==== LISTA COMBINAÇÃO MODELOS ===
;; ====EX=3=========================
;; DROP
;; list natural -> list
;; retrieves a list containing the elements of the given list but the first n elements. It will throw an error if the number of elements to remove is higher than the list lenght
(define drop-tests
  (test-suite
   "drop tests"
   (check-equal? (drop empty 0) empty)
   (check-exn exn:fail? (thunk (drop empty 1)))
   (check-equal? (drop (list 10 40 70 20 3) 0) (list 10 40 70 20 3))
   (check-equal? (drop (list 10 40 70 20 3) 1) (list 40 70 20 3))
   (check-equal? (drop (list 10 40 70 20 3) 2) (list 70 20 3))))
(define (drop lst n)
  (cond [(zero? n) lst]
        [(empty? lst) (error "cannot remove more elements than the list have")]
        [else (drop (rest lst) (sub1 n))]))
;; ====EX=5=========================
;; INSERT-AT
;; list number natural -> list
;; retrieves a list containing the elements of the given list plus the first number argument in the given position (second number argument). It will throw an error if the given index is not in the list or it is not the next new element
(define insert-at-tests
  (test-suite
   "insert-at tests"
   (check-equal? (insert-at empty 30 0) (list 30))
   (check-exn exn:fail? (thunk (insert-at empty 30 1)))
   (check-equal? (insert-at (list 10 40 70 20 3) 30 0) (list 30 10 40 70 20 3))
   (check-equal? (insert-at (list 10 40 70 20 3) 30 1) (list 10 30 40 70 20 3))
   (check-equal? (insert-at (list 10 40 70 20 3) 30 2) (list 10 40 30 70 20 3))))
(define (insert-at lst e n)
  (cond [(zero? n) (cons e lst)]
        [(empty? lst) (error "bad index")]
        [else (cons (first lst) (insert-at (rest lst) e (sub1 n)))]))
;; ====EX=8=========================
;; APPEND
;; list list -> list
;; retrieves a list containing the elements of the given list plus the given list elements as the last arguments.
(define append-tests
  (test-suite
   "append tests"
   (check-equal? (append empty (list 30)) (list 30))
   (check-equal? (append (list 10 ) (list 30)) (list 10 30))
   (check-equal? (append (list 10 40) (list 30)) (list 10 40 30))
   (check-equal? (append (list 10) empty) (list 10))
   (check-equal? (append (list 10) (list 40 30)) (list 10 40 30))))
(define (append lsta lstb)
  (cond [(empty? lsta) lstb]
        [(empty? lstb) lsta]
        [else (cons (first lsta) (append (rest lsta) lstb))]))

(run-much-tests increase-rate-tests
                increased-salary-tests
                remove-all-tests
                tail-tests
                remove-1st-equals-tests
                remove-duplicates-tests
                is-search-tests
                sum-tests
                sub-tests
                mult-tests
                drop-tests
                insert-at-tests
                append-tests)
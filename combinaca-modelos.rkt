#lang racket

(require rackunit)
(require rackunit/text-ui)

;; test ... -> void
;; Execute various tests
(define (run-much-tests . tests)
  (run-tests (test-suite "tests" tests))
  (void))

;; FIND
;; list number -> number
;; retrieves k-th element of the given list
(define find-tests
  (test-suite
   "find tests"
   (check-exn exn:fail? (thunk (find empty 0)))
   (check-exn exn:fail? (thunk (find empty 2)))
   (check-equal? (find (list 1) 0) 1)
   (check-equal? (find (list 1 2) 1) 2)
   (check-equal? (find (list 1 3 2) 1) 3)
   (check-exn exn:fail? (thunk (find (list 1 3 2) 4)))))
(define (find l k)
  (cond [(empty? l) (error "bad index")]
        [(zero? k) (first l)]
        [else (find (rest l) (sub1 k))]))

;; GREATER?
;; natural natural -> boolean
;; retrieves if the first number is greater than
(define greater?-tests
  (test-suite
   "greater? tests"
   (check-equal? (greater? 0 0) #f)
   (check-equal? (greater? 0 1) #f)
   (check-equal? (greater? 1 0) #t)
   (check-equal? (greater? 1 1) #f)
   (check-equal? (greater? 2 1) #t)
   (check-equal? (greater? 2 2) #f)
   (check-equal? (greater? 3 1) #t)))
(define (greater? x y)
  (cond [(zero? x) #f]
        [(zero? y) #t]
        [else (greater? (sub1 x) (sub1 y))]))

;; TAKE
;; list natural -> list
;; retrieves a list containing the first n elements of the given list. If the given number is higher than the list content, the output will be identical the input.
(define take-tests
  (test-suite
   "take tests"
   (check-equal? (take empty 0) empty)
   (check-equal? (take empty 1) empty)
   (check-equal? (take (list 10 40) 3) (list 10 40))
   (check-equal? (take (list 10 40 70 20 3) 0) empty)
   (check-equal? (take (list 10 40 70 20 3) 1) (list 10))
   (check-equal? (take (list 10 40 70 20 3) 2) (list 10 40))))
(define (take lst n)
  (cond [(or (zero? n) (empty? lst)) empty]
        [else (cons (first lst) (take (rest lst) (sub1 n)))]))

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

;; REMOVE-AT
;; list natural -> list
;; retrieves a list containing the elements of the given list but the element at given position (0-indexed). It will throw an error if the given index is not in the list (also if the list is empty)
(define remove-at-tests
  (test-suite
   "remove-at tests"
   (check-exn exn:fail? (thunk (remove-at empty 0)))
   (check-exn exn:fail? (thunk (remove-at empty 1)))
   (check-equal? (remove-at (list 10 40 70 20 3) 0) (list 40 70 20 3))
   (check-equal? (remove-at (list 10 40 70 20 3) 1) (list 10 70 20 3))
   (check-equal? (remove-at (list 10 40 70 20 3) 2) (list 10 40 20 3))))
(define (remove-at lst n)
  (cond [(empty? lst) (error "bad index")]
        [(zero? n) (rest lst)]
        [else (cons (first lst) (remove-at (rest lst) (sub1 n)))]))

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

(run-much-tests find-tests
                greater?-tests
                take-tests
                drop-tests
                remove-at-tests
                insert-at-tests
                append-tests)
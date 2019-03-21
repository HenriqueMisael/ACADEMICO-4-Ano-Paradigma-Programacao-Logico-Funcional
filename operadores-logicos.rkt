;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname operadores-logicos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (e-logico x y)
  (cond
    [x y]
    [else #f]))
(define (ou-logico x y)
  (cond
    [x #t]
    [y #t]
    [else #f]))
(define (nao-logico x)
  (if x #f #t))

(check-expect (e-logico #f #f) #f)
(check-expect (e-logico #f #t) #f)
(check-expect (e-logico #t #f) #f)
(check-expect (e-logico #t #t) #t)
(check-expect (ou-logico #f #f) #f)
(check-expect (ou-logico #f #t) #t)
(check-expect (ou-logico #t #f) #t)
(check-expect (ou-logico #t #t) #t)
(check-expect (nao-logico #t) #f)
(check-expect (nao-logico #f) #t)
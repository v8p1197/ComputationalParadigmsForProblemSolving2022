#lang scheme
(require racket/stream)

; ------------------------------------- ;
; Basic functions for stream management ;
; ------------------------------------- ;

; Empty stream
(define empty-stream '())

; Concate
(define-syntax cons-stream
  (syntax-rules ()
    ( (cons-stream expr1 expr2)
      (cons expr1 (delay expr2)))))

; Returns true if a stream is null
(define stream-null? null?)

; Head of a stream
(define stream-car car)

; Returns a stream starting from the next element of a stream
(define (stream-cdr s) (force (cdr s)))

; Returns the next element of a stream
(define (stream-cadr s) (stream-car (stream-cdr s)))

; Returns a stream starting from the next-next element of a stream
(define (stream-cddr s) (stream-cdr (stream-cdr s)))

; Returns the next-next element of a stream
(define (stream-caddr s) (stream-cdr (stream-cdr s)))

; Maps a stream into another stream with the unary operator op
(define (map-stream op s)
  (cons-stream (op (stream-car s))
               (map-stream op (stream-cdr s))))

; Maps a stream into another stream with the binary operator (x op s)
(define (map2-stream op x s)
  (cons-stream (op x (stream-car s))
               (map2-stream op x (stream-cdr s))))

; Maps two streams (s1, s2) in a new stream applying operation op 
(define (map-streams op s1 s2)
  (cons-stream (op (stream-car s1) (stream-car s2))
               (map-streams op (stream-cdr s1) (stream-cdr s2))))

; Multiplies two streams (s1, s2) element-wise producing a new stream
(define (*-streams s1 s2)
  (map-streams * s1 s2))

; Returns the first n elements of a stream
(define (take n stream)
  (if (or (stream-null? stream) (< n 1))
      '()
      (cons (stream-car stream)
            (take (- n 1) (stream-cdr stream)))))

; --------------- ;
; evaluate-series ;
; --------------- ;

; Returns n!
(define (fact n)
  (define (fact-sub n acc)
    (if (= 0 n)
        acc
        (fact-sub (- n 1) (* acc n))))
  (fact-sub n 1))

; Returns 1/n!
(define (inv-fact n)
  (/ 1 (fact n)))

; Returns a stream of the powers of x
(define (powers x)
  (map2-stream expt x (integers-from 0)))

; Returns a stream of integers starting from n
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

; Returns a stream of coefficients {a_n}_{n=0}^{+inf} for S(x)=e^x, hence a_n=1/n!
(define inv-fact-stream
  (map-stream inv-fact (integers-from 0)))

; Evaluates the series S(x), where S is a stream representing S' coefficients
; Returns a stream containing approximations for S(x)
(define (power-series-terms S x)
  (*-streams S (powers x)))

(define (evaluate-series S x)
  (stream-fold + 0 (power-series-terms S x)))
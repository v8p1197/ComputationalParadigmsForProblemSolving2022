#lang scheme
(require racket/stream)

; ------------------------------------- ;
; Basic functions for stream management ;
; ------------------------------------- ;

; Maps a stream into a new stream with the binary operator (x op s)
(define (stream-map2r op x s)
  (stream-cons (op x (stream-first s))
               (stream-map2r op x (stream-rest s))))

; Maps a stream into a new stream with the binary operator (s op x)
(define (stream-map2l op s x)
  (stream-cons (op (stream-first s) x)
               (stream-map2l op (stream-rest s) x)))

; Maps two streams (s1, s2) in a new stream applying the binary operator op element-wise
(define (map-streams op s1 s2)
  (stream-cons (op (stream-first s1) (stream-first s2))
               (map-streams op (stream-rest s1) (stream-rest s2))))

; Adds two streams (s1, s2) element-wise producing a new stream
(define (+-streams s1 s2)
  (map-streams + s1 s2))

; Multiplies two streams (s1, s2) element-wise producing a new stream
(define (*-streams s1 s2)
  (map-streams * s1 s2))

; Divides two streams (s1, s2) element-wise producing a new stream
(define (/-streams s1 s2)
  (map-streams / s1 s2))

; Returns the approximation of a stream within a certain tolerance
(define (within tolerance stream)
  (define first (stream-first stream))
  (define second (stream-first (stream-rest stream)))
  (if (< (abs (- first second)) tolerance)
      second
      (within tolerance (stream-rest stream))))

; --------------- ;
; evaluate-series ;
; --------------- ;

; Returns a stream of integers starting from n
(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

; Returns a stream of the powers of x, i.e. x^n, x^{n+1}, x^{n+2}, ...
(define (powers-from n x)
  (stream-map2r expt x (integers-from n)))

; Returns a stream of powers of x, computed according to an exponent function
(define (powers exp-func x n)
  (stream-cons (expt x (exp-func n))
               (powers exp-func x (+ n 1))))

; Returns the stream of terms of a power series, i.e. a_0 * x^0, a_1 * x^1, ...,
; where S is the steam containins {a_0, a_1, ...} and x is a real number
(define (power-series-terms S power-stream)
  (*-streams S power-stream))

; Returns a stream of evaluations of the power series S(x),
; where S is the stream of the coefficients of the series,
; and exp-func is a function returning the exponent function of x
(define (evaluate-power-series exp-func S x)
  (define (evaluate-series-sub S x acc)
    (define curr-evaluation (+ acc (stream-first S)))
    (stream-cons curr-evaluation
                 (evaluate-series-sub (stream-rest S) x curr-evaluation)))
  (define power-stream (powers exp-func x 0))
  (define series-terms (power-series-terms S power-stream))
  (evaluate-series-sub series-terms x 0))

; n
(define (exp-exp-func n)
  n)

; Returns a stream of evaluations of the power series S(x),
; where S is the stream of the coefficients of the series
(define (evaluate-series S x)
  (evaluate-power-series exp-exp-func S x))

; ---------- ;
; add-series ;
; ---------- ;

; Returns a stream representing the series S(x) = S1(x) + S2(x)
(define (add-series S1 S2 x)(define (multiply-series S1 S2 x)
  (define (multiply-series-sub S1 S2 n)
    (stream-cons (stream-ref (evaluate-series (cauchy-product S1 S2 n) 1) n)
                 (multiply-series-sub S1 S2 (+ n 1))))
  (evaluate-series (multiply-series-sub S1 S2 0) x))
  (evaluate-series (+-streams S1 S2) x))

; --------------- ;
; multiply-series ;
; --------------- ;

; Returns a stream containing the coefficients of the Cauchy product
; between the series {a_0, a_1, ... a_n} and {b_0, b_1, ..., b_n}
(define (cauchy-product a b n)
  (stream-cons (* (stream-first a) (stream-ref b n))
               (cauchy-product (stream-rest a) b (- n 1))))



; Returns a stream represeting the series S(x) = S1(x) * S2(x)
; It uses the theoretical result that S(x) is the power series evaluated at x,
; where the coefficients of S are the cachy products between S1 and S2
(define (multiply-series S1 S2 x)
  (define (multiply-series-sub S1 S2 n)
    (stream-cons (stream-ref (evaluate-series (cauchy-product S1 S2 n) 1) n)
                 (multiply-series-sub S1 S2 (+ n 1))))
  (evaluate-series (multiply-series-sub S1 S2 0) x))

; ---------------- ;
; integrate-series ;
; ---------------- ;

; Returns a stream containing the coeffients of the series
; representing the integral of the series S(x),
; where S is the stream of the coefficients of the series,
; and exp-func is a function returning the exponent function of x
(define (integrate-power-series S exp-func)
  (define (exp-stream n)
    (stream-cons (+ (exp-func n) 1)
                 (exp-stream (+ n 1))))
  (/-streams S (exp-stream 0)))
                          
; Returns a stream containing the coeffients of the series
; representing the integral of the series S(x),
; where S is the stream of the coefficients of the series
(define (integrate-series S)
  (integrate-power-series S exp-exp-func))

; -------------------- ;
; trigonometric-series ;
; -------------------- ;

; 2n
(define (cos-exp-func n)
  (* 2 n))

; 2n + 1
(define (sin-exp-func n)
  (+ (* 2 n) 1))

; Returns the stream of the coefficients of cosine expressed as power series
; a_n = (-1)^n / (2n)!
(define (cos-coeffs-sub n)
  (stream-cons (/ (expt -1 n) (stream-ref facts (* 2 n)))
               (cos-coeffs-sub (+ n 1))))
(define cos-coeffs (cos-coeffs-sub 0))

; Returns the stream of the coefficients of sine expressed as power series
; a_n = (-1)^n / (2n + 1)!
(define (sin-coeffs-sub n)
  (stream-cons (/ (expt -1 n) (stream-ref facts (+ (* 2 n) 1)))
               (sin-coeffs-sub (+ n 1))))
(define sin-coeffs (sin-coeffs-sub 0))

; Returns the stream of the coefficients of cosine expressed
; as the integral of the sine series
(define cos-series
  (stream-cons 1
               (stream-map2r - 0 (integrate-power-series sin-coeffs sin-exp-func))))

; Returns the stream of the coefficients of sine expressed
; as the integral of the cosine series
(define sin-series
  (integrate-power-series cos-coeffs cos-exp-func))

; ---- ;
; MAIN ;
; ---- ;

; Returns a stream of factorials, i.e. 1, 1, 2, 6, 24, ...
(define facts
  (stream-cons 1 (*-streams (integers-from 1) facts)))

; Returns a stream of inverse of factorials, i.e. 1, 1, 1/2, 1/6, 1/24, ...
(define inv-facts
  (stream-map2l expt facts -1))

; e^x
(define exponent 3)

; Define e (the Euler number) as a stream
(define e-stream (evaluate-series inv-facts exponent))

; Approximate e to a certain number of decimal points
(define decimal-tolerance 20)
(define tolerance (expt 10 (- 0 decimal-tolerance)))
(define e (within tolerance e-stream))
(define e-string (real->decimal-string e decimal-tolerance))
(printf "* The appoximation of e^~a up to ~a decimal points is\n~a\n\n"
        exponent decimal-tolerance e-string)

; Get the n-th approximation of e
(define n 10)
(define e-approx (exact->inexact (stream-ref e-stream n)))
(define e-approx-string (real->decimal-string e-approx decimal-tolerance))
(printf "* The ~a-th approximation of e^~a is\n~a\n\n"
        n exponent e-approx-string)

; Approximate 2e with a stream (test add-series)
(define 2*e-stream (add-series inv-facts inv-facts exponent))
(define 2*e (within tolerance 2*e-stream))
(define 2*e-string (real->decimal-string 2*e decimal-tolerance))
(printf "* The appoximation of 2e^~a up to ~a decimal points is\n~a\nThe actual value is\n~a\n\n"
        exponent decimal-tolerance 2*e-string
        (real->decimal-string (within tolerance (stream-map2r * 2 e-stream)) decimal-tolerance))

; Approximate e^2 with a stream (test multiply-series)
(define e2-stream (multiply-series inv-facts inv-facts exponent))
(define e2 (within tolerance e2-stream))
(define e2-string (real->decimal-string e2 decimal-tolerance))
(printf "* The appoximation of e^~a up to ~a decimal points is\n~a\nThe actual value is\n~a\n\n"
        (* 2 exponent) decimal-tolerance e2-string
        (real->decimal-string (within tolerance (*-streams e-stream e-stream)) decimal-tolerance))

; Approximate e^x with its integral
(define exp-series
  (stream-cons 1 (integrate-series exp-series)))
(define exp (within tolerance (evaluate-series exp-series exponent)))
(define exp-string (real->decimal-string exp decimal-tolerance))
(printf "* The integral of e^x (evaluated at x=~a) up to ~a decimal points is\n~a\nThe actual value is\n~a\n\n"
        exponent decimal-tolerance exp-string
        (real->decimal-string (within tolerance (evaluate-series inv-facts exponent)) decimal-tolerance))

; ---------------------------- ;
; Evaluate sin(x)^2 + cos(x)^2 ;
; ---------------------------- ;

; First, evaluate with the known coefficients...
(define x (random))
(define c (within tolerance (evaluate-power-series cos-exp-func cos-coeffs x)))
(define s (within tolerance (evaluate-power-series sin-exp-func sin-coeffs x)))
(define id (+ (expt c 2) (expt s 2)))

(define x-string (real->decimal-string x 2))
(define c-string (real->decimal-string c 2))
(define s-string (real->decimal-string s 2))
(define id-string (real->decimal-string id 2))
(printf "* cos(~a) = ~a, sin(~a) = ~a,\n  cos(~a)^2 + sin(~a)^2 = ~a\n"
        x-string c-string
        x-string s-string
        x-string x-string id-string)

; ...Then, evaluate with the coefficients computed from the integrals...
(define c-int (within tolerance (evaluate-power-series cos-exp-func cos-series x)))
(define s-int (within tolerance (evaluate-power-series sin-exp-func sin-series x)))
(define id-int (+ (expt c-int 2) (expt s-int 2)))

(define c-int-string (real->decimal-string c-int 2))
(define s-int-string (real->decimal-string s-int 2))
(define id-int-string (real->decimal-string id-int 2))
(printf "* After integation, cos(~a) = ~a, sin(~a) = ~a,\n  cos(~a)^2 + sin(~a)^2 = ~a\n"
        x-string c-int-string
        x-string s-int-string
        x-string x-string id-int-string)

; ...GRAN FINALE
;(define id-stream (add-series (multiply-series 
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

; Returns a stream representing the series S(x) = S1(x) + S2(x),
(define (add-series S1 S2)
  (+-streams S1 S2))

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
(define (multiply-series S1 S2)
  (define (one n) 1)
  (define (multiply-series-sub S1 S2 n)
    (stream-cons (stream-ref (evaluate-power-series one (cauchy-product S1 S2 n) 1) n)
                 (multiply-series-sub S1 S2 (+ n 1))))
  (multiply-series-sub S1 S2 0))

; ---------------- ;
; integrate-series ;
; ---------------- ;

; Returns a stream containing the coeffients of the series
; representing the integral of the series S(x),
; where S is the stream of the coefficients of the series,
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

; Returns +1 if n is even, -1 is n is odd
(define (parity n)
  (if (even? n)
      +1
      -1))

; Returns the stream of the coefficients of cosine expressed as Taylor series
(define (cos-coeffs-sub n)
  (define (cos-sign n)
    (if (even? n)
        (parity (/ n 2))
        0))
  (stream-cons (/ (cos-sign n) (stream-ref facts n))
               (cos-coeffs-sub (+ n 1))))
(define cos-coeffs (cos-coeffs-sub 0))

; Returns the stream of the coefficients of sine expressed as Taylor series
(define (sin-coeffs-sub n)
  (define (sin-sign n)
    (if (even? n)
        0
        (- (parity (/ (+ n 1) 2)))))
  (stream-cons (/ (sin-sign n) (stream-ref facts n))
               (sin-coeffs-sub (+ n 1))))
(define sin-coeffs (sin-coeffs-sub 0))

; Returns the stream of the coefficients of cosine expressed
; as the integral of the sine series
(define cos-series
  (stream-cons 1
               (stream-map - (integrate-series sin-coeffs))))

; Returns the stream of the coefficients of sine expressed
; as the integral of the cosine series
(define sin-series
  (stream-cons 0
               (integrate-series cos-coeffs)))

; Returns a stream containing only the elements in stream s at even positions
(define (only-even s)
  (stream-cons (stream-first s)
               (only-even (stream-rest (stream-rest s)))))

; Returns a stream containing only the elements in stream s at odd positions
(define (only-odd s)
  (stream-cons (stream-first (stream-rest s))
               (only-odd (stream-rest (stream-rest s)))))

; ---- ;
; MAIN ;
; ---- ;

; Returns a stream of integers starting from n, i.e. n, n+1, n+2, ...
(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

; Returns a stream of factorials, i.e. 1, 1, 2, 6, 24, ...
(define facts
  (stream-cons 1 (*-streams (integers-from 1) facts)))

; Returns a stream of inverse of factorials, i.e. 1, 1, 1/2, 1/6, 1/24, ...
(define inv-facts
  (stream-map2l expt facts -1))

; e^x
(define exponent 1)

; Define e (the Euler number) as a stream
(define e-stream (evaluate-series inv-facts exponent))

; Approximate e to a certain number of decimal points
(define decimal-tolerance 20)
(define tolerance (expt 10 (- decimal-tolerance)))
(define e (within tolerance e-stream))
(define e-string (real->decimal-string e decimal-tolerance))
(printf "* The appoximation of e^~a up to ~a decimal points is\n  ~a\n\n"
        exponent decimal-tolerance e-string)

; Get the n-th approximation of e
(define n 10)
(define e-approx (exact->inexact (stream-ref e-stream n)))
(define e-approx-string (real->decimal-string e-approx decimal-tolerance))
(printf "* The ~a-th approximation of e^~a is\n  ~a\n\n"
        n exponent e-approx-string)

; Approximate 2e with a stream (tests add-series)
(define 2*e-stream (add-series inv-facts inv-facts))
(define 2*e (within tolerance (evaluate-series 2*e-stream exponent)))
(define 2*e-string (real->decimal-string 2*e decimal-tolerance))
(printf "* The appoximation of 2e^~a up to ~a decimal points is\n  ~a\n  The actual value is\n  ~a\n\n"
        exponent decimal-tolerance 2*e-string
        (real->decimal-string (within tolerance (stream-map2r * 2 e-stream)) decimal-tolerance))

; Approximate e^2 with a stream (tests multiply-series)
(define e2-stream (multiply-series inv-facts inv-facts))
(define e2 (within tolerance (evaluate-series e2-stream exponent)))
(define e2-string (real->decimal-string e2 decimal-tolerance))
(printf "* The appoximation of e^~a up to ~a decimal points is\n  ~a\n  The actual value is\n  ~a\n\n"
        (* 2 exponent) decimal-tolerance e2-string
        (real->decimal-string (within tolerance (*-streams e-stream e-stream)) decimal-tolerance))

; Approximate e^x with its integral (tests integrate-series)
(define exp-series
  (stream-cons 1 (integrate-series exp-series)))
(define exp (within tolerance (evaluate-series exp-series exponent)))
(define exp-string (real->decimal-string exp decimal-tolerance))
(printf "* The integral of e^x (evaluated at x=~a) up to ~a decimal points is\n  ~a\n  The actual value is\n  ~a\n\n"
        exponent decimal-tolerance exp-string
        (real->decimal-string (within tolerance (evaluate-series inv-facts exponent)) decimal-tolerance))

; ---------------------------- ;
; Evaluate cos(x)^2 + sin(x)^2 ;
; ---------------------------- ;

; Generate a random number x in range ] -pi ; +pi [
(define x (+ (- pi) (* (* 2 pi) (random))))
(define precision 4)

; First, evaluate with the known coefficients...
(define c (within tolerance (only-odd (evaluate-series cos-coeffs x))))
(define s (within tolerance (only-even (evaluate-series sin-coeffs x))))
(define c2 (expt c 2))
(define s2 (expt s 2))
(define id (+ c2 s2))

(define x-string (real->decimal-string x precision))
(define c-string (real->decimal-string c precision))
(define s-string (real->decimal-string s precision))
(define c2-string (real->decimal-string c2 precision))
(define s2-string (real->decimal-string s2 precision))
(define id-string (real->decimal-string id precision))
(printf "* Evaluating the trigonometric identity cos(x)^2 + sin(x)^2 for x=~a...\n  cos(~a) = ~a, sin(~a) = ~a,\n  cos(~a)^2 = ~a, sin(~a)^2 = ~a\n  cos(~a)^2 + sin(~a)^2 = ~a\n"
        x-string
        x-string c-string x-string s-string
        x-string c2-string x-string s2-string
        x-string x-string id-string)

; ...Then, evaluate with the coefficients computed from the integrals...
(define c-int (within tolerance (only-even (evaluate-series cos-series x))))
(define s-int (within tolerance (only-odd (evaluate-series sin-series x))))
(define c2-int (expt c-int 2))
(define s2-int (expt s-int 2))
(define id-int (+ c2-int s2-int))

(define c-int-string (real->decimal-string c-int precision))
(define s-int-string (real->decimal-string s-int precision))
(define c2-int-string (real->decimal-string c2-int precision))
(define s2-int-string (real->decimal-string s2-int precision))
(define id-int-string (real->decimal-string id-int precision))
(printf "* After integration...\n  cos(~a) = ~a, sin(~a) = ~a,\n  cos(~a)^2 = ~a, sin(~a)^2 = ~a\n  cos(~a)^2 + sin(~a)^2 = ~a\n"
        x-string c-int-string x-string s-int-string
        x-string c2-int-string x-string s2-int-string
        x-string x-string id-int-string)

; ----------------------------------------------- ;
; GRAN FINALE (tests all the developed functions) ;
; ----------------------------------------------- ;

(define c2-stream (multiply-series cos-series cos-series))
(define s2-stream (multiply-series sin-series sin-series))
(define c2-mul (within tolerance (only-even (evaluate-series c2-stream x))))
(define s2-mul (within tolerance (only-odd (evaluate-series s2-stream x))))
(define c2+s2-stream (add-series c2-stream s2-stream))
(define id-add (within tolerance (evaluate-series c2+s2-stream x)))

(define c2-mul-string (real->decimal-string c2-mul precision))
(define s2-mul-string (real->decimal-string s2-mul precision))
(define id-add-string (real->decimal-string id-add precision))
(printf "* Approximating cos(x)^2 with (cos*cos)(x), sin(x)^2 with (sin*sin)(x), and cos(x)^2 + sin(x)^2 with ((cos*cos)+(sin*sin))(x)...\n  cos(~a) = ~a, sin(~a) = ~a,\n  cos(~a)^2 = ~a, sin(~a)^2 = ~a\n  cos(~a)^2 + sin(~a)^2 = ~a\n\n"
        x-string c-int-string x-string s-int-string
        x-string c2-int-string x-string s2-int-string
        x-string x-string id-int-string)

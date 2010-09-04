(define (square x) (* x x))
(define tolerance 0.00001)

;f(x) = x
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixe-point (newton-transform g) guess))

;1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* b x x) (* c x))))

;1.41
(define (inc x)
  (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

;(((double (double double)) inc) 5) = 21

;1.42 
(define (compose f g)
  (lambda (x) (f (g x))))

;((compose square inc) 6) = 49

;1.43
(define (repeated f times)
  (if (= times 1)
      f
      (compose (repeated f (- times 1)) f)))

;1.43 try iterative
(define (repeated-i f times)
  (define (repeated-step accumulator times)
    (if (= times 1)
        accumulator
        (repeated-step (compose accumulator f) (- times 1))))
  (repeated-step (lambda (x) x) times))

;1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
;(define (scos-5 x) (((repeated smooth 5) cos) x))

;1.45
(define (dec x)
  (- x 1))
(define (average x y)
  (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))
;examples for average-dampe usage
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (pow value power)
  (define (pow-step acc power)
    (if (= power 1)
        acc
        (pow-step (* acc value) (dec power))))
  (pow-step value power))

(define (quad-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (pow y 3)))) 1.0))
      
(define (pent-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (pow y 4)))) 1.0))

(define (n-root n x)
  (fixed-point ((repeated average-damp n) (lambda (y) (/ x (pow y (dec n))))) 1.0))

;1.46
;functions to rewrite
(define (sqrt x)
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)  
    (< (abs (- (square guess) x)) 0.001))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve-guess guess))))
  iter)

(define (sqrt2 x)
  ((iterative-improve 
    (lambda (guess) (< (abs (- (square guess) x)) 0.001)) 
    (lambda (guess) (average guess (/ x guess)))
    ) 1.0))

(define (fixed-point2 f first-guess)
  ((iterative-improve
    (lambda (guess) (< (abs (- guess (f guess))) 0.00001))
    (lambda (guess) (f guess))) first-guess))
;from 1.2.5
(define (greatest-common-divisor a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat numerator denominator)
  (let ((g (greatest-common-divisor numerator denominator)))
    (cons (/ numerator g) (/ denominator g))))
(define (numer x)
  (car x))
(define (denom x)
  (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(print-rat (add-rat one-third one-third))

;2.1
(define (sign x)
  (/ x (abs x)))
(define (make-rat2 numerator denominator)
  (let (
        (g (greatest-common-divisor numerator denominator))
        (sign-num (sign numerator))
        (sign-denom (sign denominator)))
    (cons (* (/ (abs numerator) g) sign-num sign-denom) (/ (abs denominator) g))))
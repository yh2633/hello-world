# hello-world

Want to learn programming, and I am planning to start with python


2 years later...

SICP Chapter 1

-------------1.3---------------

(define (square x) (* x x))

(define (sum-squared-two a b)
        (+ (square a) (square b)))

(define (larger x y)
        (if (> x y)
            x 
            y ))

(define (smaller x y) 
        (if (< x y) 
            x 
            y))
          
(define (second x y z)
        (larger (smaller x y)z))    

(define (inpu x y z) 
        (sum-squared-two (larger x y) 
                         (second x y z)))
                         
---------------开平方---------------

(define (squrit_iter z x) 
        (/ (+ z (/ x z)) 2))

(define (square x) 
        (* x x))

(define (abs x)
        (if (< x 0)
            (- x)
            x))

(define (good_enough x y)
        (if (< (abs (- (square x) y)) 0.00001 )
            x
            (good_enough (squrit_iter x y) y)))
          
(define (squrit x)
        (good_enough 1 x))

(squrit 55567685767)

----VERSION 2----

(define (iter guess x)
        (if (good guess x)
            guess
            (iter (improved guess x) x)))

(define (good guess x)
        (< (abs (- (square guess) x)) 0.0001))
      
(define (improved guess x)
        (average guess (/ x guess)))

(define (squrit x)
        (iter 1 x))

(squrit 1243124)





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

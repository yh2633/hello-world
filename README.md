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

-------------------------1.7------------------------
(define (average x y) 
        (/ (+ x y) 2))

(define (square x) 
        (* x x))

(define (abs x)
        (if (< x 0)
            (- x)
            x))

(define (iter guess x)
        (if (good guess (improved guess x))
        guess
        (iter (improved guess x) x)))

(define (good old_guess new_guess)
        (> 0.0001 
              (/ (abs (- old_guess new_guess)) old_guess)))
      
(define (improved guess x)
        (average guess (/ x guess)))

(define (sqit x) (iter 1 x))

--------------------1.8 开三次方---------------------
(define (average x y) 
        (/ (+ x y) 2))

(define (square x) 
        (* x x))

(define (abs x)
        (if (< x 0)
            (- x)
            x))

(define (iter guess x)
        (if (good guess (improved guess x))
        guess
        (iter (improved guess x) x)))

(define (good old_guess new_guess)
        (> 0.0001 
              (/ (abs (- old_guess new_guess)) old_guess)))
      
(define (improved guess x)
        (/(+ (/ x (square guess)) (* 2 guess)) 3))

(define (trit x) (iter 1 x))

(trit 125)

--------------fib(n) [a b法]--------------

(define (fib n)
    (iter 0 1 n))
(define (iter a b count)
    (if (= count 0)
        a 
        (iter b (+ a b) (- count 1))))

递归与迭代：递归每部引用自己并需记录过程；迭代更新常数

---------------1.11 recursion---------------
(define (f n)
         (if (< n 3)
             n 
             (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

----------------1.11 iteration----------------
(define (reit a b c count)
        (if (< count 3)
            (+ a (* 2 b) ( * 3 c))
            (+ (reit (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
               
(define (f n)
        (if (< n 3)
            n
            (reit 2 1 0 n)))
          
(f 5)

-------------------revise----------
(define (reit a b c count)
        (if (= count 3)
            (+ a (* 2 b) ( * 3 c))
            (+ (reit (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
               
(define (f n)
        (if (< n 3)
            n
            (reit 2 1 0 n)))
          
(f 3)

------------------1.16 exponential iteration fast approach----------
(define (square x)
  (* x x))

(define (itera evenp n odda)
  (if (= n 0)
      odda
      (if (= (remainder n 2) 0)
          (itera (square evenp) (/ n 2) odda)
          (itera evenp (- n 1) (* evenp odda)))))
        
(define (expon b n)
  (itera b n 1 ))

**********注意迭代的方法以及后边的那个常量*****************








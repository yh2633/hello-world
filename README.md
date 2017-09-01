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

-------------1.17------------ multiplication recuration----
(define (even? x)
        (if (= (remainder x 2) 0)))
      
(define (multrecur a b)
  (if (= b 0)
      0
      (if (even? b)
          (multrecur (+ a a) (/ b 2))
          (+ a (multrecur a (- b 1) )))))

需要多思考

--------------1.18---------- multiplication iteration---
(define (even? x)
        (if (= (remainder x 2) 0)))
      
(define (muliter evenp n oddr)
  (if (= n 0)
      oddr
      (if (even? n)
        (muliter (+ evenp evenp) (/ n 2) oddr)
        (muliter evenp (- n 1) (+ oddr evenp)))))

(define (multipled a b)
  (muliter a b 0))
  
-
---------------1.19-----fib快速幂----------可以和1.16结合看（平方可以施用于数，也可以施用于process）--------

(define (even? x)
        (= (remainder x 2) 0))

(define (square x) 
        (* x x))

(define (fibf a b p q n)
  (cond ((= n 0) b)
        ((even? n) (fibf a b (+ (square p) (square q)) (+ (square q) (* 2 p q)) (/ n 2)))
        (else (fibf (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- n 1)))))

(define (fastf n)
  (fibf 1 0 0 1 n))

------------fermat prime test------------------已经看出来事情开始难起来了-----------------注意括号！！！！！！！--第二个procedure真的很吊，包含了最小公约数和快速幂的算法

(define (even? x)
  (= (remainder x 2) 0))

(define (expon base top n)
  (cond ((= top 0) 1)
        ((even? top)
                (remainder (square (expon base (/ top 2) n)) n))
        (else
                (remainder (* base (expon base (- top 1) n)) n))))

(define (fermat n)
  (define (try-it a)
    (= (expon a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (test n count)
  (cond ( (= count 0) true)
        ((fermat n) (test n (- count 1)))
        (else false)))


        (else false)))
--------------------------1.21----------smallestd
(define (smallestd n)
(define (devisor small)
  (cond ((> (square small) n) n)
        ((= (remainder n small) 0) small)
        (else (devisor (+ small 1)))))
  (devisor 2))


------------1.22 .1-------------忙了一早上，勉强写出一个素数报名器，发现了一些昨天没搞懂的东西，第三部仍然没有很好的理解----------

(define (range alpha n t)
  (cond ((< n alpha) "finished")
        ((not (= (remainder n 2) 0))
            (test n t)
            (newline)
            (range alpha (- n 2) t))
        (else (range alpha (- n 1) t))))
        
(define (even? x)
  (= (remainder x 2) 0))

(define (expon base top n)
  (cond ((= top 0) 1)
        ((even? top)
                (remainder (square (expon base (/ top 2) n)) n))
        (else
                (remainder (* base (expon base (- top 1) n)) n))))

(define (fermat n)
  (define (try-it a)
    (= (expon a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (test n count)
  (cond ( (= count 0) (display n))
        ((fermat n) (test n (- count 1)))
        (else (display "aint a prime"))))




































;;;习题1.3
(define (bigger a b) (if (> a b)
                         a
                         b))

(define (smaller a b) (if (< a b)
                          a
                          b))


(define (maxsum x y z) (+ (bigger x y)
                          (bigger (smaller x y) z)))

(maxsum 1 2 3)

;;;习题1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;;;实例1.1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? old-guess new-guess)
  (> 0.01
     (/ (abs (- new-guess old-guess))
        old-guess)))

(define (suqare x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;;练习1.8
(define (cube-iter guess x)
  (if (good-enough? guess (improve3 guess x))
      guess
      (cube-iter (improve3 guess x)
                 x)))

(define (improve3 guess x)
  (/ (+ (/ x (suqare guess)) (* 2 guess)) 3))

;;;练习1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;;;实例换零钱
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
;;;练习 1.11

(define (f11 n)
  (cond ((< n 3) n)
        (else (+
               (f11 (- n 1))
               (* 2 (f11 (- n 2)))
               (* 3 (f11 (- n 3)))))))

(define (f11i n)
  (f11-iter 2 1 0 0 n))
(define (f11-iter a b c i n)
  (if (= i n)
      c
      (f11-iter (+ a (* 2 b) (* 3 c))
                a
                b
                (+ i 1)
                n)))
;;;练习 1.12

(define (pascal n m)
  (if (> n 2)
   (cond ((= m 1) 1)
         ((= n m) 1)
         (else (+ (pascal (- n 1) (- m 1))
                  (pascal (- n 1) m) )))
   1))

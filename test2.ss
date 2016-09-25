(define (bigger a b) (if (> a b)
                         a
                         b))

(define (smaller a b) (if (< a b)
                          a
                          b))


(define (maxsum x y z) (+ (bigger x y)
                          (bigger (smaller x y) z)))

(maxsum 1 2 3)


(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
(+ 1 3)

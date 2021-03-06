(define rember

  (lambda (s l)

    (cond

      ((null? l) (quote ()))

      (( equal? ( car l) s) (cdr l))

      (else ( cons ( car l)

                   (rember s ( cdr l)))))))



(define rember-f1

  (lambda (test? s l)

    (cond

      ((null? l) '())

      (((test? (car l) s) (cdr l)))

      (else (cons (car l)

                  (rember-f1 test? s (cdr l)))))))



(define rember-f

  (lambda (test?)

    (lambda (s l)

      (cond

        ((null? l) '())

        ((test? (car l) s) (cdr l))

        (else (cons (car l)

                    ((rember-f test?) s (cdr l))))))))



(define insertL-f

  (lambda (test?)

    (lambda (new old l)

      (cond

        ((null? l) '())

        ((test? (car l) old)

         (cons new (cons old (cdr l))))

        (else (cons (car l)

                    ((insertL-f test?) new old

                    (cdr l))))))))



(define insertR-f

  (lambda (test?)

    (lambda (new old l)

      (cond

        ((null? l) '())

        ((test? (car l) old)

         (cons old (cons new (cdr l))))

         (else (cons (car l)

                    ((insertR-f test?) new old

                    (cdr l))))))))



(define seqL

  (lambda (new old l)

    (cons new (cons old l))))



(define seqR

  (lambda (new old l)

    (cons old (cons new l))))



(define insert-g1 

  (lambda (test?)

    (lambda (seq)

      (lambda (new old l)

        (cond

          ((null? l) '())

          ((test? (car l) old)

           (seq new old (cdr l)))

          (else (cons (car l)

                      (((insert-g1 test?) seq) new old 

                                                (cdr l)))))))))

(define insert-g (insert-g1 eq?))



(define insertL 

      (lambda (new old l)

        (cond

          ((null? l) '())

          ((eq? (car l) old)

           (cons new (cons old (cdr l))))

          (else (cons (car l)

                      (insertL new old (cdr l)))))))

(define subst

  (lambda (new old l)

    (cond

      (( null? l) (quote ()))

      ( ( eq? ( car l) old)

        ( cons new ( cdr l)))

      (else ( cons ( car l)

             (subst new old ( cdr l)))))))



(define multirember-f

  (lambda ( test?)

    (lambda ( a lat)

      (cond

        ( ( null? lat) (quote ()))

        ( ( test? a ( car lat))

          (( multirember-f test?) a

                                   ( cdr lat)))

        (else ( cons ( car lat)

                     (( multirember-f test?) a

                                             ( cdr lat))))))))

(define eq?-tuna

  (lambda (a)

    (eq? a 'tuna)))





(define multirember&co

  (lambda ( a lat col)
    (cond

      ((null? lat)

       (col '() '()))

      ((eq? (car lat) a)

       (multirember&co a

                       ( cdr lat)

                       (lambda (newlat seen)

                         ( col newlat

                               ( cons ( car lat) seen)))))

      (else

       (multirember&co a

                       (cdr lat)

                       (lambda (newlat seen)

                         (col (cons ( car lat) newlat)

                              seen)))))))

(define multiremberT

  (lambda ( test? lat)

    (cond

      ((null? lat) (quote ()))

      ((test? ( car lat))

       (multiremberT test? ( cdr lat)))

      (else ( cons ( car lat)

                   (multiremberT test?

                                 ( cdr lat)))))))



(define a-friend

  (lambda (a y)

    (null? y)))



(define new-friend

  (lambda (newlat seen)

    (a-friend newlat

         cons '(tuna) seen)))

(define latest-friend

  (lambda ( newlat seen)

    ( a-friend ( cons (quote and) newlat)

               seen)))



(multirember&co 'tuna '(and  a  c tuna b tuna) (lambda (x y) x))

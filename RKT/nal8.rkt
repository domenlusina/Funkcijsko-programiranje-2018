#lang racket


(define ones(cons 1 (lambda () ones)))

(define naturals (
                  letrec(
                         (nat (lambda (x) (cons x (lambda () (nat (+ x 1))))))
                         )
                   (nat 1)
                  ))

(define fibs (
                  letrec(
                         (fib (lambda (x y) (cons x (lambda () (fib y (+ x y))))))
                         )
                   (fib 1 1)
                  ))
(define (first n flow) (
                       if (= n 0)
                          null
                          (cons (car flow) (first (- n 1) ((cdr flow))))
                       ))

(define (squares flow) (
                        cons (* (car flow) (car flow))
                        (lambda () (squares ((cdr flow))))
                        ))

(define-syntax sml
  (syntax-rules
    (nil null :: hd tl)
    ((sml nil) (list))
    ((sml null xs) (null? xs))
    ((sml head :: tail) (cons head tail))
    ((sml hd xs) (car xs) )
    ((sml tl xs) (cdr xs) )
   )
)


(define (my-delay thunk) 
  (mcons 0 (mcons thunk thunk))) 

(define (my-force prom)
  (if (> (mcar prom) 0)
      [begin 
        (set-mcar! prom (modulo (+ (mcar prom) 1) 5))
        (mcdr (mcdr prom)) 
      ]
      [begin 
        (set-mcar! prom 1)
        (set-mcdr! prom (mcons (mcar (mcdr prom)) ((mcar (mcdr prom))))) 
        (mcdr (mcdr prom))                                              
      ]                             
  )
)

(define (partitions k n)
  (cond [(and (= k 0) (= n 0)) 1]
        [(or (< k 1) (< n 1)) 0]
        [else (+ (partitions (- k 1) (- n 1)) (partitions k (- n k)))]
  )
)




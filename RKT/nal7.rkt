#lang racket

;(define x "Hello world")
;x

(define (power x n) (if (= n 0)
                        1
                        (* x (power x (- n 1)))
                        ))

(define (gcd a b) (if (= b 0)
                      a
                      (gcd b (modulo a b))
                      ))

(define (fib x) (if (or (= x 1) (= x 2))
                    1
                    (+ (fib (- x 2)) (fib (- x 1)))
                    ))

(define (reverse xs) (if (null? xs)
                         xs
                         (append (reverse (cdr xs)) (list (car xs)))
                         ))

(define (remove x xs) (if (null? xs)
                         xs
                         (if (= x (car xs))
                             (remove x (cdr xs))
                             (cons (car xs) (remove x (cdr xs)))
                             )
                         ))

(define (map f xs)
  (if (null? xs)
      xs
      (cons (f(car xs)) (map f (cdr xs)))
   ))


(define (filter f xs)
  (if (null? xs)
      null
      (if (f (car xs))
          (cons (car xs) (filter f (cdr xs)))
          (filter f (cdr xs)))))

(define (zip sez1 sez2) (if (or (null? sez1) (null? sez2) )
                            null
                            (cons (cons (car sez1)(car sez2)) (zip (cdr sez1)(cdr sez2)))
                            ))

(define (range beg end step) (if (< end beg)
                                 null
                                 (cons beg (range (+ beg step) end step))
                              ))

(define (everynth n sez)(
                         letrec([f (lambda (x xs) (if (null? xs)
                                     xs
                                     (if (= x 1)
                                         (cons (car xs) (f n (cdr xs)))
                                         (f (- x 1) (cdr xs))
                                         )
                                     ))]
                                 )
                          (f n sez)
                         ))
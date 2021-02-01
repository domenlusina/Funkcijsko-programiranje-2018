#lang racket

(define-syntax ifte
  (syntax-rules (then else)
    ((ifte e1 then e2 else e3) (if-then-else e1 e2 e3))
    )
  )

(define-syntax lt
  (syntax-rules ()
    ((lt e1 e2) (gt e2 e1))
    )
  )

(struct int (st) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)

(struct add (e1 e2) #:transparent)
(struct gt (e1 e2) #:transparent)
(struct both (e1 e2) #:transparent)
(struct ! (e) #:transparent)
(struct is-int (e) #:transparent)
(struct if-then-else (condition e1 e2) #:transparent)


(define (mi exp)
  (cond
    [(int? exp) (if [integer? (int-st exp)] exp false)]
    [(false? exp) exp]
    [(true? exp) exp]
    [(add? exp)
     (let ([st1 (mi (add-e1 exp))]
           [st2 (mi (add-e2 exp))]
           )
     (int (+ (int-st st1) (int-st st2))))
     ]
    [(gt? exp)
     (let ([st1 (mi (gt-e1 exp))]
           [st2 (mi (gt-e2 exp))]
           )
     (if [> (int-st st1) (int-st st2)] [mi(true)] [mi(false)]))
     ]
    [(both? exp)
     (let ([st1 (mi (both-e1 exp))]
           [st2 (mi (both-e2 exp))]
           )
     (if (and (true? st1) (true? st2))
         (mi (true))
         (mi (false))
         ))
     ]
    [(!? exp)
        (let ([st1 (mi (!-e exp))])
           (cond [(true? st1) (mi (false))]
                 [(false? st1) (mi (true))])
           )
        ]
    [(is-int? exp)
      (if (int? (mi (is-int-e exp))) (mi (true)) (mi (false)))
        ]
    [(if-then-else? exp)
     (let ([condition (mi (if-then-else-condition exp))])
       (cond [(true? condition) (mi (if-then-else-e1 exp))]
             [(false? condition) (mi (if-then-else-e2 exp))])

       )

     ]
    )
  )
  
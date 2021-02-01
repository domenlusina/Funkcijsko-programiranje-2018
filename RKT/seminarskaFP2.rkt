#lang racket
(require racket/trace)
(require racket/list)


;Podatkovni tipi
(struct int (st) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct complex (a b) #:transparent)
(struct :: (e1 e2) #:transparent)
(struct empty () #:transparent)

;vejitev
(struct  if-then-else (condition e1 e2)#:transparent)
;preverjanje tipov
(struct  is-int (e)#:transparent)
(struct  is-bool (e)#:transparent)
(struct  is-complex (e)#:transparent)
(struct  is-list (e)#:transparent)
; aritmetične operacije
(struct  add (e1 e2)#:transparent)
(struct  mul (e1 e2)#:transparent)
(struct  gt (e1 e2)#:transparent)
; logične operacije
(struct  both (e1 e2)#:transparent)
(struct  any (e1 e2)#:transparent)
(struct  ! (e)#:transparent)
;operacije nad seznami
(struct  hd (x)#:transparent)
(struct  tl (x)#:transparent)
(struct  is-empty (x)#:transparent)
(struct  @ (a b)#:transparent)
;operacije nad kompleksimi stevili
(struct  real (e)#:transparent)
(struct  imaginary (e)#:transparent)


;spremenljivke
(struct var (s e1 e2) #:transparent)
(struct valof (s) #:transparent)



;funkcije
(struct call (e args) #:transparent)
(struct proc (name body) #:transparent)
(struct envelope (env f)#:transparent)
(struct fun (name fargs body) #:transparent)

(define (mi izraz okolje)
  (cond ;PODATKOVNI TIPI
        [(int? izraz) (if [integer? (int-st izraz)] izraz (error "Nisi podal celo stevilo."))]
        [(true? izraz) izraz]
        [(false? izraz) izraz]
        [(complex? izraz)
         ( let ([st1 (mi (complex-a izraz) okolje)]
                [st2 (mi (complex-b izraz) okolje)]) 
            (if [and (int? st1) (int? st2)]
                (complex st1 st2)
                (error "Podana argumenta morata biti celi stevili.")))
         ]
        [(empty? izraz) izraz]
        [(::? izraz)
         (let ([glava (mi (::-e1 izraz) okolje)]
               [rep (mi (::-e2 izraz) okolje)])
                   (if (or (::? rep) (empty? rep))
                    (:: glava rep)
                    (error "Nisi podal seznama kot drugi argument.")) )]
        ;NADZOR TOKA
        [(if-then-else? izraz)
         (let ([condition (mi (if-then-else-condition izraz) okolje)])
           (cond [(true? condition) (mi (if-then-else-e1 izraz) okolje)]
                 [(false? condition) (mi (if-then-else-e2 izraz) okolje)])
           )
         ]
        [(is-int? izraz)
         (if (int? (mi (is-int-e izraz) okolje))
             (mi (true) okolje)
             (mi (false) okolje))
        ]
        [(is-bool? izraz) 
             (if (or (false? (mi (is-bool-e izraz) okolje)) (true? (mi (is-bool-e izraz) okolje)))
                 (mi (true) okolje)
                 (mi (false) okolje))          
        ]
        [(is-complex? izraz)
         (if (complex? (mi (is-complex-e izraz) okolje))
             (mi (true) okolje)
             (mi (false) okolje))
        ]
        [(is-list? izraz)
         (if [or (::? (mi (is-list-e izraz) okolje)) (empty? (mi (is-list-e izraz) okolje))]
             (mi (true) okolje)
             (mi (false) okolje))
        ]
        ;ARITMETICNE OPERACIJE
        [(gt? izraz)
         (let ([st1 (mi (gt-e1 izraz) okolje)]
               [st2 (mi (gt-e2 izraz) okolje )]
               )
           (if [> (int-st st1) (int-st st2)] [mi(true) okolje] [mi(false) okolje]))
         ]

        ;(mi (add (int 2) (int 3)) null)
        ;(mi (add (complex (int 2) (int 3)) (complex (int 1) (int 4))) null)
        [(add? izraz)
         (let ([st1 (mi (add-e1 izraz) okolje)]
               [st2 (mi (add-e2 izraz) okolje)]
               )
           (cond  
                  [(and (complex? st1) (complex? st2)) (complex (int (+ (int-st(complex-a st1)) (int-st(complex-a st2)))) (int (+ (int-st(complex-b st1)) (int-st(complex-b st2)))))]
                  [(and (int st1) (int st2))(int (+ (int-st st1) (int-st st2)))]
                  )
           )
         ]
        ;(mi (mul (int 2) (int 3)) null)
        [(mul? izraz)
         (let ([st1 (mi (mul-e1 izraz) okolje)]
               [st2 (mi (mul-e2 izraz) okolje)]
               )
           (cond [ (and (complex? st1) (complex? st2))
                   (complex (int (- (* (int-st(complex-a st1)) (int-st(complex-a st2))) ; a*a
                                    (* (int-st(complex-b st1)) (int-st(complex-b st2)))))   ; -b*b
                            (int (+ (* (int-st(complex-a st1)) (int-st(complex-b st2))) ; a1*b2
                                    (* (int-st(complex-b st1)) (int-st(complex-a st2))))) ; b1 * a2
                            )]
                 [(and (int? st1) (int? st2)) (int (* (int-st st1) (int-st st2)))]
                 ))
         ]
        ;LOGICNE OPERACIJE
        [(both? izraz)
         (let ([st1 (mi (both-e1 izraz) okolje)]
               [st2 (mi (both-e2 izraz) okolje)]
               )
           (if(and (true? st1) (true? st2))
              (mi (true) okolje)
              (mi (false) okolje))
           )
         ]
        [(any? izraz)
         (let ([st1 (mi (any-e1 izraz) okolje)]
               [st2 (mi (any-e2 izraz) okolje)]
               )
           (if (or (true? st1) (true? st2))
               (mi (true) okolje)
               (mi (false) okolje)))
         ]
        [(!? izraz)
         (let ([st1 (mi (!-e izraz) okolje)])
           (cond [(true? st1) (mi (false) okolje)]
                 [(false? st1) (mi (true) okolje)])
           )
         ]
        ; OPERACIJE NAD SEZNAMI
        [(hd? izraz)
         (let ([xs (mi (hd-x izraz) okolje)])
           (::-e1 xs)
                    )]
        [(tl? izraz)
         (let ([xs (mi (tl-x izraz) okolje)])
           (::-e2 xs)
                    )]
        [(is-empty? izraz)
         (let ([xs (mi (is-empty-x izraz) okolje)])
           (if (empty? xs)
               (mi (true) okolje)
               (mi (false) okolje))
                    )]
        ; @ seznamov
        [(@? izraz)
          (let ([xs1 (mi (@-a izraz) okolje)]
                [xs2 (mi (@-b izraz) okolje)])
           (cond [(and (empty? xs1) (empty? xs2)) (empty)];oba sta prazna
                 [(and (empty? xs1) (::? xs2)) xs2] ; levi prazen
                 [(and (::? xs1) (empty? xs2)) xs1] ; desni prazen
                 [(and (::? xs1) (::? xs2)) [:: (::-e1 xs1)(mi (@ (::-e2 xs1) xs2) okolje)]] ; rekurzija
           ))]

        
        ; OPERACIJE NAD KOMPLEKSNIMI STEVILI
        [(real? izraz)
         (let ([st1 (mi (real-e izraz) okolje)]
               )
           (mi (complex-a st1) okolje))
         ]
        [(imaginary? izraz)
         (let ([st1 (mi (imaginary-e izraz) okolje)]
               )
           (mi (complex-b st1) okolje))
         ]

        ;SPREMENLJIVKE
        [(var? izraz) 
         (mi (mi(var-e2 izraz) (list* (cons (var-s izraz) (mi (var-e1 izraz)okolje) ) okolje) ) okolje)]
        [(valof? izraz)
               [define (lastval sez) (if (equal? (caar sez) (valof-s izraz))
                                                 (cdar sez)
                                                 (lastval (cdr sez))
                                         )] (lastval okolje)]

        ;FUNKCIJE
        [(fun? izraz) (envelope (remove-duplicates okolje #:key car) izraz)]
        [(proc? izraz) izraz]
        [(envelope? izraz) izraz]
        [(call? izraz)                   
           (let ([evaluiranaF (mi (call-e izraz) okolje)])         
             (cond
               [(envelope? evaluiranaF)
               [define (zdruziSeznamF sez1 sez2) (if (null? sez1) null (list* (cons (car sez1) (mi (car sez2) okolje)) (zdruziSeznamF (cdr sez1) (cdr sez2))))]
               [define novoOkolje (append [zdruziSeznamF (fun-fargs (envelope-f evaluiranaF)) (call-args izraz)]
                                          [list* (cons (fun-name (envelope-f evaluiranaF)) (mi (call-e izraz) okolje)) (envelope-env (mi (call-e izraz) okolje))] )] 
                (mi (fun-body (envelope-f evaluiranaF)) novoOkolje)
                 ]
               [(proc? evaluiranaF)
                  (mi (proc-body evaluiranaF) [list* (cons (proc-name evaluiranaF) (mi (call-e izraz) okolje)) okolje])
                  ]
               )
             )]
        
        
   )
  )

;MAKRO
(define-syntax to-complex
  (syntax-rules ()
    ((to-complex e1) (complex e1 (int 0)))
    )
  )
(define-syntax conj
  (syntax-rules ()
    ((conj e1) (var "e1_" e1 (complex (real (valof "e1_")) (mul (int -1) (imaginary (valof "e1_"))))))
    )
  )
(define-syntax ~
  (syntax-rules ()
    ((~ e1) (mul (int -1) e1))
    )
  )

(define-syntax lt
  (syntax-rules ()
    ((lt e1 e2) (gt e2 e1))
    )
  )
(define-syntax same
  (syntax-rules ()
    ((same e1 e2)
  (var "e1_" e1 (var "e2_" e2 (both (! (gt (valof "e1_") (valof "e2_"))) (! (gt (valof "e2_") (valof "e1_"))))))
    )
  )
  )


#|
(print "PODATKOVNI TIPI")
(newline)
(equal?  (int 5) (mi (int 5) null))
(equal?  (true) (mi (true) null))
(equal?  (false) (mi (false) null))
(equal?  (complex (int 5) (int 4)) (mi (complex (int 5) (int 4)) null))
(equal?  (empty) (mi (empty) null))
(equal?  (:: (true)  (empty)) (mi (:: (true)  (empty)) null))
(equal?  (:: (int 5) (empty)) (mi (:: (int 5) (empty)) null))
(equal?  (:: (complex (int 3) (int 4)) (empty)) (mi (:: (complex (int 3) (int 4)) (empty)) null))
(newline)

(print "TESTI NADZOR TOKA")
(newline)
(equal? (int 5) (mi (if-then-else (true) (int 5) (int 3)) null))
(equal? (int 3) (mi (if-then-else (false) (int 5) (int 3)) null))
(equal? (int 3) (mi (if-then-else (false) (int 5) (add (int 1) (int 2))) null))

(equal? (true) (mi (is-int (int 4)) null))
(equal? (false) (mi (is-int (true)) null))
(equal? (false) (mi (is-int (complex (int 3)(int 4))) null))

(equal? (true) (mi (is-bool (true)) null))
(equal? (false) (mi (is-bool (int 3)) null))
(equal? (false) (mi (is-bool (complex (int 3)(int 4))) null))


(equal? (false) (mi (is-complex (true)) null))
(equal? (false) (mi (is-complex (int 3)) null))
(equal? (true) (mi (is-complex (complex (int 3)(int 4))) null))

(equal? (true) (mi (is-list (empty)) null))
(equal? (true) (mi (is-list (:: (int 4) (empty))) null))
(equal? (false) (mi (is-list (int 1)) null))

(newline)
(print "TESTI ARITMETIČNE OPERACIJE")
(newline)

(equal? (false) (mi (gt (int 1) (add (int 2) (int 1))) null))
(equal? (false) (mi (gt (int 4) (int 4)) null))
(equal? (true) (mi (gt (int 6) (int 4)) null))

(equal? (int 3) (mi (add (int -1) (int 4)) null))
(equal? (int 4) (mi (add (int 0) (add (int 2) (int 2))) null))
(equal? (int 10) (mi (add (int 6) (int 4)) null))

(equal? (complex (int 5) (int 6)) (mi (add (complex (add (int 2) (int 1)) (int 3)) (complex (int 2) (int 3))) null))
(equal? (complex (int 3) (int 3)) (mi (add (complex (int 3) (int 0)) (complex (int 0) (int 3))) null))
(equal? (complex (int 2) (int 2)) (mi (add (complex (int 3) (int -1)) (complex (int -1) (int 3))) null))

(equal? (int -4) (mi (mul (int -1) (int 4)) null))
(equal? (int 0) (mi (mul (int 0) (int 4)) null))
(equal? (int 24) (mi (mul (int 6) (int 4)) null))

(equal? (complex (int -3) (int 15)) (mi (mul (complex (add (int 2) (int 1)) (int 3)) (complex (int 2) (int 3))) null))
(equal? (complex (int 0) (int 9)) (mi (mul (complex (int 3) (int 0)) (complex (int 0) (int 3))) null))
(equal? (complex (int 0) (int 10)) (mi (mul (complex (int 3) (int -1)) (complex (int -1) (int 3))) null))

(newline)
(print "TESTI LOGIČNIH OPERACIJ")
(newline)

(equal? (true) (mi (both (true) (true)) null))
(equal? (false) (mi (both (true) (false)) null))
(equal? (false) (mi (both (false) (true)) null))
(equal? (false) (mi (both (false) (false)) null))
(equal? (true) (mi (both (true) (gt (int 5) (int 3))) null))

(equal? (true) (mi (any (true) (true)) null))
(equal? (true) (mi (any (true) (false)) null))
(equal? (true) (mi (any (false) (true)) null))
(equal? (false) (mi (any (false) (false)) null))
(equal? (true) (mi (any (true) (gt (int 5) (int 3))) null))

(equal? (true) (mi (! (false)) null))
(equal? (false) (mi (! (true)) null))
(equal? (false) (mi (! (gt (int 5) (int 3))) null))


(newline)
(print "TESTI OPERACIJ NAD SEZNAMI")
(newline)

(equal? (true) (mi (hd (:: (true )(empty))) null))
(equal? (int 5) (mi (hd (:: (int 5 )(empty))) null))
(equal? (int 5) (mi (hd (:: (add (int  3) (int 2) )(empty))) null))

(equal? (empty) (mi (tl (:: (true )(empty))) null))
(equal? (:: (true ) (empty)) (mi (tl (:: (true )(:: (true ) (empty)))) null))
(equal? (:: (int 5 ) (empty)) (mi (tl (:: (int 3 )(:: (add (int 4)(int 1)) (empty)))) null))

(equal? (false) (mi (is-empty (:: (int 5 )(empty))) null))
(equal? (true) (mi (is-empty (empty)) null))

(equal? (empty) (mi (@ (empty) (empty)) null))
(equal? (:: (true )(empty)) (mi (@ (:: (true )(empty)) (empty)) null))
(equal? (:: (true )(empty)) (mi (@ (empty) (:: (true )(empty))) null))
(equal? (:: (int 1 )(:: (int 2) (empty)) ) (mi (@ (:: (int 1)(empty)) (:: (int 2)(empty))) null))

(newline)
(print "TESTI OPERACIJ NAD KOMPLEKSNIMI STEVILI")
(newline)
(equal? (int 5) (mi (real (complex (int 5) (int 3))) null))
(equal? (int 5) (mi (real (complex (add (int 3) (int 2)) (int 3))) null))

(equal? (int 3) (mi (imaginary (complex (int 5) (int 3))) null))
(equal? (int 5) (mi (imaginary (complex (int 3) (add (int 3) (int 2)) )) null))


(newline)
(print "TESTI NAD SPREMENLJIVKAMI")
(newline)

(equal? (int 3) (mi (valof "x") (list (cons "x" (int 3))))) 
(equal? (true) (mi (valof "x") (list (cons "x" (true)))))
(equal? (false) (mi (valof "x") (list (cons "x" (false)))))
(equal? (complex (int 1) (int 4)) (mi (valof "x") (list (cons "x" (complex (int 1) (int 4))))))


(equal? (int 3) (mi (var "x" (empty) (int 3)) (empty)))
(equal? (true) (mi (var "x" (empty) (true)) (empty)))
(equal? (false) (mi (var "x" (empty) (false)) (empty)))
(equal? (complex (int 3) (int 1)) (mi (var "x" (empty) (complex (int 3) (int 1))) (empty)))

(newline)
(print "TESTI FUNKCIJ")
(newline)

(define quick-sort 
  (fun "quick_sort" (list "lst")
    (var "filter"
      (fun "filter_" (list "foo" "lst")
        (if-then-else (is-empty (valof "lst"))(empty)
          (if-then-else (call (valof "foo")(list (hd (valof "lst"))))
            (:: (hd (valof "lst")) (call (valof "filter_") (list (valof "foo") (tl (valof "lst")))))
            (call (valof "filter_") (list (valof "foo") (tl (valof "lst")))))))
      ;end of filter, start quicksort
      (if-then-else (is-empty (tl (valof "lst")))(valof "lst")
      (var "left" 
      (call (valof "filter") (list (fun "" (list "x") (lt (valof "x") (hd (valof "lst"))))(valof "lst")))
        (var "right" (call (valof "filter") (list (fun "" (list "x") (gt (valof "x") (hd (valof "lst")))) (valof "lst")))
      (@ (call (valof "quick_sort")(list (valof "left")))(:: (hd (valof "lst")) (call (valof "quick_sort")(list (valof "right"))) ))  ))
      ))
    )
)

(define foldl_
  (fun "foldl" (list "foo" "accum" "lst")
    (if-then-else (is-empty (valof "lst")) (valof "accum")
      (call (valof "foldl")(list (valof "foo") 
                            (call (valof "foo")(list (hd (valof "lst"))(valof "accum"))) 
                            (tl (valof "lst")))))
  )
)

(define foldr_
  (fun "foldr" (list "foo" "accum" "lst")
    (if-then-else (is-empty (valof "lst")) (valof "accum")
      (call (valof "foo") (list (hd (valof "lst"))
        (call (valof "foldr") (list (valof "foo")(valof "accum")(tl(valof "lst")))))))
  )
)

(define sum_ 
  (fun "sum" (list "lst") 
    (call foldl_ (list 
      (fun "" (list "x" "X")(add (valof "x")(valof "X")))(int 0) (valof "lst")))
  )
)

(define map_
  (fun "map" (list "foo" "lst")
    (call foldr_ (list
      (fun "" (list "x" "X") (:: (call (valof "foo") (list (valof "x"))) (valof "X"))) (empty)(valof "lst"))))
)

(define square_
  (fun "square" (list "lst")
    (call map_ (list (fun "" (list "x") (mul (valof "x")(valof "x"))) (valof "lst"))))
)

(define listic (:: (int 8)(:: (int 10)(:: (int 2)(:: (int 6) (:: (int 1) (:: (int 9) (:: (int 11) (empty)))))))))


(print "Quick-sort test")
(newline)
(equal? (:: (int 1) (:: (int 2) (:: (int 6) (:: (int 8) (:: (int 9) (:: (int 10) (:: (int 11) (empty)))))))) (mi (call quick-sort (list listic)) null))

(print "Sum test")
(newline)
(equal? (int 47) (mi (call sum_ (list listic))null))

(print "Square test")
(newline)
(equal? (:: (int 64) (:: (int 100) (:: (int 4) (:: (int 36) (:: (int 1) (:: (int 81) (:: (int 121) (empty)))))))) (mi (call square_ (list listic))null))



(define fib (fun "fib" (list "n")
                 (if-then-else (gt (int 3) (valof "n"))
                               (int 1)
                               (add (call (valof "fib") (list (add (valof "n") (int -1))))
                                    (call (valof "fib") (list (add (valof "n") (int -2))))))))
(print "Fib test")
(newline)
(equal? (int 5)(mi (call fib (list (int 5))) null))
(equal? (int 8)(mi (call fib (list (int 6))) null))
(equal? (int 1)(mi (call fib (list (int 1))) null))

(define reverse (fun "reverse" (list "xs")
                     (if-then-else
                      (is-empty (valof "xs"))
                      (empty)
                      (@ (call (valof "reverse") (list (tl (valof "xs"))))
                               (:: (hd (valof "xs")) (empty))))))
(print "Reverse test")
(newline)
(equal? (:: (int 3) (:: (int 4) (:: (int 5) (empty)))) (mi (call reverse (list (:: (int 5) (:: (int 4) (:: (int 3) (empty)))))) null))
(equal? (:: (int 3) (:: (int 4) (:: (int 6)(:: (int 5) (empty))))) (mi (call reverse (list (:: (int 5) (:: (int 6)(:: (int 4) (:: (int 3) (empty))))))) null))



(print "Tail-reverse test")
(newline)
(define tail-reverse (fun "tail-reverse" (list "xs")
                     (var "aux" (fun "aux" (list "xs" "acc")
                                    (if-then-else
                                     (is-empty (valof "xs"))
                                     (valof "acc")
                                     (call (valof "aux") (list (tl (valof "xs")) (:: (hd (valof "xs")) (valof "acc"))))))
                     (call (valof "aux") (list (valof "xs") (empty))))))

(equal? (:: (int 3) (:: (int 4) (:: (int 5) (empty)))) (mi (call tail-reverse (list (:: (int 5) (:: (int 4) (:: (int 3) (empty)))))) null))

(newline)
(print "TESTI MAKRO")
(newline)
(equal? (mi (to-complex (int 2)) null) (complex (int 2) (int 0)))
(equal? (mi (to-complex (int -2)) null) (complex (int -2) (int 0)))
(equal? (mi (to-complex (add (int 1) (int -3))) null) (complex (int -2) (int 0)))

(equal? (mi (conj (complex (int -1) (int 0))) null) (complex (int -1) (int 0)))
(equal? (mi (conj(complex (int 2) (int 5))) null) (complex (int 2) (int -5)))
(equal? (mi (conj(complex (int 2) (add (int 2) (int 3)))) null) (complex (int 2) (int -5)))

(equal? (mi (~ (int 2)) null) (int -2) )
(equal? (mi (~ (int -2)) null) (int 2) )
(equal? (mi (~ (add (int -1) (int -1))) null) (int 2) )

(equal? (mi (lt (int 1) (int 5)) null) (true))
(equal? (mi (lt (int 5) (int 1)) null) (false))
(equal? (mi (lt (add (int 4)(int 5)) (int 1)) null) (false))

(equal? (mi (same (int 5) (int 5)) null) (true))
(equal? (mi (same (int 5) (int 1)) null) (false))
(equal? (mi (same (int 5) (add (int 1) (int 4))) null) (true))
(newline)



(printf "\nTest envelope - argument shadows function name\n")
(equal? (list)(envelope-env(mi (fun "foo" (list "foo") (valof "foo"))null)))
(printf "Result:\n")
(equal? (int 10) (mi (call(fun "foo" (list "foo") (valof "foo"))(list (int 10)))null))


(printf "\nTest envelope - variable inside function shadows function argument\n")
( equal? (list) (envelope-env(mi (fun "foo" (list "foo") (var "foo" (add (valof "foo") (int 11))(valof "foo")))null)))
(printf "Result:\n")
(equal? (int 21) (mi (call(fun "foo" (list "foo") (var "foo" (add (valof "foo") (int 11))(valof "foo")))(list (int 10)))null))


|#


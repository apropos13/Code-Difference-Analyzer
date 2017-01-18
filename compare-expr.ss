

(define ns (make-base-namespace))




(define constant?
  (lambda (n)
    (cond ( [number? n] #t)
          ( [boolean? n] #t)
          ( [string? n] #t)
          ( [char? n] #t)
          (else #f))

    ))


;If two lists have same length then true else false
(define eq_length?
  (lambda (li1 li2)
    (cond
     ; [ (number? li1) #t] ;if a number then true
      [ ( eqv? (length li1) (length li2)) #t]
      [else #f])))


  
;Handles comparison of Procedure vs procedure; Does not handle comparisons of the type '(f (a b)). Problem with
;the interior brackets, when we see those we have to step in and compare the sublists
(define naive1-compare
  (lambda (li1 li2)
    (cond
      [(not( eq_length? li1 li2)) `(if TCP ,li1 ,li2)] ;Not equal length. TCP from the beginning
      [(null? li1) '() ] ;Base case
      [(or  (and (list? (car li1)) (list (car li2)))  (and (constant? (car li1)) (constant? (car li2))) ) (cons
                                                  (delegator (car li1) (car li2)) (naive1-compare (cdr li1) (cdr li2)) ) ]
      ;if you see a sublist, call naive-cmpr again and call naive-cmpr on the rest so you can continue comparing the rest of the lists 
      [(eqv? (car li1) (car li2) ) (cons (car li1) (naive1-compare  (cdr li1) (cdr li2)))] ;Same head, then append
      [else (cons `(if TCP ,(car li1) ,(car li2)) (naive1-compare  (cdr li1) (cdr li2))) ] ;Different head, use if structure

      )))


;Expect input '((a 1) (b 2)) '((a 2) (b 3))
;Checks to see if identifiers match (names and position) 
(define check-identifiers
  (lambda (id1 id2)
    (cond
      [(not( eq_length? id1 id2)) #f ]
      [(null? id1) #t ] ;Base case
      [ (not (eqv? (car (car id1)) (car (car id2))))  #f]
      [else (check-identifiers (cdr id1) (cdr id2)) ]

      )))


(define fix-identifiers ;fixes the format inside the identifiers section of the let expression
  (lambda (id1 id2)
    (cond
      [(null? id1) '() ] ;Base case
      [  (eqv? (cdr (car id1)) (cdr (car id2))) (fix-identifiers (cdr id1) (cdr id2))] ;Check to see if the bindings are the same , if yes continue 
      [else (cons `( ,(car (car id1)) if TCP ,(car (cdr (car id1))) ,(car (cdr (car id2))) ) (fix-identifiers (cdr id1) (cdr id2)) ) ] ;if bindings are not the same then use if structure

      )))


 ;Assumed expression to be passed is: compare-let '(let ((a 1)) (f a)) '(let ((a 2)) (g a))
(define compare-let
  (lambda (li1 li2)
    (cond
      [(check-identifiers (car (cdr li1) ) (car (cdr li2)))
       ( cons 'let
              (cons (fix-identifiers (car (cdr li1) ) (car (cdr li2)) ) (naive1-compare (cdr (cdr li1) ) (cdr (cdr li2)))))]
      ;if identifiers are equal(name and position) then fix the appearance of the identifiers and append that to body. What happens in empty body?
      [else `(if TCP ,li1 ,li2)] ))) ;Else just put TCP at the beginning




(define compare-constant
  (lambda (c1 c2)
    (cond
      [(and (eqv? c1 #t) (eqv? c2 #t)) #t]
      [(and (eqv? c1 #t) (eqv? c2 #f)) 'TCP] 
      [(and (eqv? c1 #f) (eqv? c2 #f)) #f]
      [(and (eqv? c1 #f) (eqv? c2 #t)) '(not TCP)]
      [(eqv? c1 c2) c1]
      [else `(if TCP ,c1 ,c2)] ))) 


(define compare-lambda ;Expects expression '((lambda (a) (f a)) 1)
  (lambda (li1 li2)
    (cond  ;if equal identifiers (name and order)                 ;then fix functions and cons with parameter
      [( equal? (car (cdr (car li1))) (car (cdr (car li2))) )  ( cons
                                                                 `(lambda ,(car (cdr (car li1))) ;this is lambda (list of parameters)
                                                                    ,(naive1-compare (car (cdr (cdr (car li1)))) (car (cdr (cdr (car li2)))))) ;appends the function part
                                                                 (naive1-compare (cdr li1) (cdr li2) ))] ;tail= number part
      [( equal? (cdr li1) (cdr li2)) (cons `(if TCP ,(car li1) ,(car li2) ) (cdr li1) )  ] ;If bindings are different but numbers the same (last case in spec)
      [else `(if TCP ,li1 ,li2)]
      )))


(define compare-quote
  (lambda (li1 li2)
    (cond
      [( equal? (car (cdr li1)) (car (cdr li2)) ) `,li1 ]
      [else `(if TCP ,li1 ,li2)]
      )))

    
(define delegator
  (lambda (li1 li2)
    (cond
      [ (and (constant? li1) (constant? li2) ) ( compare-constant li1 li2)] ;constant
      [ (and (eqv? 'let (car li1)) (eqv? 'let (car li2))  ) ( compare-let li1 li2)] ;let expressions
      [ (and (list? (car li1)) (list? (car li2)))  (cond ;Check that both are a list of lists then proceed
                            [ (and (eqv? 'lambda (car (car li1))) (eqv? 'lambda (car(car li2)))  ) ( compare-lambda li1 li2)]) ] ;lambda expressions
      [ (and (eqv? ( car li1) 'quote) (eqv? (car li2) 'quote) )  (compare-quote li1 li2)] ;quote expressions
      

      ;Trivial Case to catch here: if  head of the list is not the same, and head of either list is if, just put TCP at the beginning
      [( and (not ( eqv? (car li1) (car li2))) (or (eqv? (car li1) 'if) (eqv? (car li2) 'if)))  `(if TCP ,li1 ,li2) ]
      ;Catches '(if x y z) '(g x y z)
      [else (naive1-compare li1 li2)]

      )))

   
(define compare-expr
  (lambda (li1 li2)
    (cond
      [ (and (constant? li1) (constant? li2) ) ( delegator li1 li2)]
      [ (and (list? li1) (list? li2) ) (delegator li1 li2)]
      
      [else `(if TCP ,li1 ,li2)] ;catches cases such as: (compare-expr 'a '(cons a b))

      )))







(define replace ;replaces TCP with true or false 
  (lambda (bool li)
    (cond
      [(null? li) '()]
      [(list? (car li)) ( cons (replace bool (car li)) (replace bool (cdr li)))]
      [(eqv? 'TCP (car li)) (cons bool (replace bool (cdr li) ) ) ]
      [else (cons (car li) (replace bool (cdr li))) ]
      )))


(define test-compare-expr
  (lambda (li1 li2)
    (cond
      [ ( and
          (equal? (eval li1 ns) (eval (replace #t (compare-expr li1 li2 ) ) ns )) ;if TCP true
          (equal? (eval li2 ns) (eval (replace #f (compare-expr li1 li2 ) ) ns )) ) ;if TCP false
          #t]
      [else #f]
      )))


(define test-x '(* 3 5 #t #f #f #t
                   x
                   (list x y)
                   (cons (cons x y) (cons b c))
                   (cons (cons x y) (cons c b))
                   (let ((a 1)) (cons a b))
                   (let ((a 1) (b 10)) (list a b))
                   (if x y)
                   ((lambda (a b) (g a b)) 1 5)
                   ((lambda (a c) (f u b)) 1 2)
                   ((lambda (a c) (g a b)) 1 5)
                   ((lambda (a c) (f a l)) 1 5)
                     '(a b)
                     '(a c) ))
(define test-y '(+ 2 5 #f #f #t #t
                   (list a b)
                   (list x y)
                   (cons (cons y x) (cons b c))
                   (cons (cons x y) (cons b c))
                   (let ((a -1)) (quote( a c)))
                   (let ((a 1) (b 10)) (cons a b))
                   (g x y)
                   ((lambda (a b) (f a b)) 0 2)
                   ((lambda (a b) (f a b)) 1 2)
                   ((lambda (a x) (g a b)) 1 0)
                   ((lambda (a c) (f a b)) 1 5)
                     '(a b)
                     '(a x) ))

;(compare-expr test-x test-y)


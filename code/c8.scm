(define nil (quote ()))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
  
(define add1
  (lambda (n)
    (+ n 1)))
  
(define sub1
  (lambda (n)
    (- n 1)))
  
(define add
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (add n (sub1 m)))))))

(define sub
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (sub n (sub1 m)))))))

(define product
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (add n (product n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond ((less n m) 0)
          (else (add1 (div (sub n m) m))))))

(define exp
  (lambda (n m)
    (cond ((zero? m) 1)
          (else (product n (exp n (sub1 m)))))))

(define greater
  (lambda (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (greater (sub1 n) (sub1 m))))))


(define less
  (lambda (n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else (less (sub1 n) (sub1 m))))))
        
(define equal
  (lambda (n m)
    (cond ((greater n m) #f)
          ((less n m) #f)
          (else #t))))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
                (equal a1 a2))
          ((or (number? a1) (number? a2))
               #f)
          (else (eq? a1 a2)))))
        
(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l2) (null? l1)) #f)
          ((and (atom? (car l1)) (atom? (car l2)))
                (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
          ((or (atom? (car l1)) (atom? (car l2))) #f)
          (else (and (eqlist? (car l1) (car l2))
                     (eqlist? (cdr l1) (cdr l2)))))))

;(eqlist? (list "beef" (list (list "sauage") (list "and" (list "soda")))) c)
;(eqlist? c c)

(define def-equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
          ((or (atom? s1) (atom? s2)) #f)
          (else (eqlist? s1 s2)))))
        

(define rember-f
  (lambda (test? s l)
    (cond ((null? l) nil)
          ((test? (car l) s) (cdr l))
          (else (cons (car l)
                      (rember-f test? s (cdr l)))))))

;(rember-f def-equal? '(pop corn) '(lemonade (pop corn) and (cake)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

;((eq?-c 2) 2)

(define rember-f-opt
  (lambda (test?)
    (lambda (s l)
      (cond ((null? l) nil)
            ((test? (car l) s) (cdr l))
            (else (cons (car l)
                        ((rember-f-opt test?) l (cdr s))))))))

;((rember-f-opt def-equal?) '(pop corn) '(lemonade (pop corn) and (cake)))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond ((null? lat) nil)
            ((test? (car lat) old) (cons new lat))
            (else (cons (car lat)
                        ((insertL-f test?) new old (cdr lat))))))))

;((insertL-f def-equal?) "coffee" "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond ((null? lat) nil)
            ((test? (car lat) old) (cons old (cons new (cdr lat))))
            (else (cons (car lat)
                        ((insertR-f test?) new old (cdr lat))))))))
                      
;((insertR-f def-equal?) "coffee" "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))

(define insert-g
  (lambda (direction-f)
    (lambda (test?)
      (lambda (new old lat)
        ((direction-f test?) new old lat)))))
      
;(((insert-g insertR-f) def-equal?) "coffee" "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))

;(((insert-g insertL-f) def-equal?) "coffee" "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))
  
(define insert-g2
  (lambda (test?)
    (lambda (seq)
      (lambda (new old lat)
        (cond ((null? lat) nil)
              ((test? (car lat) old) (seq new old lat))
              (else (cons (car lat)
                          (((insert-g2 test?) seq) new old (cdr lat)))))))))

;(((insert-g2 def-equal?) seqL) "coffee" "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))

;(((insert-g2 def-equal?) seqR) "coffee" "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))

(define insertL-f2
  ((insert-g2 def-equal?) seqL))

;(insertL-f2 "coffee" "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))

(define insertR-f2
  ((insert-g2 def-equal?) seqR))

;(insertL-f2 "coffee" "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) nil)
          ((eq? (car lat) old) (cons new (cdr lat)))
          (else (cons (car lat)
                      (subst new old (cdr lat)))))))

(define seqS
  (lambda (new old lat)
    (cons new (cdr lat))))

(define subst-g ((insert-g2 def-equal?) seqS))

;(subst "coffee" "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))
;(subst-g "coffee" "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))

(define seqrem
  (lambda (new old l)
    (cdr l)))
  
(define rember-g
  (lambda (a l)
    (((insert-g2 def-equal?) seqrem) #f a l)))
  
  
;(rember-g "pop" '("lemonade" "pop" ("pop" "corn") "and" ("cake")))


;(+ (x 3 4) (+ 4 6))
(define first-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
  

(define second-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))
  
(define preOrderValue
  (lambda (nexp)
    (cond ((atom? nexp) (cond
                          ((number? nexp) nexp)
                          (else (error "not an arithmatic expression"))))
          ((eq? (operator nexp) (quote add))
                (add (preOrderValue (first-sub-exp nexp))
                     (preOrderValue (second-sub-exp nexp))))
          ((eq? (operator nexp) (quote sub))
                (sub (preOrderValue (first-sub-exp nexp))
                     (preOrderValue (second-sub-exp nexp))))
          ((eq? (operator nexp) (quote product))
                (product (preOrderValue (first-sub-exp nexp))
                     (preOrderValue (second-sub-exp nexp))))
          ((eq? (operator nexp) (quote div))
                (div (preOrderValue (first-sub-exp nexp))
                     (preOrderValue (second-sub-exp nexp))))
          ((eq? (operator nexp) (quote exp))
                (exp (preOrderValue (first-sub-exp nexp))
                     (preOrderValue (second-sub-exp nexp))))
          (else (error "not support this operation")))))

(define atom-to-function
  (lambda (x)
    (cond ((eq? x (quote add)) add)
          ((eq? x (quote sub)) sub)
          ((eq? x (quote product)) product)
          ((eq? x (quote div)) div)
          ((eq? x (quote exp)) exp)
          (else (error "not support this operation")))))


(define preOrderValueOpt
  (lambda (nexp)
    (cond ((atom? nexp) (cond ((number? nexp) nexp)
                              (else (error "not an arithmatic expression"))))
          (else ((atom-to-function (operator nexp)) (preOrderValueOpt (first-sub-exp nexp))
                                                    (preOrderValueOpt (second-sub-exp nexp)))))))
                                                  
;(preOrderValueOpt '(add (product 3 6) (exp 8 2)))

(define multirember 
  (lambda (a lat) 
    (cond 
      ((null? lat) nil)
      ((eq? (car lat) a) 
            (multirember a (cdr lat))) 
      (else (cons (car lat) 
                  (multirember a (cdr lat)))))))
                

(define multirember-f
  (lambda (test?)
    (lambda (a lat) 
      (cond 
        ((null? lat) nil)
        ((test? (car lat) a) 
              ((multirember-f test?) a (cdr lat))) 
        (else (cons (car lat) 
                    ((multirember-f test?) a (cdr lat))))))))

((multirember-f def-equal?) (quote tuna) '(shrimp salad tuna salad and tuna))
                
(define eq?-tuna
  (eq?-c (quote tuna)))


(define multiremberT
  (lambda (test? lat)
      (cond 
        ((null? lat) nil)
        ((test? (car lat)) 
               (multiremberT test? (cdr lat)))
        (else (cons (car lat) 
                    (multiremberT test? (cdr lat)))))))
                  
;(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

(define multiremberCol
  (lambda (a lat col)
    (cond ((null? lat) (col nil nil))
          ((eq? (car lat) a)
                (multiremberCol a 
                                (cdr lat)
                                (lambda (newlat seen)
                                        (col newlat
                                             (cons (car lat) seen)))))
          (else (multiremberCol a
                                (cdr lat)
                                (lambda (newlat seen)
                                        (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

;(multiremberCol (quote tuna) '(tuna) a-friend)

#|
(define new-friend
  (lambda (newlat seen)
    (col newlat
         (cons (car lat) seen))))
|#

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
         (cons (quote tuna) seen))))

;(new-friend '(1 2 3) nil)

(define length
  (lambda (x)
    (cond ((null? x) 0)
          (else (add1 (length (cdr x)))))))
      
(define last-friend
  (lambda (x y)
    (length x)))


;(multiremberCol (quote tuna) '(strawberries tuna and swordfish) last-friend)

(define multiInsertL
  (lambda (new old lat)
    (cond ((null? lat) nil)
          ((eq? (car lat) old)
                (cons new (cons old (multiInsertL new old (cdr lat)))))
          (else (cons (car lat)
                      (multiInsertL new old (cdr lat)))))))
                  
(define multiInsertR
  (lambda (new old lat)
    (cond ((null? lat) nil)
          ((eq? (car lat) old)
                (cons old (cons new (multiInsertR new old (cdr lat)))))
          (else (cons (car lat)
                      (multiInsertR new old (cdr lat)))))))
              
;(multiInsertL (quote red) (quote tuna) '(strawberries tuna and swordfish))

;(multiInsertR (quote red) (quote tuna) '(strawberries tuna and swordfish))

(define multiInsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) nil)
          ((eq? (car lat) oldL)
                (cons oldL (cons new (multiInsertLR new oldL oldR (cdr lat)))))
          ((eq? (car lat) oldR)
                (cons new (cons oldR (multiInsertLR new oldL oldR (cdr lat)))))
          (else (cons (car lat)
                      (multiInsertLR new oldL oldR (cdr lat)))))))
            
;(multiInsertLR (quote red) (quote tuna) (quote swordfish) '(strawberries tuna and swordfish))


(define multiInsertLRCol
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col nil 0 0))
          ((eq? (car lat) oldL)
                (multiInsertLRCol new oldL oldR (cdr lat) (lambda (newlat L R)
                                                            (col (cons new (cons oldL newlat)) (add1 L) R))))
          ((eq? (car lat) oldR)
                (multiInsertLRCol new oldL oldR (cdr lat) (lambda (newlat L R)
                                                            (col (cons oldR (cons new  newlat)) L (add1 R)))))
          (else (multiInsertLRCol new oldL oldR (cdr lat)
                                  (lambda (newlat L R)
                                    (col (cons (car lat) newlat) L R)))))))

(define printInfo
  (lambda (lat L R)
    (display "(")
    (display lat)
    (display " ")
    (display L)
    (display " ")
    (display R)
    (display ")")
    (newline)))
  
;(multiInsertLRCol (quote salty) (quote fish) (quote chips) '(chips and fish or fish and chips) printInfo)

(define def-even?
  (lambda (n)
    (equal (product (div n 2) 2) n)))

(define even-only*
  (lambda (lat)
    (cond ((null? lat) nil)
          ((atom? (car lat))
                  (cond ((def-even? (car lat)) (cons (car lat)
                                                     (even-only* (cdr lat))))
                        (else (even-only* (cdr lat)))))
          (else (cons (even-only* (car lat))
                      (even-only* (cdr lat)))))))
;(even-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define even-only*Col 
  (lambda (lat col)
    (cond ((null? lat) (col nil 1 0))
          ((atom? (car lat))
                  (cond ((def-even? (car lat))
                                    (even-only*Col (cdr lat) (lambda (newlat p s)
                                                               (col (cons (car lat) newlat) (product (car lat) p) s))))
                        (else (even-only*Col (cdr lat) (lambda (newlat p s)
                                                         (col newlat p 
                                                              (add s (car lat))))))))
          (else (even-only*Col (car lat) (lambda (al ap as)
                                                 (even-only*Col (cdr lat)
                                                                (lambda (dl dp ds)
                                                                  (col (cons al dl)
                                                                       (product ap dp)
                                                                       (add as ds))))))))))
                      

(define the-last-friend
  (lambda (newlat p s)
    (cons p (cons s newlat))))
  
(even-only*Col '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
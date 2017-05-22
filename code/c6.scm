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
        
(define div
  (lambda (n m)
    (cond ((less n m) 0)
          (else (add1 (div (sub n m) m))))))

(define exp
  (lambda (n m)
    (cond ((zero? m) 1)
          (else (product n (exp n (sub1 m)))))))


;(define y (quote a))
;(eq? (quote a) y)

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
                (equal a1 a2))
          ((or (number? a1) (number? a2))
               #f)
          (else (eq? a1 a2)))))
        
(define s '(1 add ((3 product 4) sub (4 product 5))))

(define atom?
  (lambda (n)
    (and (not (pair? n)) (not (null? n)))))

(define numbered? 
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          ((or (eq? (car (cdr aexp)) (quote add))
               (eq? (car (cdr aexp)) (quote sub))
               (eq? (car (cdr aexp)) (quote div))
               (eq? (car (cdr aexp)) (quote product))
               (eq? (car (cdr aexp)) (quote exp)))
                (and (numbered? (car aexp)) 
                     (numbered? (car (cdr (cdr aexp))))))
          (else (error "not support that operation")))))
                   
;(numbered? s)

(define numbered2?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          (else (and (numbered2? (car aexp))
                     (numbered2? (car (cdr (cdr aexp)))))))))

;(numbered2? s)
(define value
  (lambda (nexp)
    (cond ((atom? nexp) (cond
                          ((number? nexp) nexp)
                          (else (error "not an arithmatic expression"))))
          ((eq? (car (cdr nexp)) (quote add))
                (add (value (car nexp))
                     (value (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp)) (quote sub))
                (sub (value (car nexp))
                     (value (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp)) (quote product))
                (product (value (car nexp))
                         (value (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp)) (quote div))
                (div (value (car nexp))
                     (value (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp)) (quote exp))
                (exp (value (car nexp))
                     (value (car (cdr (cdr nexp))))))
          (else (error "not support this operation")))))

;(value '((1 add (4 product 5)) sub (2 exp 3)))

(define preOrderValue
  (lambda (nexp)
    (cond ((atom? nexp) (cond
                          ((number? nexp) nexp)
                          (else (error "not an arithmatic expression"))))
          ((eq? (car nexp) (quote add))
                (add (preOrderValue (car (cdr nexp)))
                     (preOrderValue (car (cdr (cdr nexp))))))
          ((eq? (car nexp) (quote sub))
                (sub (preOrderValue (car (cdr nexp)))
                     (preOrderValue (car (cdr (cdr nexp))))))
          ((eq? (car nexp) (quote product))
                (product (preOrderValue (car (cdr nexp)))
                     (preOrderValue (car (cdr (cdr nexp))))))
          ((eq? (car nexp) (quote div))
                (div (preOrderValue (car (cdr nexp)))
                     (preOrderValue (car (cdr (cdr nexp))))))
          ((eq? (car nexp) (quote exp))
                (exp (preOrderValue (car (cdr nexp)))
                     (preOrderValue (car (cdr (cdr nexp))))))
          (else (error "not support this operation")))))

;(preOrderValue '(add (product 3 6) (exp 8 2)))

(define first-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
  

(define second-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))
  
(define preOrderValueOpt
  (lambda (nexp)
    (cond ((atom? nexp) (cond
                          ((number? nexp) nexp)
                          (else (error "not an arithmatic expression"))))
          ((eq? (operator nexp) (quote add))
                (add (preOrderValueOpt (first-sub-exp nexp))
                     (preOrderValueOpt (second-sub-exp nexp))))
          ((eq? (operator nexp) (quote sub))
                (sub (preOrderValueOpt (first-sub-exp nexp))
                     (preOrderValueOpt (second-sub-exp nexp))))
          ((eq? (operator nexp) (quote product))
                (product (preOrderValueOpt (first-sub-exp nexp))
                     (preOrderValueOpt (second-sub-exp nexp))))
          ((eq? (operator nexp) (quote div))
                (div (preOrderValueOpt (first-sub-exp nexp))
                     (preOrderValueOpt (second-sub-exp nexp))))
          ((eq? (operator nexp) (quote exp))
                (exp (preOrderValueOpt (first-sub-exp nexp))
                     (preOrderValueOpt (second-sub-exp nexp))))
          (else (error "not support this operation")))))
        
;(preOrderValueOpt '(add (product 3 6) (exp 8 2)))

;using () repressent zero
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))
  

(define zub1
  (lambda (n)
    (cdr n)))
  
(define sadd
  (lambda (n m)
    (cond ((sero? m) n)
          (else (edd1 (sadd n (zub1 m)))))))
        

(define zsub
  (lambda (n m)
    (cond ((sero? m) n)
          (else (zub1 (zsub n (zub1 m)))))))

;(sero? (quote ()))

(edd1 (quote ()))
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

(define product
  (lambda (n m)
    (cond ((zero? m) n)
          (add n (product n (sub1 m))))))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
                (equal a1 a2))
          ((or (number? a1) (number? a2))
               #f)
          (else (eq? a1 a2)))))
        
(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))
  

(define rember*
  (lambda (a l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
                  (cond ((eq? (car l) a)
                              (rember* a (cdr l)))
                        (else (cons (car l)
                                    (rember* a (cdr l))))))
          (else (cons (rember* a (car l))
                      (rember* a (cdr l)))))))

(define x (list (list (list "tomato" "sause")) (list (list "bean") "sause") (list "and" (list (list "flying")) "sause")))

(define y (list "tomato" "sause" "bean" "flying"))

;(rember* "sause" (list (list (list "tomato" "sause")) (list (list "bean") "sause") (list "and" (list (list "flying")) "sause")))

(define lat?
  (lambda (lat)
    (cond ((null? lat) #t)
          ((atom? (car lat)) (lat? (cdr lat)))
          (else #f))))
;(lat? x)
;(lat? y)

(define insertR*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
                  (cond ((eq? (car l) old)
                         (cons (car l) (cons new (insertR* new old (cdr l)))))
                        (else (cons (car l)
                                    (insertR* new old (cdr l))))))
          (else (cons (insertR* new old (car l))
                      (insertR* new old (cdr l)))))))

(define z (list (list "how" "much" (list "wood")) "could" (list (list "a" (list "wood") "chuck")) (list (list (list "chuck"))) (list "if" (list "a") (list (list "wood" "chuck"))) "could" "chuck" "wood"))


;(insertR* "roast" "chuck" z)

(define insertL*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
                  (cond ((eq? (car l) old)
                         (cons new (cons  (car l) (insertL* new old (cdr l)))))
                        (else (cons (car l)
                                    (insertL* new old (cdr l))))))
          (else (cons (insertL* new old (car l))
                      (insertL* new old (cdr l)))))))
;(insertL* "roast" "chuck" z)

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((atom? (car l))
                  (cond ((eq? (car l) a)
                              (add1 (occur* a (cdr l))))
                        (else (occur* a (cdr l)))))
          (else (add (occur* a (car l))
                     (occur* a (cdr l)))))))

(define a (list (list "banana") (list "split" (list (list (list (list "banana" "ice"))) (list "cream" (list "banana")) "sherbet")) (list "banana") (list "bread") (list "banana" "brandy")))

;(occur* "banana" a)

(define subst*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
                  (cond ((eq? (car l) old)
                              (cons new (subst* new old (cdr l))))
                        (else (cons (car l) (subst* new old (cdr l))))))
          (else (cons (subst* new old (car l))
                      (subst* new old (cdr l)))))))


;(subst* "orange" "banana" a)

(define member*
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l))
                  (or (eq? (car l) a) (member* a (cdr l))))
          (else (or (member* a (car l))
                    (member* a (cdr l)))))))


(define c (list (list "photo") (list "chips" (list (list "with") "fish") (list "chips"))))

;(member* "chips" c)

(define leftmost
  (lambda (l) 
    (cond ((null? l) "no leftmost")
          ((atom? (car l)) (car l))
          (else (leftmost (car l))))))

;(leftmost (quote ()))
;(leftmost c)

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
        

(define rember
  (lambda (s l)
    (cond ((null? l) (quote ()))
          ((def-equal? (car l) s) (cdr l))
          (else (cons (car l)
                      (rember s (cdr l)))))))
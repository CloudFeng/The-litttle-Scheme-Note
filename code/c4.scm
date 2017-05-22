(define add1
  (lambda (n)
    (+ n 1)))

;(add1 2)

(define sub1
  (lambda (n)
    (- n 1)))

;(sub1 2)

(define add
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (add n (sub1 m)))))))

;(add 2 3)

(define sub
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (sub n (sub1 m)))))))

(define mutil
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (add n (mutil n (sub1 m)))))))

(define exp
  (lambda (n m)
    (cond ((zero? m) 1)
          ((mutil n (exp n (sub1 m)))))))

;(exp 1 1)
;(exp 2 3)
;(exp 5 3)

(define greater
  (lambda (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (greater (sub1 n) (sub1 m))))))
        
;(greater 1 2)

(define less
  (lambda (n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else (less (sub1 n) (sub1 m))))))
        
;(less 1 2)

(define equal
  (lambda (n m)
    (cond ((greater n m) #f)
          ((less n m) #f)
          (else #t))))
        
(define div
  (lambda (n m)
    (cond ((less n m) 0)
          (else (add1 (div (sub n m) m))))))
;(div 15 2)

(define length
  (lambda (lat)
    (cond ((null? lat) 0)
          ((add1 (length (cdr lat)))))))

;(length (quote (1 2 3 4 5)))

(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else (pick (sub1 n) (cdr lat))))))
;(pick 2 (list 1 2 3 4 5))

(define rempick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (cdr lat))
          (else (cons (car lat)
                      (rempick (sub1 n) (cdr lat)))))))
                    
;(rempick 2 (list 1 2 3 4 5))

;(number? "atom")

(define no-nums
  (lambda (lat)
    (cond ((null? lat) (quote ()))
          ((number? (car lat)) (no-nums (cdr lat)))
          (else (cons (car lat)
                      (no-nums (cdr lat)))))))
;(no-nums (list "a" 1 2 "a" "b" "c" 3))

(define all-nums
  (lambda (lat)
    (cond ((null? lat) (quote ()))
          ((number? (car lat)) 
                    (cons (car lat) 
                          (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))
;(all-nums (list "a" 1 2 "a" "b" "c" 3))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2)) (equal a1 a2))
          ((or (number? a1) (number? a2)) #f)
          (else (eq? a1 a2)))))

;(eqan? 1 "a")
;(eqan? "a" "a")
;(eqan? 1 1)

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))
;(occur 5 (list "a" "1" 1 2 3 4 1 "b"))
(occur 1 (list "a" "1" 1 2 3 4 1 "b"))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else (add (car tup)
                     (addtup (cdr tup)))))))

;(addtup (list 1 2 3 4 5))

(define mutil
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (add n (mutil n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else (cons (add (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))))))

(define nil (quote ()))

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
        
(define atom?
  (lambda (lat)
    (and (not (pair? lat)) (not (null? lat)))))

(define pick
  (lambda (n lat)
    ;(display n)
    (cond ((equal n 1) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(define keep-looking
  (lambda (a n lat)
    ;(display n)
    ;(newline)
    (cond ((number? n) (keep-looking a (pick n lat) lat))
          (else (eq? n a)))))
        
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

;(looking (quote caviar) '(6 2 4 1 5 7 3))

(define first
  (lambda (x)
    (car x)))
  
(define second
  (lambda (x)
    (car (cdr x))))
  
(define build
  (lambda (x y)
    (cons x (cons y nil))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))
                
;(shift '((a b) (c d)))

(define a-pair?
  (lambda (pair)
    (cond ((atom? pair) #f)
          ((null? pair) #f)
          ((null? (cdr pair)) #f)
          ((null? (cdr (cdr pair))) #t)
          (else #f))))
  
(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
                    (align (shift pora)))
          (else (build (first pora)
                       (align (second pora)))))))

;(align '((a b) c d))

(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (add (length* (first pora))
                     (length* (second pora)))))))

;(lenght* (align '((a b) c d)))

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (add (product (weight* (first pora)) 2)
                     (weight* (second pora)))))))
                   
;(weight* '((a b) c))

(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))
         
(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
                    (shuffle (revpair pora)))
          (else (build (first pora)
                       (shuffle (second pora)))))))

;(shuffle '((a b) c))

(define one?
  (lambda (n)
    (equal n 1)))
  
(define C
  (lambda (n)
    (cond ((one? n) 1)
          ((even? n) (C (div n 2)))
          (else (C (add1 (product 3 n)))))))
        
(define A
  (lambda (n m)
    (cond ((zero? n) (add1 m))
          ((zero? m) (A (sub1 n) 1))
          (else (A (sub1 n)
                   (A n (sub1 m)))))))
                

(define eternity
    (lambda (x)
      (eternity x)))
    

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))


(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length2)
      (lambda (l)
        (cond ((null? l) 0)
              (else (add1 (length2 (cdr l)))))))
    (mk-length mk-length)))) '(apple))
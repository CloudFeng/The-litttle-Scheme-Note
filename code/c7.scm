(define nil (quote ()))

(define x '(apple peaches apple plum))

(define y '(apples peaches pears plums))

(define z '(apple 3 pear 4 9 apple 3 4))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
  
(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))
  
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
    (cond ((and (number? a1) (number? a2)) (equal a1 a2))
          ((or (number? a1) (number? a2)) #f)
          (else (eq? a1 a2)))))

(define member?
  (lambda (n lat)
    (cond ((null? lat) #f)
          (else (or (eqan? (car lat) n)
                    (member? n (cdr lat)))))))

(define multirember
  (lambda (n lat)
    (cond ((null? lat) nil)
          ((eqan? (car lat) n) (multirember n (cdr lat)))
          (else (cons (car lat)
                      (multirember n (cdr lat)))))))
                    
(define set?
  (lambda (lat)
    ;(display lat)
    ;(newline)
    (cond ((null? lat) #t)
          ((member? (car lat) (cdr lat)) #f)
          (else (set? (cdr lat))))))

;(set? x)
;(set? y)
;(set? z)

(define makeset
  (lambda (lat)
    (cond ((null? lat) nil)
          ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
          (else (cons (car lat)
                      (makeset (cdr lat)))))))

(define testSet '(apple peach pear peach plum apple lemon peach))
;(makeset testSet)
;(multirember (quote apple) testSet)

(define makesetOpt
  (lambda (lat)
    (cond ((null? lat) nil)
           (else (cons (car lat)
                       (makesetOpt (multirember (car lat) (cdr lat))))))))
;(makesetOpt testSet)
;(makesetOpt z)

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          (else (and (member? (car set1) set2)
                     (subset? (cdr set1) set2))))))

;(subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))

;(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

;(eqset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

;(eqset? '( 6 large chickens with wings) '(6 large chickens with wings))

(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          (else (or (member? (car set1) set2)
                  (intersect? (cdr set1) set2))))))

;(intersect? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

(define intersect
  (lambda (set1 set2)
    (cond ((or (null? set1) (null? set2)) nil)
          ((member? (car set1) set2)
                    (cons (car set1)
                          (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))
        

;(intersect '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((member? (car set1) set2) (union (cdr set1) set2))
          (else (cons (car set1)
                      (union (cdr set1) set2))))))

;(union '(4 pounds of horseradish) 
;             '(four pounds chicken and 5 ounces horseradish))
 
 (define diff
   (lambda (set1 set2)
     (cond ((null? set1) nil)
           ((member? (car set1) set2) (diff (cdr set1) set2))
           (else (cons (car set1)
                       (diff (cdr set1) set2))))))         
                     
;(diff '(4 pounds of horseradish) 
;            '(four pounds chicken and 5 ounces horseradish))

(define intersectall
  (lambda (l-set)
    (cond
          ((null? (cdr l-set)) (car l-set))
          (else (intersect (car l-set)
                           (intersectall (cdr l-set)))))))

(define l-set '((6 pears and) (3 peaches and 6 peppers) (8 pears and 6 plums) (and 6 prunes with some apples)))

;(intersectall l-set)

(define a-pair?
  (lambda (x)
    (cond ((atom? x) #f)
          ((null? x) #f)
          ((null? (cdr x)) #f)
          ((null? (cdr (cdr x))) #t)
          (else #f))))

;(a-pair? '(1 2)

(define first
  (lambda (x)
    (car x)))

(define second
  (lambda (x)
    (car (cdr x))))

(define third
  (lambda (x)
    (car (cdr (cdr x)))))
  
(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 nil))))

(define testS (build '(1 2 3) '(4 5)))
;testS
;(first testS)
;(second testS)

(define revpair
  (lambda (pair)
    ;(display "origin:")
    ;(display pair)
    (build (second pair)
           (first pair))))
         
(define firsts
  (lambda (lat)
    ;(display lat)
    (cond ((null? lat) nil)
          (else (cons (car (car lat))
                      (firsts (cdr lat)))))))

;(firsts '((a b) (c d) (e f)))

(define seconds
  (lambda (lat)
    (cond ((null? lat) nil)
          (else (cons (car (cdr (car lat)))
                      (seconds (cdr lat)))))))
                    


;(seconds '((a b) (c d) (e f)))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))
;(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))

(define revel
  (lambda (rel)
    (cond ((null? rel) nil)
          (else (cons (build (second (car rel))
                             (first (car rel)))
                      (revel (cdr rel)))))))

;(revpair '(8 a))
;(revel '((8 a) (pumpkin pie) (got sick)))

(define revel-opt
  (lambda (rel)
    (cond ((null? rel) nil)
          (else (cons (revpair (car rel))
                      (revel-opt (cdr rel)))))))
;(revel-opt '((8 a) (pumpkin pie) (got sick)))

(define fullfun?
  (lambda (rel)
      (set? (seconds rel))))
       
;(fullfun? '((8 3) (4 2) (7 6) (6 2)  (3 4)))

;(fullfun? '((8 3) (4 8)  (7 6) (6 2)  (3 4)))

(define one-to-one?
  (lambda (fun)
    (fun? (revel-opt fun))))
  
(one-to-one? '((8 3) (4 8)  (7 6) (6 2)  (3 4)))

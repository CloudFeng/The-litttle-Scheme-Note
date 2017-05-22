(define atom? 
    (lambda (x)
          (and (not (pair? x)) 
               (not (null? x)))))

(define lat? 
  (lambda (x) 
     (cond ((null? x) #t)
           ((atom? (car x)) (lat? (cdr x)))
           (else #f)))

(define member?
  (lambda (a lat)
    (cond 
          ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))

(define firsts
  (lambda (x)
    (cond
      ((null? x) (quote ()))
      (else (cons (car (car x))
                  (firsts (cdr x)))))))
                
(define seconds
  (lambda (x)
    (cond
      ((null? x) (quote ()))
      (else (cons (car (cdr (car x)))
                  (seconds (cdr x)))))))
                
(define insertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old)
                (cons old
                      (cons new (cdr lat))))
          (else (cons (car lat)
                      (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old)
                (cons new lat))
          (else (cons (car lat)
                      (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old)
                  (cons new (cdr lat)))
          (else (cons (car lat)
                      (subst new old (cdr lat)))))))

(define subst2
  (lambda (new firstOld secondOld lat)
    (cond ((null? lat) (quote ()))
          ((or (eq? (car lat) firstOld) (eq? (car lat) secondOld))
               (cons new (cdr lat)))
          (else (cons (car lat) (subst2 new firstOld secondOld (cdr lat)))))))


(define multiInsertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old)
                (cons old
                      (cons new (multiInsertR new old (cdr lat)))))
          (else (cons (car lat)
                      (multiInsertR new old (cdr lat)))))))
                    

(define multiInsertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old)
                (cons new (cons old
                      (multiInsertL new old (cdr lat)))))
          (else (cons (car lat)
                      (multiInsertL new old (cdr lat)))))))

(define mutiSubst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old)
                  (cons new (mutiSubst new old (cdr lat))))
          (else (cons (car lat)
                      (mutiSubst new old (cdr lat)))))))

(define mutiSubst2
  (lambda (new firstOld secondOld lat)
    (display lat)
    (newline)
    (cond ((null? lat) (quote ()))
          ((or (eq? (car lat) firstOld) (eq? (car lat) secondOld))
               (cons new (mutiSubst2 new firstOld secondOld (cdr lat))))
          (else (cons (car lat) 
                      (mutiSubst2 new firstOld secondOld (cdr lat)))))))
                    
(define numbered? 
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          ((eq? (car (cdr aexp)) (quote add)) 
                (and (numbered? (car aexp)) 
                     (numbered? (car (cdr (cdr aexp))))))
          ((eq? (car (cdr aexp)) (quote sub)) 
                (and (numbered? (car aexp)) 
                     (numbered? (car (cdr (cdr aexp))))))
          ((eq? (car (cdr aexp)) (quote div)) 
                (and (numbered? (car aexp)) 
                     (numbered? (car (cdr (cdr aexp))))))
          ((eq? (car (cdr aexp)) (quote product)) 
                (and (numbered? (car aexp)) 
                     (numbered? (car (cdr (cdr aexp)))))))))
        
(define numbered2?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          (else (and (numbered2? (car aexp))
                     (numbered2? (car (cdr (cdr aexp)))))))))
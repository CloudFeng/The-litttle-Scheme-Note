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
  
(define first
  (lambda (pair)
    (car pair)))
  
(define second
  (lambda (pair)
    (car (cdr pair))))

(define build
  (lambda (n m)
    (cons n
          (cons m nil))))
(define third
  (lambda (lat)
    (car (cdr (cdr lat)))))
  
(define new-entry build)

(define x '(appetizer entree beverage))
(define y '(pate boeuf vin))

;(new-entry x y)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
      (cond ((null? names) (entry-f name))
          ((eq? (car names) name) (car values))
          (else (lookup-in-entry-help name 
                                      (cdr names) 
                                      (cdr values) 
                                      entry-f)))))

(define entry '((appetizer entree beverage) (food tastes good)))
;(lookup-in-entry (quote entree) entry (lambda (x) (display x) (display " cannot find its value")))

;(lookup-in-entry (quote dessert) entry (lambda (x) (display x) (display " cannot find its value") (newline)))

(define extend-tabls cons)

(define a-table '(((appetizer entree beverage) (pate boeuf vin)) ((beverage dessert) ((food is) (number one with us)))))

;(extend-tabls entry a-table)

(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table) (table-f name))
          (else (lookup-in-entry name 
                                 (car table) 
                                 (lambda (x) (lookup-in-table x 
                                                              (cdr table) 
                                                              table-f)))))))
;(lookup-in-table (quote dessert) a-table (lambda (x) (display "cannot find ") (display name) (display "'s value in table")))

(define atom-to-action
  (lambda (e)
    (display "atom-to-action: ")
    (display e)
    (newline)
    (cond ((or (number? e)
               (eq? e #t)
               (eq? e #f)
               (eq? (quote car) e)
               (eq? (quote cdr) e)
               (eq? (quote cons) e)
               (eq? (quote null?) e)
               (eq? (quote eq?) e)
               (eq? (quote atom?) e)
               (eq? (quote zero?) e)
               (eq? (quote add1) e)
               (eq? (quote sub1) e)
               (eq? (quote number?) e)) *const)
          (else *identifier))))

(define list-to-action
  (lambda (e)
    (display e)
    (newline)
    (display (car e))
    (cond ((atom? (car e))
                  (cond ((eq? (car e) (quote quote)) *quote)
                        ((eq? (car e) (quote lambda)) *lambda)
                        ((eq? (car e) (quote cond)) *cond)
                        (else *application)))
          (else *application))))

(define expression-to-action
  (lambda (e)
    (cond ((atom? e) (atom-to-action e))
          (else (list-to-action e)))))

(define value
  (lambda (e)
    (meaning e (quote ()))))
  
(define meaning
  (lambda (e table)
    (display "meaning ")
    (display e)
    (newline)
    ((expression-to-action e) e table)))


(define *const
  (lambda (e table)
    (cond ((or (eq? e #t) (eq? e #f) (number? e)) e)
          (else (build (quote primitive) e)))))

(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e)))
  
;(value (quote (quote (a b))))

(define initial-table
  (lambda (name)
    (car (quote ()))))
  
(define *identifier
  (lambda (e table)
    (display "*identifier:")
    (display e)
    (newline)
    (lookup-in-table e table initial-table)))


(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

       
(define table-of first)

(define formals-of second)

(define body-of third)

(define lambda-info
  (lambda (x)
    (display "lambda expression:")
    (display x)
    (newline)
    (display "table:")
    (display (table-of (car (cdr x))))
    (newline)
    (display "formals:")
    (display (formals-of (car (cdr x))))
    (newline)
    (display "body:")
    (display (body-of (car (cdr x))))
    (newline)))
  
;(lambda-info (meaning (quote (lambda (x) (cons x y))) '(((y z) ((8) 9)))))

(define else?
  (lambda (x)
    (cond ((atom? x) (eq? x (quote else)))
          (else #f))))

(define question-of first)
(define answer-of second)
(define cond-lines-of cdr)

(define evcon
  (lambda (lines table)
    (display lines)
    (newline)
    (cond ((else? (question-of (car lines)))
                  (meaning (answer-of (car lines))))
          ((meaning (question-of (car lines)) table)
                  (meaning (answer-of (car lines)) table))
          (else (evcon (cdr lines) table)))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
  
;(*cond '(cond (coffee klatsch) (else party)) '(((coffee) (#t))
                                              ;((klatsch party) (5 (6)))))
                                  
(define evlis
  (lambda (args table)
    (cond ((null? args) nil)
          (else (cons (meaning (car args) table)
                      (evlis (cdr args) table))))))

(define function-of car)

(define arguments-of cdr)

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
           (evlis (arguments-of e) table))))

;(primitive primitive-name)
;(non-primitive (table formals body))
;closure:(table formals body)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))


(define apply
  (lambda (fun vals)
    (cond ((primitive? fun) (apply-primitive (second fun) 
                                             vals))
          ((non-primitive fun)
                          (apply-closure (second fun)
                                         vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond ((eq? name (quote cons))
                (cons (first vals) 
                      (second vals)))
          ((eq? name (quote car))
                (car (first vals)))
          ((eq? name (quote cdr))
                (cdr (first vals)))
          ((eq? name (quote null?))
                (null? (first vals)))
          ((eq? name (quote eq?))
                (eq? (first vals)
                     (second vals)))
          ((eq? name (quote atom?))
                (:atom? (first vals)))
          ((eq? name (quote zero?))
                (zero? (first vals)))
          ((eq? name (quote add1))
                (add1 (first vals)))
          ((eq? name (quote sub1))
                (sub1 (first vals)))
          ((eq? name (quote number?))
                (number? (first vals))))))

(define :atom?
  (lambda (x)
    ((atom? x) #t)
    ((null? x) #f)
    ((eq? (car x) (quote primitive)) #t)
    ((eq? (car x) (quote non-primitive)) #t)
    (else #f)))
  
  
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-tabls (new-entry (formals-of closure)
                                      vals)
                            (table-of closure)))))
                          
(define closure '(
                  (((u v w) (1 2 3))
                    ((x y z) (4 5 6)))
                  (x y)
                  (cons z x)))

(define vals '((a b c) (d e f)))

(apply-closure closure vals)

#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence x y z)
    (if (<= x  y)
     (cons x (sequence  (+ x z) y z))
            null))

(define (string-append-map xs x)
  (map (lambda (i) (string-append i x)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]));; remainder 


(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1))))) ;; rememeber this
        
 
(define funny-number-stream
   (letrec ([f (lambda (x) (if (= (remainder x 5) 0) ;;remainder
                               (cons (- x) (lambda () (f (+ x 1))))
                               (cons x (lambda () (f (+ x 1))))))])
      (lambda () (f 1))))

(define dan-then-dog
     (letrec ([f (lambda (x) (if (= (remainder x 2) 0)
                               (cons "dog.jpg" (lambda () (f (+ x 1))))
                               (cons "dan.jpg" (lambda () (f (+ x 1))))))])
      (lambda () (f 1))))


(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s)))))) ;; use (s) to call, (cdr (s))


(define (cycle-lists xs ys)
 (letrec ([f (lambda (x) (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (lambda () (f (+ x 1)))))])
  (lambda () (f 0)))) 

(define (vector-assoc v vec)
  (letrec ([f (lambda (x)
                (cond [(>= x (vector-length vec)) #f]
                      [(and (pair? (vector-ref vec x)) (equal? (car (vector-ref vec x)) v)) (vector-ref vec x)] ;; pair?
                      [#t (f (+ x 1))]))])
    (f 0)))
  

(define (cached-assoc xs n)  ;; .....
  (letrec ([memo (make-vector n #f)]
           [index 0])
    (lambda (v)
      (if  (vector-assoc v memo)
           (vector-assoc v memo)
          (let ([cur-val (assoc v xs)])
            (begin
              (vector-set! memo index cur-val)
              (set! index
                    (remainder (+ index 1) n))
              (vector-assoc v memo)))))))


















  
  
  
  
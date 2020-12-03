#lang racket
(provide (all-defined-out))

(define (sequence low high stride)
  (if (< high low)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (and (> n 0) s)
      (let ([v (s)])
        (cons (car v)
              (stream-for-n-steps (cdr v) (- n 1))))
      null))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons
                 (if (= 0 (remainder x 5)) (* -1 x) x)
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
    (letrec ([f (lambda (x)
                  (cons
                   (if (= 0 x) "dan.jpg" "dog.jpg")
                   (lambda () (f (- 1 x)))))])
      (lambda () (f 0))))

(define (stream-add-zero s)
  (lambda ()
    (let ([sv (s)])
      (if sv
          (let ([v (car sv)] [f (cdr sv)])
            (cons
             (cons 0 v)
             (lambda () ((stream-add-zero f)))))
          null))))

(define (cycle-lists xs1 xs2)
  (letrec ([f (lambda (i)
                (cons
                 (cons
                  (list-nth-mod xs1 i)
                  (list-nth-mod xs2 i))
                 (lambda () (f (+ i 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [search (lambda (i)
                     (if (>= i l)
                         #f
                         (let ([vi (vector-ref vec i)])
                           (if (and (pair? vi) (equal? v (car vi)))
                               vi
                               (search (+ 1 i))))))])
    (search 0)))
                
(define (cached-assoc xs n)
  (let*([memo (make-vector n #f)]
        [i 0]
        [f (lambda (v)
             (let ([x (vector-assoc v memo)])
               (if x 
                   x
                   (let ([x-v (assoc v xs)])
                     (begin 
                       (vector-set! memo i x-v)
                       (set! i (if (>= i (- n 1)) 0 (+ i 1)))
                       x-v)))))])
    f))

(define-syntax while-less 
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([ea e1]
              [f (lambda (x)
                   (if (<= ea x)
                       #t
                       (f e2)))])
       (f e2))]))

#lang racket

(provide (all-defined-out))


;;1
(define (sequence l h s)
  (cond [(< h l) null]
        [#t (cons l (sequence (+ l s) h s))]))

;;2
(define (string-append-map xs s) (map (lambda (x) (string-append x s)) xs))

;;3
(define (list-nth-mod xs n)
  (letrec ([get_nth (lambda (xs n) (if (= n 0) (car xs) (get_nth (cdr xs) (- n 1))))])
    (cond [(null? xs) (error "list-nth-mod: empty list")]
          [(< n 0) (error "list-nth-mod: negative number")]
          [#t (get_nth xs (remainder n (length xs) ))])))
;;4
(define (stream-maker fn arg)
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))

(define nats (stream-maker + 1))
(define multiples-of-3 (stream-maker + 3))
(define (print-n-from-stream stream n)
  (if (> n 0) (begin (print (car (stream))) (print " ") (print-n-from-stream (cdr (stream)) (- n 1))) (print "end")))

(define (stream-for-n-steps s n)
  (let ([pr (s)])
    (cond [(<= n 0) null]
          [#t (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1)))])))

;;5
(define funny-number-stream
  (letrec ([negate-if-dv-by-5 (lambda (x) (if (= 0 (remainder x 5)) (- x)  x ))]
           [f (lambda (x) (cons (negate-if-dv-by-5 x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;;6
;(define dan-then-dog (stream-maker (lambda (d1 d2) (if (string=? d1 "dan.jpg" ) "dog.jpg" d2)) "dan.jpg"))

(define dan-then-dog
    (letrec ([v (vector "dan.jpg" "dog.jpg")]
             [f (lambda (x) (cons (vector-ref v (remainder x 2)) (lambda () (f (+ x 1)))))])
      (lambda () (f 0))))

;;7
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
    (lambda () (f s))))


;;8
;;given a list, create a stream out of it
(define (cycle-lists xs ys)
  ;;convert list to vector, then gen stream from vec via mod indexing
  (define (gen-stream xs)
    (letrec ([v_xs (list->vector xs)]
             [l (vector-length v_xs)]
             [f (lambda (x) (cons (vector-ref v_xs x) (lambda () (f (remainder (+ x 1) l)))))])
      (lambda () (f 0))))
  ;;pair 2 streams
  (define (pair-streams s1 s2)
    (letrec ([f (lambda (s1 s2) (cons (cons (car (s1)) (car (s2))) (lambda () (f (cdr (s1)) (cdr (s2))))))])
      (lambda () (f s1 s2))))
  ;;compose
  (pair-streams (gen-stream xs) (gen-stream ys)))

;;9


(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [get-match (lambda (i)
                        (if (<= l i)
                            #f
                            (let ([w (vector-ref vec i)])
                              (cond  [(and (pair? w) (equal? v (car w))) w]
                                     [#t (get-match (+ i 1))]))))])
    (get-match 0)))


;;10

(define (cached-assoc xs n)
  (let* ([cache (make-vector n #f)]
         [i 0]
         [add-to-cache (lambda (pr) (if (pair? pr)
                                        (begin (vector-set! cache i pr) (set! i (remainder (+ i 1) n)) pr)
                                        pr))])
    (lambda (v) (let ([pr (vector-assoc v cache)]) (if (pair? pr)
                                                     pr
                                                     (let ([pr_l (assoc v xs)]) (add-to-cache pr_l) ))))))


;;(define (cached-assoc-2 xs n)
;;  (let* ([cache (make-vector n #f)]
;;         [i 0]
;;         [add-to-cache (lambda (pr) (begin (vector-set! cache i pr) (set! i (remainder (+ i 1) n)) ))]
;;         [get-from-cache (lambda (v) (vector-assoc v cache))])
;;    (lambda (v) (let ([pr (get-from-cache v)])
;;                  (cond [pr (begin (print "from_cache") pr)]
;;                        [#t (let ([pr_l (assoc v xs)])
;;                              (cond [pr_l (begin (print "from list")(add-to-cache pr_l) pr_l)]
;;                                    [#t (begin (print "not anywhere") #f)]))])))))










(struct add (e1 e2) #:transparent )






























































;;end

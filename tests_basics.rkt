#lang racket


(require rackunit)
(require "basics.rkt")

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"

   ; sequence test
   (test-case "Sequence test"
              (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5 ) )
              (check-equal? (sequence 1 1 1) (list 1))
              (check-equal? (sequence 5 3 1) '()))

   ; string-append-map test
   (test-case "string-append-map test"
              (check-equal? (string-append-map
                  (list "dan" "dog" "curry" "dog2")
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") )
              (check-equal? (string-append-map '() "suffix") '())
              (check-equal? (string-append-map (list "a" "b") "") (list "a" "b")))

   ; list-nth-mod test
   (test-case "list-nth-mod test"
              (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 )
              )

   ; stream-for-n-steps test
   (test-case "stream-for-n-steps test"
              (check-equal? (stream-for-n-steps ones 2) (list 1 1) )
              )


   ; funny-number-stream test
   (test-case "funny-number-stream test"
              (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) )
              )


   ; dan-then-dog test
   (test-case "dan-then-dog test"
              (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") )
              (check-equal? (stream-for-n-steps dan-then-dog 3) (list "dan.jpg" "dog.jpg" "dan.jpg") )
             )



   ; stream-add-zero test
   (test-case "stream-add-zero test"
              (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) )
              (check-equal? (stream-for-n-steps (stream-add-zero ones) 0) '() )
              (check-equal? (stream-for-n-steps (stream-add-zero ones) 5) '((0 . 1) (0 . 1) (0 . 1) (0 . 1) (0 . 1)) )
             )


   ; cycle-lists test
   (test-case "cycle-lists test"
              (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")))
             )


   ; vector-assoc test
   (test-case "vector-assoc test"
              (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1))
              (check-equal? (vector-assoc 4 #()) #f)
              (check-equal? (vector-assoc 4 (vector 1 2 3 4)) #f)
              (check-equal? (vector-assoc 4 (vector 1 2 3 (cons 4 10))) (cons 4 10))
              )

   ; cached-assoc tests
   (test-case "cached-assoc test"
              (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4))
              )

                #|
   ; while-less test
   (check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")
  |#
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)

#lang racket

(require "MUPL_evaluator.rkt")


(require rackunit)

(define tests
  (test-suite
   "Sample tests"

   ;;racketlist->mupllist test
   (test-case "racketlist->mupllist test" (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit)))))

   ;;check mupllist to racketlist with normal list
   (test-case "mupllist->racketlist test" (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4))))

   ;;check that values evaluate to themselves
   (test-case "testing evaluate for values as per part 2"
              (check-equal? (eval-exp (int 4)) (int 4))
              (check-equal? (eval-exp (aunit)) (aunit))
              (check-equal? (eval-exp (apair (add (int 4) (int 5)) (aunit) )) (apair (int 9) (aunit))))

   ;;check simple variable lookup
   (test-case "basic variable lookup"
              (check-equal? (eval-under-env (var "x") (list (cons "x" (int 5)))) (int 5)))
   ;;call test
   (test-case "call test"
              (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) )
               )

   ;;isaunit test
   (test-case "isaunit test"
              (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0)
                            ))

   ;;ifgreater test
   (test-case "ifgreater test"
              (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) )
               )

   ;;mlet test
   (test-case "mlet test"
              (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) )
                  )

   ;;snd test
   (test-case "snd test"
              (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2)
               ))


   ;;ifaunit test
   (test-case "ifaunit test"
              (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3))
               )

   ;;mlet* test
   (test-case "mlet* test"
              (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 20))) (add (var "x") (var "y")))) (int 30))
               )

   ;;ifeq test
   (test-case "ifeq test"
              (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4))
               )

   ;;mupl-map test
   (test-case "mupl-map test"
              (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 79))) ) (apair (int 1) (aunit)) )  )
                             (apair (int 80) (aunit)))
               )

   ;;combined test
   (test-case "combined test"
              (check-equal? (mupllist->racketlist
                             (eval-exp (call (call mupl-mapAddN (int 7))
                                             (racketlist->mupllist
                                              (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
              )

   ;testing MUPL higher order functions use
   (test-case "higher order functions"
              ;;function that takes in a function and applies it to (int 10)
              (check-equal? (eval-exp (call (fun #f "f_int" (call (var "f_int") (int 10) )) (fun #f "n" (add (int 100) (var "n"))))) (int 110))

              ;;function that returns a function that's closure on the argument given to the outer function
              (check-equal? (eval-exp (call (fun #f "x" (fun #f "y" (add (var "x") (var "y")))) (int 5)))
                            (closure (list (cons "x" (int 5))) (fun #f "y" (add (var "x") (var "y")))))
              ;;extending above example, curried add
              (check-equal? (eval-exp (call (call (fun #f "x" (fun #f "y" (add (var "x") (var "y")))) (int 5)) (int 6)))
                            (int 11))
              ;;trying out recursion, recursive add of numbers till 0,
              (check-equal? (eval-exp (call (fun "desc_add" "n" (ifgreater (var "n") (int 0) (add (call (var "desc_add") (add (var "n") (int -1))) (var "n")) (var "n"))) (int 100)))
                            (int 5050)))

   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct product (e1 e2) #:transparent)
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make mupla new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Warm-up
(define (racketlist->mupllist xs)
  (cond [(null? xs) (aunit)]
        [#t (apair (car xs) (racketlist->mupllist (cdr xs)))]))

(define (mupllist->racketlist ms)
  (cond [(aunit? ms) null]
        [#t (cons (apair-e1 ms) (mupllist->racketlist (apair-e2 ms)))]))


;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))


(define (eval-under-env e env)
  (cond
    ;;evaluate var
    [(var? e)
     (envlookup env (var-string e))]
    ;;evaluate add expression
    [(add? e)
     (let ([v1 (eval-under-env (add-e1 e) env)]
           [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (int (+ (int-num v1)
                   (int-num v2)))
           (error "MUPL addition applied to non-number")))]
    ;;evaluate values -  just return the values
    [(or (int? e) (aunit? e) (closure? e)) e]

    ;;evaluate pairs
    [(apair? e) (let ([v1 (eval-under-env (apair-e1 e) env)]
                      [v2 (eval-under-env (apair-e2 e) env)])
                  (apair v1 v2))]

    [(call? e) (let ([fc  (eval-under-env (call-funexp e) env)]
                     [arg (eval-under-env (call-actual e) env)])
                 (if (closure? fc)
                     (eval-mupl-func-call fc arg)
                     (error "MUPL arg to call not function closure")))]

    ;;evaluate a function to a closure ie pair of function and env in which function is defined
    ;;lexical scoping
    [(fun? e) (let* ([f_id (fun-nameopt e)]
                     [fc (cond [ (or (string? f_id) ( eq? #f f_id) )  (closure env e) ]
                              [#t (error "MUPL invalid function id")])])
                fc)]


    [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env))
                      (int 1)
                      (int 0))]

    [(ifgreater? e) (let ([v1 (eval-under-env (ifgreater-e1 e) env )]
                          [v2 (eval-under-env (ifgreater-e2 e) env )])
                      (cond [(and (int? v1) (int? v2)) (if (> (int-num v1) (int-num v2))
                                                           (eval-under-env (ifgreater-e3 e) env)
                                                           (eval-under-env (ifgreater-e4 e) env))]
                            [#t (error "MUPL non int arguments applied to ifgreater")]))]

    [(fst? e) (let ([v (eval-under-env (fst-e e) env)])
                (if (apair? v)
                    (apair-e1 v)
                    (error "MUPL expression for fst does not evaluate to pair")))]

    [(snd? e) (let ([v (eval-under-env (snd-e e) env)])
                (if (apair? v)
                    (apair-e2 v)
                    (error "MUPL expression for snd does not evaluate to pair")))]


    [(mlet? e) (let ([val (eval-under-env (mlet-e e) env)]
                     [id (mlet-var e)])
                 (if (string? id)
                     (eval-under-env (mlet-body e) (extend-env env id val))
                     (error "MUPL invalid var identifier")))]

    ;; END
    [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp e)
  (eval-under-env e null))

;;adding "macro"
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [#t (let ([id  (car (car lstlst))]
                  [exp (cdr (car lstlst))])
              (if (null? (cdr lstlst))
                  (mlet id exp e2)
                  (mlet id exp (mlet* (cdr lstlst) e2))))]))


(define (ifeq e1 e2 e3 e4) (mlet "_x" e1 (mlet "_y" e2 (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3)))))


(define mupl-map
  ;;currying to take in multiple arguments
  ;;fun fx is applied to each element of mupl-list xs
  (fun #f "fx" (fun "map" "xs"
                    (ifaunit (var "xs")
                             (aunit)
                             (apair
                              (call (var "fx")  (fst (var "xs")))
                              (call (var "map") (snd (var "xs"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "i" (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))


;;HELPER FUNCTIONS
(define (extend-env env id val) (cons (cons id val) env))

(define (eval-mupl-func-call fc arg)
  (let* (;;get function
        [f (closure-fun fc)]
        ;;get function_id
        [f_id (fun-nameopt f)]
        ;;extend closure env with func arg
        [c_env (extend-env (closure-env fc) (fun-formal f) arg)])
    ;;if named func, extend closure env with fc to allow for recursion
    (if (string? f_id)
        (eval-under-env (fun-body f) (extend-env c_env f_id fc))
        (eval-under-env (fun-body f) c_env))))
























































































;;end

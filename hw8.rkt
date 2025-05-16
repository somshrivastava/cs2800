#lang lsl

;; Problem 1

;; part p1
(define-struct tv [T F AND OR NOT])

(define-contract (TruthVal A)
  (Struct tv [A
              A
              (-> A A A)
              (-> A A A)
              (-> A A)]))
;; part p1


;; part p1a
(: BOOL-TV (TruthVal Boolean))
(define BOOL-TV (make-tv #t #f (λ (v1 v2) (and v1 v2)) (λ (v1 v2) (or v1 v2)) (λ (v) (not v))))
;; part p1a


;; Problem 2:


;; part p2
(define-contract Variable Natural)

(define-struct n (var))
(define-contract (N X) (Struct n [X]))
;; a negated variable

(define-contract VariableClause (OneOf Variable (N Variable)))
;; a Variable is either a natural number or a negated one

(define-contract (Formula V) (List V))
(define-contract (CNF V) (List (Formula V)))

;; part p2


;; part p2b
(: variable-upper-bound (-> (CNF VariableClause) Variable))
(define (variable-upper-bound cnf)
  (local [(define (max-variable-in-formula formula)
    (foldl (lambda (var acc)
             (if (n? var) 
                 (max acc (n-var var)) 
                 (max acc var))) 
           0 
           formula))]
  
  (foldl (lambda (formula acc) 
           (max acc (max-variable-in-formula formula)))
         0 
         cnf)))



;; Problem 3:

;; part p3
(: eval (All (A) (-> (TruthVal A)
                     (CNF A)
                     A)))
(define (eval api cnf)
  (if (empty? cnf)
      (tv-T api)  
      (local [(define (or-vals formula)
                (foldr (tv-OR api) (tv-F api)  
                       (map (λ (var)
                              (if (n? var)
                                  ((tv-NOT api) var)
                                  var))
                            formula)))]
        (foldr (tv-AND api) (tv-T api) (map or-vals cnf))))) 

(test-suite
 "eval"

 (check-expect (eval BOOL-TV '()) #t)
 (check-expect (eval BOOL-TV (list (list #t #f))) #t)
 (check-expect (eval BOOL-TV (list (list #t #f) (list #f) (list #t #t))) #f)

 )

;; part p3


;; Problem 4:

;; part p4
(: subst (All (A) (-> (TruthVal A)
                      (List (Tuple Variable A))
                      (CNF VariableClause)
                      (CNF A))))
(define (subst api vars cnf)
  (local
      ;;finds truth value for the given variable v 
      [(define (find-var v vars)
         (cond
           [(empty? vars) (tv-F api)]  ;; false if variable not found 
           [(equal? (first (first vars)) v) (second (first vars))]
           [else (find-var v (rest vars))]))

       ;; substitues variables in a clause (used in map class)
       (define (subst-var var)
         (cond
           [(number? var) (find-var var vars)]
           [(n? var) ((tv-NOT api) (find-var (n-var var) vars))]))]
    ;; Main substitution logic
    (map (lambda (clause) (map subst-var clause)) cnf)))

(test-suite 
  "subst"
  ;; Test case for empty CNF
  (check-expect (subst BOOL-TV '() '()) '())

  ;; Test case for CNF with a single variable
  (check-expect (subst BOOL-TV (list (list 1 #t)) (list (list 1))) (list (list #t)))

  ;; Test case for CNF with a negated variable
  (check-expect (subst BOOL-TV (list (list 1 #t) (list 2 #f)) (list (list 1 (make-n 2))))
                (list (list #t ((tv-NOT BOOL-TV) #f))))
)
;; part p4



;; Problem 5:

;; part p5
(: all-tvs (All (X) (-> X
                        X
                        Variable
                        (List (List (Tuple Variable X))))))
(define (all-tvs T F n)
  (local ((define (all-tvs-local n)
          (cond
            [(zero? n) '(())]
            [else
             (let ([xs (all-tvs-local (sub1 n))]) 
               (append (map (lambda (x) (cons (list (sub1 n) T) x)) xs)
                       (map (lambda (x) (cons (list (sub1 n) F) x)) xs)))])))
    (map reverse (all-tvs-local n))))

(test-suite
 "all-tvs"
(check-expect (all-tvs "a" "b" 3)
              '(((0 "a") (1 "a") (2 "a"))
                ((0 "b") (1 "a") (2 "a"))
                ((0 "a") (1 "b") (2 "a"))
                ((0 "b") (1 "b") (2 "a"))
                ((0 "a") (1 "a") (2 "b"))
                ((0 "b") (1 "a") (2 "b"))
                ((0 "a") (1 "b") (2 "b"))
                ((0 "b") (1 "b") (2 "b"))))
              
)

(: sat (All (A) (-> (TruthVal A)
                    (CNF VariableClause)
                    A)))
(define (sat api cnf)
    (cond
      [(empty? cnf) (tv-T api)]
      [(cons? cnf)
       (foldr (lambda (x y) ((tv-OR api) (eval api (subst api x cnf)) y)) (tv-F api) (all-tvs (tv-T api) (tv-F api) (+ 1 (variable-upper-bound cnf))))]
      ))

(test-suite
 "sat"
 ;; Test with empty CNF
 (check-expect (sat BOOL-TV '()) (tv-T BOOL-TV))

 ;; Test with a single clause containing a single variable
 (check-expect (sat BOOL-TV '((1))) (tv-T BOOL-TV))

 ;; Test with a single clause containing a negated variable
 (check-expect (sat BOOL-TV (list (list (make-n 1)))) (tv-T BOOL-TV))

 ;; Test with multiple clauses
 (check-expect (sat BOOL-TV '((1) (2))) (tv-T BOOL-TV))

 ;; Test with a clause containing both a variable and its negation
 (check-expect (sat BOOL-TV (list (list 1 (make-n 1)))) (tv-T BOOL-TV))

 ;; Test with a complex CNF
 (check-expect (sat BOOL-TV (list (list 1 2 (make-n 3)) (list (make-n 1) 2 3))) (tv-T BOOL-TV))

 ;; Test with a CNF that should be unsatisfiable
 (check-expect (sat BOOL-TV (list (list 1) (list (make-n 1)))) (tv-F BOOL-TV))

 ;; Test with invalid CNF structure
 (check-error (sat BOOL-TV 'invalid-cnf))

 ;; Test with invalid variable in CNF
 (check-error (sat BOOL-TV (list (list 'invalid-var))))

 ;; Test with invalid TruthVal API
 (check-error (sat #f '()))

 ;; Test with unexpected input types
 (check-error (sat BOOL-TV (list (list (make-tv 1 2 3 4 5)))))

 ;; Test with a CNF that has repeated variables in a clause
 (check-expect (sat BOOL-TV (list (list 1 1) (list 1))) (tv-T BOOL-TV))

 ;; Test with a CNF that has a large number of variables
 (check-expect (sat BOOL-TV (list (list 1 2 3 4 5 6 7 8 9 10))) (tv-T BOOL-TV))

 ;; Test with a CNF that has a mix of positive and negated variables
 (check-expect (sat BOOL-TV (list (list 1 (make-n 2) 3 (make-n 4) 5))) (tv-T BOOL-TV))

 ;; Test with a CNF that has a single variable repeated multiple times
 (check-expect (sat BOOL-TV (list (list 1 1 1 1 1))) (tv-T BOOL-TV))

 ;; Test with a CNF that has a single negated variable repeated multiple times
 (check-expect (sat BOOL-TV (list (list (make-n 1) (make-n 1) (make-n 1)))) (tv-T BOOL-TV))

 ;; Test to ensure the function handles the case where the CNF is trivially satisfiable
 (check-expect (sat BOOL-TV (list (list 1) (list 2) (list 3))) (tv-T BOOL-TV))

 ;; Test to ensure the function handles the case where the CNF is trivially unsatisfiable
 (check-expect (sat BOOL-TV (list (list 1) (list (make-n 1)) (list 2) (list (make-n 2)))) (tv-F BOOL-TV))

 ;; Test to ensure the function handles the case where the CNF has a large number of clauses
 (check-expect (sat BOOL-TV (list (list 1) (list 2) (list 3) (list 4) (list 5) (list 6) (list 7) (list 8) (list 9) (list 10))) (tv-T BOOL-TV))
)






;; part p5



;; Problem 6:

;; part p6

(: ONEZERO-TV (TruthVal (OneOf (Constant 0) (Constant 1))))
(define ONEZERO-TV
  (make-tv 1
           0
           (lambda (x y) (if (and (= x 1) (= y 1)) 1 0)) ; AND
           (lambda (x y) (if (or (= x 1) (= y 1)) 1 0))  ; OR
           (lambda (x) (if (= x 1) 0 1))))               ; NOT

(: STRING-TV (TruthVal String))
(define STRING-TV
  (make-tv "true"
           "false"
           (lambda (x y) (if (and (string=? x "true") (string=? y "true")) "true" "false")) ; AND
           (lambda (x y) (if (or (string=? x "true") (string=? y "true")) "true" "false"))  ; OR
           (lambda (x) (if (string=? x "true") "false" "true"))))                           ; NOT

(define-struct true ())
(define-struct false ())

(: STRUCT-TV (TruthVal (OneOf (Struct true []) (Struct false []))))
(define STRUCT-TV
  (make-tv (make-true)
           (make-false)
           (lambda (x y) (if (and (true? x) (true? y)) (make-true) (make-false))) ; AND
           (lambda (x y) (if (or (true? x) (true? y)) (make-true) (make-false)))  ; OR
           (lambda (x) (if (true? x) (make-false) (make-true)))))                  ; NOT


(: LAMBDA-TV (TruthVal (All (X) (-> X X X))))
(define LAMBDA-TV
  (make-tv (lambda (x y) x) ; T: selects the first argument
           (lambda (x y) y) ; F: selects the second argument
           (lambda (x y) (x y x)) ; AND: applies x to y and x
           (lambda (x y) (x x y)) ; OR: applies x to x and y
           (lambda (x) (x (lambda (a b) b) (lambda (a b) a))))) ; NOT: swaps arguments
(test-suite
 "sat-other"
 (check-expect (sat ONEZERO-TV (list (list 1 0) (list 3))) 1)

 (check-expect (sat STRING-TV (list (list 1 0) (list 3))) "true")

 (check-expect (sat STRUCT-TV (list (list 1 0) (list 3))) (make-true))

 (check-expect ((sat LAMBDA-TV (list (list 1 0) (list 3))) #true #false) #true)
)
;; part p6

#lang lsl

;; We'll define a tree where each node has a key, value, and two descendents.
(define-struct leaf [])
(define-struct node [k v l r])
(define-contract (Tree K V)
  (OneOf (Struct leaf [])
         (Struct node [K V (Tree K V) (Tree K V)])))

;; The operations that a set should support
(define-struct sop [empty singleton union intersect member?])
(define-contract (Sop Set Elt)
  (Struct sop [Set
               (-> Elt Set)
               (-> Set Set Set)
               (-> Set Set Set)
               (-> Elt Set Boolean)]))

(: tree-keys (All (Set V)
                  (-> (Sop Set String)
                      (Tree String V)
                      Set)))

;; Exercise 1: Implementing sets as lists

(define SOP-LIST (make-sop '()
                            (lambda (s) (explode s))
                            (lambda (l1 l2) (append l1 l2))
                            (lambda (l1 l2) (filter (lambda (s) (member? s l2)) l1))
                            (lambda (s lst) (member? s lst))))

;; Exercise 2: Writing a test

(define T1 (make-node "1" "som" (make-leaf) (make-leaf)))
(define T1-KEYS (list "1"))

(define (t1-prop sop) (equal? (tree-keys sop T1) (T1-KEYS)))

;; Exercise 3: Implementing tree-keys, using the set interface

(define (tree-keys sop t)
   (cond [(leaf? t) (sop-empty sop)]
         [(node? t) ((sop-union sop) ((sop-singleton sop) (node-k t)) ((sop-union sop) ((sop-singleton sop) (node-l t)) ((sop-singleton sop) (node-r t)))))]))

(check-satisfied SOP-LIST t1-prop)

;; Exercise 4: Implementing sets as characteristic functions

;; (define SOP-FUN ...)

;; (check-satisfied SOP-FUN t1-prop)

;; Challenge: If you finished early, come up with a third implementation - maybe a tree?

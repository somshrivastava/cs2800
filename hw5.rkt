#lang lsl

;; Problem 0

;; part p0a
(: left-pad (-> Natural String String))
; adds n characters of whitespace to the left of the input string s
(define (left-pad n s)
  (string-append (implode (build-list n (lambda (_) " "))) s))
;; part p0a


;; part p0b
(: buggy-left-pad (-> Natural String String))
(define (buggy-left-pad n s)
  (string-append (implode (build-list n (lambda (_) "."))) s))
 ;; part p0b


;; part p0c
(: left-pad-spec (-> Natural String True))
(define (left-pad-spec n s)
  (= (string-length (buggy-left-pad n s))
     (+ n (string-length s))))
;; part p0c

;; part p0d
(define BUGGY-INPUT-N 5)
(define BUGGY-INPUT-S "som")
;; part p0d

;; Problem 1:

;; part p1a
(define-contract Bit (OneOf (Constant 0) (Constant 1)))

(define-contract Key (Tuple Bit Bit Bit Bit Bit Bit))

(define-contract Message (Tuple Bit Bit Bit Bit Bit Bit))
;; part p1a

;; part p1b
(: xor (-> Bit Bit Bit))
(define (xor b1 b2)
  (modulo (+ b1 b2) 2))
(check-expect (xor 0 0) 0)
(check-expect (xor 0 1) 1)
(check-expect (xor 1 0) 1)
(check-expect (xor 1 1) 0)

(: xor-list (-> [List Bit] [List Bit] [List Bit]))
(define (xor-list l1 l2)
  (map xor l1 l2))
(check-expect (xor-list (list 1 0 0) (list 1 1 1)) (list 0 1 1))
(check-expect (xor-list (list 0 0 0) (list 0 0 0)) (list 0 0 0))

(: encrypt (-> Message Key Message))
(define encrypt xor-list)

(: decrypt (-> Message Key Message))
(define decrypt xor-list)
;; part p1b

;; part p1c
(: xor-perfect-prop (-> Message Message True))
(define (xor-perfect-prop encr-msg arbitrary-msg)
  (local [(define computed-key (xor-list encr-msg arbitrary-msg))  
          (define decrypted-msg (decrypt encr-msg computed-key))]  
    (equal? decrypted-msg arbitrary-msg))) 

(check-contract xor-perfect-prop)
;; part p1c

;; Problem 2


;; part p2a
(define-contract SBit (OneOf (Constant "0") (Constant "1")))
(define-contract BitString (List SBit))

(define CORRECT-PASSWORD
  (explode "00110011"))

(define-contract Password BitString)

(: password=? (-> Password Password Boolean))
(define (password=? l1 l2)
  (local [(define (loop index mismatch-count)
            (if (= index (length l1))
                (if (= (length l1) (length l2)) (= mismatch-count 0) #f)
                (loop (+ index 1) 
                      (+ mismatch-count 
                         (if (string=? (list-ref l1 index) 
                                       (if (< index (length l2)) (list-ref l2 index) "2"))
                             0 1)))))]  
    (loop 0 0)))

(: check-password (-> Password Boolean))
(define (check-password p)
  (password=? CORRECT-PASSWORD p))

;; part p2a

;; part p2c
(: timing-spec (-> Password Password True))
(define (timing-spec p1 p2)
  (not (distinguishable? (lambda () (check-password p1))
                         (lambda () (check-password p2)))))

(check-expect (timing-spec (explode "0010000") (explode "111111111")) #t)
(check-expect (timing-spec (explode "0011001") (explode "111111111")) #t)
(check-expect (timing-spec (explode "00110011") (explode "000")) #t)

;; part p2c



;; Problem 3:
;;
;; A (height) balanced binary tree has a difference in height of the
;; left and right subtrees of at most 1, where height is the longest path to a leaf. Importantly,
;; this property (or _invariant_) must be mantained when inserting and removing elements
;; from the tree.
;;
;; Your task: define `balanced-prop` as a predicate.
;; You should test your property on several example trees using `check-expect`.
;; You're welcome to use the example trees we provide as some of your tests, but
;; you should also define some of your own.

;; part p3-a
(define-struct leaf [value])
(define-struct node [left right])
(define-contract (Leaf T) (Struct leaf [T]))
(define-contract (Node X Y) (Struct node [X Y]))
(define-contract (Tree X) (OneOf (Leaf X) (Node (Tree X) (Tree X))))
;; part p3-a

;; part p3-b
(define T1 (make-node (make-node (make-leaf 2)
                                 (make-leaf 3))
                      (make-leaf 4)))
;; part p3-b

;; part p3-c
(define T2 (make-node (make-node (make-node (make-leaf 1)
                                            (make-leaf 2))
                                 (make-leaf 3))
                      (make-leaf 4)))
;; part p3-c

;; part p3-d
(define T3 (make-node (make-node (make-leaf 1)
                                 (make-leaf 2))
                      (make-node (make-leaf 3)
                                 (make-leaf 4))))
;; part p3-d
(: tree-height (-> (Tree Any) Integer))
(define (tree-height t)
  (cond
    [(leaf? t) 1]  
    [(node? t) (+ 1 (max (tree-height (node-left t)) 
                          (tree-height (node-right t))))]))  

(: balanced? (-> (Tree Any) Boolean))
(define (balanced? t)
  (cond
    [(leaf? t) #t] 
    [(node? t)
     (and (<= (abs (- (tree-height (node-left t)) (tree-height (node-right t)))) 1) 
          (balanced? (node-left t))  
          (balanced? (node-right t)))]))

(: balanced-prop (-> (Tree Any) True))
(define (balanced-prop t)
  (balanced? t))

(check-expect (balanced-prop T1) #t)
(check-expect (balanced-prop T3) #t)
(check-error (balanced-prop 5)) 
(check-error (balanced-prop '()))
(check-error (balanced-prop "invalid"))

#lang lsl

;; Problem 1

;; Problem 2

;; Formula 1: (P ∧ Q) ∨ ¬R ∨ (Q ∧ ¬R)

(define (p1 P Q R)
  (or (and P Q)
      (not R)
      (and Q (not R))))

(check-expect (p1 #t #t #t) #t)
(check-expect (p1 #t #f #t) #f)
(check-expect (p1 #t #f #f) #t)
(check-expect (p1 #f #t #t) #f)
(check-expect (p1 #f #f #t) #f)
(check-expect (p1 #f #f #f) #t)
(check-expect (p1 #t #t #f) #t)
(check-expect (p1 #t #f #f) #t)

;; Formula 2: ¬(P ∧ Q ∧ R) ∨ (Q ∧ ¬P )

(define (p2 P Q R)
  (or (not (and P Q R))
      (and Q (not P))))

(check-expect (p2 #t #t #t) #f)
(check-expect (p2 #t #f #t) #t)
(check-expect (p2 #t #f #f) #t)
(check-expect (p2 #f #t #t) #t)
(check-expect (p2 #f #f #t) #t)
(check-expect (p2 #f #f #f) #t)
(check-expect (p2 #t #t #f) #t)
(check-expect (p2 #t #f #f) #t)

;; Problem 3

(define PRICE-DB '(8.75 3.25 2.12))
(define MENU-DB '("sandwich" "soup" "coffee"))

(: price-of-order (-> (List String) (OneOf False Real)))
(define (price-of-order order)
  (cond
    [(empty? order) 0]
    [(cons? order)
     (let ([first-price (price-of-item (first order))])
       (and first-price
            (+ first-price (price-of-order (rest order)))))]))

(: price-of-item (-> String (OneOf False Real)))
(define (price-of-item item)
  (foldl (lambda (cur-price cur-item acc)
           (or acc (and (equal? cur-item item) cur-price)))
         #f
         PRICE-DB
         MENU-DB))

(: price-of-order-prop (-> (List String) True))
(define (price-of-order-prop order)
  (let ([result (price-of-order order)])
    (or (not result) (>= result 0))))

(check-contract price-of-order-prop)

;; Describe in your own words what price-of-order-prop is checking:

;; 

;; What’s wrong with using check-contract here?
;; Describe the problem and revise the code to fix it.

;;

;; Problem 4

(define (check-palindrome num) (equal? (explode (number->string num)) (reverse (explode (number->string num))))))

(check-expect (check-palindrome 10) #f)
(check-expect (check-palindrome 2552) #t)
(check-expect (check-palindrome 010) #t)
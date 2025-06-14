#lang lsl

;; Problem 1: 
;;
;; Translate the following expressions in logic into corresponding function definitions. 
;; Note that the number (and names) of arguments may be different for the different 
;; expressions, as they do not all use the same variables.

;; Template (part p1) to fill in:

;; part p1

;; (P /\ Q) \/  ~(R /\ S)
(define (p2 p q r s)
  (or (and p q) (not (and r s))))

;; P -> ~Q
(define (p3 p q)
  (not (and p (not (not q)))))

;; ~(P /\ Q) = ~ P \/ ~Q
(define (p4 p q)
  (equal? (not (and p q)) (or (not p) (not q))))

;; Problem 2:
;;
;; Prove that the two following equalities (these are De Morgan's Laws) hold for all 
;; possible assignments (i.e., are _valid_) by first defining them (p5 and p6) 
;; and then defining their truth tables using check-expect. 
;; Remember to include all possible combinations of inputs!

;; Template (part p2) to fill in:

;; part p2

;; ~(P /\ Q) = ~P \/ ~Q
(define (p5 p q)
  (equal? (not (and p q)) (or (not p) (not q))))
(check-expect (p5 #t #t) #t)
(check-expect (p5 #t #f) #t)
(check-expect (p5 #f #t) #t)
(check-expect (p5 #f #f) #t)

;; ~(P \/ Q) = ~P /\ ~Q
(define (p6 p q)
  (equal? (not (or p q)) (and (not p) (not q))))
(check-expect (p6 #t #t) #t)
(check-expect (p6 #t #f) #t)
(check-expect (p6 #f #t) #t)
(check-expect (p6 #f #f) #t)

;; Problem 3:
;;
;; For each operator, define a version of it in terms of just `if`.
;; You are welcome to validate your encodings using truth-table tests, but 
;; you are not required. 

;; Template (part p3) to fill in:

;; part p3

;; ¬
(define (op_neg p)
  (if p #f #t))

;; /\
(define (op_and p q)
  (if p
      (if q
          #t
          #f)
      #f))

;; \/
(define (op_or p q)
  (if p
      #t
      (if q
          #t
          #f)))

;; ->
(define (op_implies p q)
  (if p
      (if q
          #t
          #f)
      #t))

;; =
(define (op_equal p q)
  (if p
      (if q
          #t
          #f)
      (if q
          #f
          #t)))

;; ⊕ (exclusive or)
(define (op_xor p q)
  (if p
      (if q
          #f
          #t)
      (if q
          #t
          #f)))

;; Problem 4:
;;
;; Perform simplifications to remove redundant variables for the three problems below, and include 
;; truth tables that confirm that your simplifications were correct. We are giving you the
;; expressions written both in logical syntax and in the LSL code that we expect
;; you to simplify & test with.

;; Template (part p4) to fill in:

;; part p4

;; (P /\ Q) /\ (R /\ ~Q)
(define (p9 P Q R)
  (and (and P Q)
       (and R (not Q))))
(define (p9s P Q R) 
  #f)

(check-expect (p9s #t #t #t) #f)
(check-expect (p9s #t #t #f) #f)
(check-expect (p9s #t #f #t) #f)
(check-expect (p9s #t #f #f) #f)
(check-expect (p9s #f #t #t) #f)
(check-expect (p9s #f #t #f) #f)
(check-expect (p9s #f #f #t) #f)
(check-expect (p9s #f #f #f) #f)

;; (P /\ Q /\ P) \/ (Q /\ R)
(define (p10 P Q R)
    (or (and P Q P) 
        (and Q R)))
(define (p10s P Q R)
    (and Q (or P R)))

(check-expect (p10s #t #t #t) #t)
(check-expect (p10s #t #t #f) #t)
(check-expect (p10s #t #f #t) #f)
(check-expect (p10s #t #f #f) #f)
(check-expect (p10s #f #t #t) #t)
(check-expect (p10s #f #t #f) #f)
(check-expect (p10s #f #f #t) #f)
(check-expect (p10s #f #f #f) #f)

;; (P /\ Q /\ R) \/ (~Q /\ S /\ Q)
(define (p11 P Q R S)
    (or (and P Q R)
        (and (not Q) S Q)))
(define (p11s P Q R S)
  (and P Q R))

(check-expect (p11s #t #t #t #t) #t)
(check-expect (p11s #t #t #t #f) #t)
(check-expect (p11s #t #t #f #t) #f)
(check-expect (p11s #t #t #f #f) #f)
(check-expect (p11s #t #f #t #t) #f)
(check-expect (p11s #t #f #t #f) #f)
(check-expect (p11s #t #f #f #t) #f)
(check-expect (p11s #t #f #f #f) #f)
(check-expect (p11s #f #t #t #t) #f)
(check-expect (p11s #f #t #t #f) #f)
(check-expect (p11s #f #t #f #t) #f)
(check-expect (p11s #f #t #f #f) #f)
(check-expect (p11s #f #f #t #t) #f)
(check-expect (p11s #f #f #t #f) #f)
(check-expect (p11s #f #f #f #t) #f)
(check-expect (p11s #f #f #f #f) #f)

;; Problem 5

(: dist (-> Real Real Real))
(define (dist x y)
  (sqrt (+ (sqr x) (sqr y))))
(check-contract dist)

;; Problem 6

(: cube-vol (-> Integer Integer))
(define (cube-vol side-len)
  (expt side-len 3))
(check-contract cube-vol)

;; Problem 7

(: nor (-> Boolean Boolean Boolean)) 
(define (nor p q)
  (not (or p q)))
(check-contract nor)

;; Problem 8

(: string-hide (-> String Integer String))
(define (string-hide str num)
  (if (or (<= num 0) (> num (string-length str)))
      str
      (string-append (substring str 0 (- num 1)) "_" (substring str num)))) 
(check-contract string-hide)
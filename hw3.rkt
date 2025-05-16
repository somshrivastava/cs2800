#lang lsl

(require racket/random)

;; Problem 1

;; common-divisors : Natural Natural -> [List-of Natural]
;; computes all common divisors of two numbers
(: common-divisors (-> Natural Natural [List Natural]))
(define (common-divisors a b)
  (filter (lambda (d) (and (zero? (remainder a d)) (zero? (remainder b d)))) (range 1 (+ 1 (min a b)))))
(check-contract common-divisors)

;; gcd-ref : Natural Nautral -> Natural
;; returns the gcd of two numbers
(: gcd-ref (-> Natural Natural Natural))
(define (gcd-ref a b)
  (if (and (= a 0) (= b 0))
      0
      (if (= a 0)
          b
          (if (= b 0)
              a
              (foldl max 0 (common-divisors a b))))))
(check-contract gcd-ref)

;; gcd : Natural Natural -> Natural
;; calculates GCD using Euclid's algorithm
(: gcd (-> Natural Natural Natural))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(check-contract gcd)

;; gcd-prop : Natural Natural -> Boolean
;; formal specification that ensures that gcd returns the greatest common divisor
(: gcd-prop (-> Natural Natural Boolean))
(define (gcd-prop a b)
  (= (gcd-ref a b) (gcd a b)))
(check-contract gcd-prop)

;; Problem 2

;; find-majority-prop : [List-of Natural] -> Boolean
;; ensures that if find-majority returns a number, it is indeed a majority element
(: find-majority-prop (-> [List Natural] Boolean))
(define (find-majority-prop lon)
  (local
    [(define result (find-majority lon))
     (define (is-majority candidate lon)
       (> (length (filter (lambda (x) (= x candidate)) lon)) (/ (length lon) 2)))]
    (if (equal? result -1)
        #t
        (is-majority result lon))))
(check-contract find-majority-prop)

;; find-majority : [List-of Natural] -> Natural
;; returns a number that exists in more than half the life, else returns -1
(: find-majority (-> [List Natural] Integer))
(define (find-majority lon)
  (local
    [(define (boyer-moore lon candidate count)
       (cond
         [(empty? lon) candidate]
         [(= count 0) (boyer-moore (rest lon) (first lon) 1)]
         [(= (first lon) candidate) (boyer-moore (rest lon) candidate (add1 count))]
         [else (boyer-moore (rest lon) candidate (sub1 count))]))
    (define (is-majority candidate lon)
       (> (length (filter (lambda (x) (= x candidate)) lon)) (/ (length lon) 2)))
    (define candidate (boyer-moore lon -1 0))]
    (if (is-majority candidate lon) candidate -1)))
(check-contract find-majority)

;; Problem 3

;; exclusive-range? : Integer Integer Integer -> Boolean
;; returns true if the number is within the range of high and low, else returns false
(: exclusive-range? (-> Integer Integer Integer Boolean))
(define (exclusive-range? lo hi n)
  (not (or (equal? lo n) (equal? hi n) (> n hi) (< n lo))))
(check-contract exclusive-range?)

;; exclusive-range?-prop : Integer Integer -> Boolean
;; Ensures that exclusive-range? correctly identifies if n is strictly within the range (lo, hi)
(: exclusive-range?-prop (-> Integer Integer True))
(define (exclusive-range?-prop lo hi)
  (if (>= (add1 lo) hi)
      #t  ; If range is invalid, the property holds trivially
      (exclusive-range? lo hi (random (add1 lo) hi))))  ; Generate and validate in one step
(check-contract exclusive-range?-prop)

;; Problem 4

(define-contract Odd (Immediate (check odd?)))

;; double-plus1 : Natural -> Natural
;; takes a number, doubles, it, and adds 1, making it an odd number
(: double-plus1 (-> Odd Odd))
(define (double-plus1 n)
  (add1 (* n 2)))
(check-contract double-plus1)

;; Problem 5

;; divisible-by-3-or-5?: Integer -> Boolean
;; checks if a number is divisble by 3 or 5
(: divisible-by-3-or-5? (-> Integer Boolean))
(define (divisible-by-3-or-5? n)
  (or (= (remainder n 3) 0) (= (remainder n 5) 0)))
(check-contract divisible-by-3-or-5?)

(define-contract Divis3or5
  (Immediate (check divisible-by-3-or-5?)))

;; divide-3-or-5: Integer -> Integer
;; returns a number if it is divisble by 3 or 5 or returns 0 if not
(: divide-3-or-5 (-> Integer Divis3or5))
(define (divide-3-or-5 n)
  (if (divisible-by-3-or-5? n)
      n
      0))
(check-contract divide-3-or-5)
#lang racket

(require lsl/performance)

;; Problem 1

;; part p1a
(define (list-reverse l)
  (define (helper l acc)
    (cond [(empty? l) acc]
          [else (helper (rest l) (cons (first l) acc))]))
  (helper l empty))

(visualize (build-list 10 (lambda (n) (build-list (* 200 n) identity))) list-reverse)

;; part p1a

;; Problem 2

;; part p2a
(define (fib n)
  (define (fib-iter a b count)
    (if (zero? count)
        a
        (fib-iter b (+ a b) (sub1 count))))
  (fib-iter 1 1 n))

(visualize '(0 3 6 9 12 15 18 19 20) fib)
;; part p2a

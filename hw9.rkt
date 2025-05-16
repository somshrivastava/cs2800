#lang lsl


;; Problem 1

;; part p1a
(define-mutable-struct counter (val))
(define-contract (Counter X) (Struct counter [X]))

(: make-counter-1 (-> (-> Natural Natural)))
(define make-counter-1
  (let ([c (make-counter 0)])
    (lambda ()
      (lambda (inc)
        (begin
          (set-counter-val! c (+ inc (counter-val c)))
          (counter-val c))))))

(: make-counter-2 (-> (-> Natural Natural)))
(define make-counter-2
  (lambda ()
    (let ([c (make-counter 0)])
      (lambda (inc)
        (begin
          (set-counter-val! c (+ inc (counter-val c)))
          (counter-val c))))))
;; part p1a

;; part p1b
(: counter-distinguish (-> (-> (-> Natural Natural)) Natural))

(define (counter-distinguish func)
  (let
    ([counter (func)])
     (begin
       (counter 1)
       ((func) 0))))

(check-expect (not (equal? (counter-distinguish make-counter-1)
                           (counter-distinguish make-counter-2)))
              #t)

;; part p1b

;; Problem 2
;; part p2a
(: fast-incr (-> (Counter Natural) (Counter Natural) Natural))
(define (fast-incr c1 c2)
  (begin (set-counter-val! c1 (+ (counter-val c1) 1))
         (set-counter-val! c2 (+ (counter-val c2) 1))
         (+ (counter-val c1) (counter-val c2))))
;; part p2a

;; part p2b
(: fast-incr-prop (-> (Counter Natural) (Counter Natural) True))
(define (fast-incr-prop c1 c2)
  (equal? (+ (counter-val c1) (counter-val c2) 2)
          (fast-incr c1 c2)))
;; part p2b

;; part p2c
(: fast-incr-exercise (-> Natural))
(define (fast-incr-exercise)
  (let ([testCounter (make-counter 0)])
        (fast-incr testCounter testCounter)))

;; part p2c

;; Problem 3

;; part p3a
(: fast-incr-fixed (Function (arguments [c1 (Counter Natural)] [c2 (AllOf (Counter Natural) (lambda (x) (not (eq? x c1))))]) (result  Natural)))
(define (fast-incr-fixed c1 c2)
  (begin (set-counter-val! c1 (+ (counter-val c1) 1))
         (set-counter-val! c2 (+ (counter-val c2) 1))
         (+ (counter-val c1) (counter-val c2))))

;; part p3a

;; Problem 4

;; part p4a
(define-mutable-struct mcons (first rest))
(define-contract (Mcons X Y) (Struct mcons [X Y]))
(define-contract MList (OneOf empty? (Mcons Integer MList)))
;; part p4a


;; part p4b
(define-struct invalid-mlist ())
;; part p4b

(: good-mlist (-> Any Boolean))
(define (good-mlist v)
  (cond [(empty? v) #t]
        [(mcons? v)
         (if (eq? (mcons-rest v) v)
             (raise (make-invalid-mlist))
             (and #t (good-mlist (mcons-rest v))))]))
  


;; part p4c
(: mlength (-> good-mlist Natural))
(define (mlength ml)
  (cond [(empty? ml) 0]
        [(mcons? ml) (add1 (mlength (mcons-rest ml)))]))
;; part p4c

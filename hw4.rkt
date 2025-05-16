#lang lsl

;; Problem 1

;; duplicate-element-prop : [List Integer] -> True
(: duplicate-element-prop (-> [List Integer] True))
(define (duplicate-element-prop lon)
  (local [(define duplicate (duplicate-element lon))]
    (cond
      [(equal? duplicate -1) #t]
      [(> (length (filter (lambda (x) (equal? x duplicate)) lon)) 1) #t]
      [else #f])))
(check-contract duplicate-element-prop)

;; duplicate-element : [List Integer] -> Integer
;; returns a repeated integer, or -1 if there are no duplicates
(: duplicate-element (-> [List Integer] Integer))
(define (duplicate-element lon)
  (local [
          (define sorted-list (sort lon <))
          (define (find-duplicate lon)
            (cond
              [(or (empty? lon) (empty? (rest lon))) -1]
              [(equal? (first lon) (first (rest lon))) (first lon)]
              [else (find-duplicate (rest lon))]))
          ]
    (find-duplicate lon)))
(check-contract duplicate-element)

;; Problem 2

;; common-element-prop : [List [List Integer]] -> True
(: common-element-prop (-> [List [List Integer]] True))
(define (common-element-prop listoflists)
  (local [
          (define common (common-element listoflists))
          (define (check-other-lists otherlists)
            (cond
              [(empty? otherlists) #t]
              [(> (length (filter (lambda (x) (equal? x common)) (first otherlists))) 0) (check-other-lists (rest otherlists))]
              [else #f]))
          ]
    (cond
      [(equal? common -1) #t]
      [(check-other-lists (rest listoflists)) #t]
      [else #f])))
(check-contract common-element-prop)

;; common-element : [List [List Integer]] -> Integer
;; returns a common element among different lists, else returns -1
(: common-element (-> [List [List Integer]] Integer))
(define (common-element listoflists)
  (local [
          (define (is-common n lon)
            (> (length (filter (lambda (x) (equal? x n)) lon)) 0))
          (define (check-other-lists n otherlists)
            (cond
              [(empty? otherlists) n]
              [(is-common n (first otherlists)) (check-other-lists n (rest otherlists))]
              [else -1]))
          (define (check-first-list lon)
            (cond
              [(empty? lon) -1]
              [(> (check-other-lists (first lon) (rest listoflists)) -1) (check-other-lists (first lon) (rest listoflists))]
              [else (check-first-list (rest lon))]))
          ]
    (cond
      [(empty? listoflists) -1]
      [else (check-first-list (first listoflists))])))
(check-contract common-element)

;; Problem 3

;; pair-with-sum-prop : [List Integer] Integer -> True
(: pair-with-sum-prop (-> [List Integer] Integer True))
(define (pair-with-sum-prop lon n)
  (local [(define pair (pair-with-sum lon n))]
    (cond
      [(empty? pair) #t]
      [(and (equal? (+ (first pair) (second pair)) n)
            (> (length (filter (lambda (x) (equal? x (first pair))) lon)) 0)
            (> (length (filter (lambda (x) (equal? x (second pair))) lon)) 0)) #t]
      [else #f])))
(check-contract pair-with-sum-prop)

;; pair-with-sum : [List Integer] Integer -> [List Integer]
;; returns a list with exactly two elements summing to target number, or empty list
(: pair-with-sum (-> [List Integer] Integer [List Integer]))
(define (pair-with-sum lon n)
  (local [
          (define (check-sum el lst)
            (cond
              [(empty? lst) '()]
              [(equal? (+ el (first lst)) n) (list (first lst) el)]
              [else (check-sum el (rest lst))]))
          ]
    (cond
      [(or (empty? lon) (empty? (rest lon))) '()]
      [(not (empty? (check-sum (first lon) (rest lon)))) (check-sum (first lon) (rest lon))]
      [else (check-sum (first (rest lon)) (rest (rest lon)))])))
(check-contract pair-with-sum)
(pair-with-sum '(1 2 3 4 5 6 7) 13)

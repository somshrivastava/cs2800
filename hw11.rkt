#lang lsl

;; part p0
(define-mutable-struct cell (free? value))
(define-contract Cell (Struct cell [Boolean Any]))

(define-mutable-struct memory (pos cells))
(define-contract Memory (Struct memory [Natural (List Cell)]))

(define MEMORY
  (make-memory 0 (build-list 100 (lambda (_) (make-cell #t 0)))))

; helper provided in case you need to know size
(define (memorysize)
  (length (memory-cells MEMORY)))
;; part p0

;; part p0a
(: with-memory (-> Memory (-> Any) Any))
(define (with-memory M thnk)
  (let ([old MEMORY])
    (begin
      (set! MEMORY M)
      (define result (thnk))
      (set! MEMORY old)     
      result)))

;; part p0a

;; Problem 1

;; part p1
(: malloc (-> (Maybe Cell)))
(define (malloc)
  (let* ([pos (memory-pos MEMORY)]
         [cells (memory-cells MEMORY)])
    (if (< pos (length cells))
        (let ([cell (list-ref cells pos)])
          (if (cell-free? cell)
              (begin
                (set-cell-free?! cell #f)
                (set-memory-pos! MEMORY (find-next-free (add1 pos) cells))
                cell)
              (scan-for-free cells)))
        (scan-for-free cells))))

;; Helper to find the next free index after pos
(: find-next-free (-> Natural (List Cell) Natural))
(define (find-next-free start cells)
  (cond
    [(>= start (length cells)) start]
    [(cell-free? (list-ref cells start)) start]
    [else (find-next-free (add1 start) cells)]))

;; Fallback scan if pos isn't valid or cell at pos is not free
(: scan-for-free (-> (List Cell) (Maybe Cell)))
(define (scan-for-free cells)
  (scan-loop 0 cells))

(: scan-loop (-> Natural (List Cell) (Maybe Cell)))
(define (scan-loop i cells)
  (cond
    [(>= i (length cells)) #f]
    [(cell-free? (list-ref cells i))
     (begin
       (define cell (list-ref cells i))
       (set-cell-free?! cell #f)
       (set-memory-pos! MEMORY (find-next-free (add1 i) cells))
       cell)]
    [else (scan-loop (add1 i) cells)]))

;; part p1


;; Problem 2

;; part p2
(: free (-> Cell False))
(define (free c)
  (begin
    (set-cell-free?! c #t)
    #f))

;; part p2

;; Problem 3

;; part p3
(: defrag (-> False))
(define (defrag)
  (let* ([cells (memory-cells MEMORY)]
         [used (filter (lambda (c) (not (cell-free? c))) cells)]
         [free (filter cell-free? cells)]
         [new-cells (append used free)]
         [new-pos (length used)])
    (begin
      (set-memory-cells! MEMORY new-cells)
      (set-memory-pos! MEMORY new-pos)
      #f)))


;; part p3


;; Problem 4

;; part p4a
(define (*= c v)
  (set-cell-value! c v))

(define (deref c) (cell-value c))

(: for-loop (-> Cell Natural (-> Any) False))
(define (for-loop idx bound body)
  (if (>= (cell-value idx) bound)
      #f
      (begin (body)
             (set-cell-value! idx (add1 (cell-value idx)))
             (for-loop idx bound body))))
;; part p4a


;; part p4b
(: sum (-> Natural (Maybe Natural)))
(define (sum n)
  (let ([idx (malloc)])
    (if (not idx)
        #f
        (let ([acc (malloc)])
          (if (not acc)
              (begin
                (free idx)
                #f)
              (begin
                (*= idx 0)
                (*= acc 0)
                (for-loop idx n
                          (lambda ()
                            (*= acc (+ (deref acc) (deref idx)))))
                (let ([result (deref acc)])
                  (begin
                    (free idx)
                    (free acc)
                    result))))))))

;; part p4b


;; Problem 5
;;
;; Add contracts above, no code down here.

;; Problem 6

;; part p6

(: leak-free? (-> (-> Any) Boolean))
(define (leak-free? thnk)
  (let* ([before (length (filter cell-free? (memory-cells MEMORY)))])
    (begin
      (thnk)
      (let ([after (length (filter cell-free? (memory-cells MEMORY)))])
        (= before after)))))

;; part p6

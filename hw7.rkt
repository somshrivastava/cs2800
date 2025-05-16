#lang lsl

;; part p1a
(define EMPTY 'empty)
(define WALL 'wall)
(define PLAYER 'player)
(define EXIT 'exit)

(define-contract Cell (OneOf (Constant EMPTY)
                             (Constant WALL)
                             (Constant PLAYER)
                             (Constant EXIT)))
;; part p1a


;; part p1b
(define-struct posn (x y))
(define-struct at (c pos))

(define-contract (At X Y) (Struct at [X Y]))
(define-contract Posn (Struct posn [Natural Natural]))

(define-contract Maze (List (At Cell Posn)))
;; part p1b

;; part p1c
(define-contract SExp (OneOf Symbol
                             Integer
                             Boolean
                             String
                             (List SExp)))
;; part p1c

;; part p3
(define-struct invalid-sexp (sexp))
;; part p3

;; Problem 1
;; 0 - 3 will be all items
;; 1 - 3 is no exit
;; 0 - 2 is no player
;; 1 - 2 is no exit or player
(define CELL-ITEMS (list EXIT WALL EMPTY PLAYER))
(: random-cell (-> Boolean Boolean Cell))
(define (random-cell player? exit?)
  (local [(define r
            (cond
              [(and player? exit?) (random 4)]
              [player? (random 1 4)]
              [exit? (random 3)]
              [else (random 1 3)]))]
    (list-ref CELL-ITEMS r)))

(: random-maze (-> Natural Maze))
(define (random-maze dimension)
  (random-maze-helper 0 0 dimension #t #t))

(: random-maze-helper (-> Natural Natural Natural Boolean Boolean Maze))
(define (random-maze-helper x y dimension player? exit?)
  (cond
    [(= y dimension) '()]
    [(= x dimension) (random-maze-helper 0 (add1 y) dimension player? exit?)]
    [else (local [(define cell (random-cell player? exit?))
                  (define new-player? (and (not (equal? 'player cell)) player?))
                  (define new-exit? (and (not (equal? 'exit cell)) exit?))]
            (cons (make-at cell (make-posn x y))
                  (random-maze-helper (add1 x) y dimension new-player? new-exit?)))]))

(check-expect (length (random-maze 3)) 9)
(check-expect (length (random-maze 4)) 16)
(check-expect (length (random-maze 5)) 25)

;; Problem 2

(: single-player-exit? (-> Maze Boolean))
(define (single-player-exit? m)
  (local
    [(define (single-player-exit?/acc m p e)
       (cond
         [(empty? m) (and (= p 1) (= e 1))]
         [(equal? (at-c (first m)) 'player) (single-player-exit?/acc (rest m) (add1 p) e)]
         [(equal? (at-c (first m)) 'exit) (single-player-exit?/acc (rest m) p (add1 e))]
         [else (single-player-exit?/acc (rest m) p e)]))]
    (single-player-exit?/acc m 0 0)))

(: random-maze-2 (-> Natural Maze))
(define (random-maze-2 dimension)
  (let ([maze (random-maze dimension)])
    (if (single-player-exit? maze)
        maze
        (random-maze-2 dimension))))

(check-expect (length (random-maze-2 3)) 9)
(check-expect (length (random-maze-2 4)) 16)
(check-expect (length (random-maze-2 5)) 25)

;; Problem 3

(: sexp->cell (Function (arguments [sexp SExp])
                        (result Cell)
                        (raises invalid-sexp)))
(define (sexp->cell sexp)
  (cond
    [(equal? 'X sexp) 'wall]
    [(equal? '_ sexp) 'empty]
    [(equal? 'P sexp) 'player]
    [(equal? 'E sexp) 'exit]
    [else (raise (make-invalid-sexp sexp))]))

(: cell->sexp (-> Cell SExp))
(define (cell->sexp cell)
  (cond
    [(equal? 'wall cell) 'X]
    [(equal? 'empty cell) '_]
    [(equal? 'player cell) 'P]
    [(equal? 'exit cell) 'E]))

(check-expect (cell->sexp 'wall) 'X)
(check-expect (cell->sexp 'empty) '_)
(check-expect (cell->sexp 'player) 'P)
(check-expect (cell->sexp 'exit) 'E)

;; Problem 4

(: cell-roundtrip-prop (-> Cell True))
(define (cell-roundtrip-prop cell)
  (equal?
   (sexp->cell (cell->sexp cell))
   cell))

;(check-contract cell-roundtrip-prop)

;; Problem 5

(: sexp->row (-> SExp Integer Integer Maze))
(define (sexp->row sexp x y)
  (if (list? sexp)
      (map (lambda (cell sx) (make-at (sexp->cell cell) (make-posn sx y))) sexp (build-list (length sexp) (lambda (i) (+ x i))))
      (raise (make-invalid-sexp sexp))))

(: sexp->maze-helper (-> SExp Integer Integer Maze))
(define (sexp->maze-helper sexp x y)
  (if (list? sexp)
      (apply append
             (map (lambda (row sy) (sexp->row row 0 sy)) sexp (build-list (length sexp) (lambda (i) (+ y i)))))
      (raise (make-invalid-sexp sexp))))

(: sexp->maze (Function (arguments [sexp SExp])
                        (result Maze)
                        (raises invalid-sexp)))
(define (sexp->maze sexp)
  (if (list? sexp)
      (sexp->maze-helper sexp 0 0)
      (raise (make-invalid-sexp sexp))))

(check-expect (sexp->maze '()) '())

(check-expect (sexp->maze '((P E X)
                            (_ _ X)))
              (list (make-at 'player (make-posn 0 0))
                    (make-at 'exit (make-posn 1 0))
                    (make-at 'wall (make-posn 2 0))
                    (make-at 'empty (make-posn 0 1))
                    (make-at 'empty (make-posn 1 1))
                    (make-at 'wall (make-posn 2 1))))

(check-expect (sexp->maze '((X _ P)
                            (X X E)
                            (_ _ X)))
              (list (make-at 'wall (make-posn 0 0))
                    (make-at 'empty (make-posn 1 0))
                    (make-at 'player (make-posn 2 0))
                    (make-at 'wall (make-posn 0 1))
                    (make-at 'wall (make-posn 1 1))
                    (make-at 'exit (make-posn 2 1))
                    (make-at 'empty (make-posn 0 2))
                    (make-at 'empty (make-posn 1 2))
                    (make-at 'wall (make-posn 2 2))))

;;;;;;;;;;;;;;;;;;;;;;;

(: max-x (-> Maze Natural))
(define (max-x maze)
  (local [(define (max-x/acc maze acc)
            (cond
              [(empty? maze) acc]
              [(cons? maze) (max-x/acc (rest maze)
                                       (max (posn-x (at-pos (first maze))) acc))]))]
    (max-x/acc maze 0)))

(: max-y (-> Maze Natural))
(define (max-y maze)
  (local [(define (max-y/acc maze acc)
            (cond
              [(empty? maze) acc]
              [(cons? maze) (max-y/acc (rest maze)
                                       (max (posn-y (at-pos (first maze))) acc))]))]
    (max-y/acc maze 0)))

(: find-dimension (-> Maze Natural))
(define (find-dimension maze)
  (add1 (max (max-x maze)
             (max-y maze))))

(: find-row (-> Maze Natural Maze))
(define (find-row maze row-num)
  (cond
    [(empty? maze) '()]
    [(cons? maze) (if (= row-num (posn-y (at-pos (first maze))))
                      (cons (first maze)
                            (find-row (rest maze) row-num))
                      (find-row (rest maze) row-num))]))

(: find-cell->sexp (-> Maze Natural SExp))
(define (find-cell->sexp maze col-num)
  (cond
    [(empty? maze) 'X]
    [(cons? maze) (if (= col-num (posn-x (at-pos (first maze))))
                      (cell->sexp (at-c (first maze)))
                      (find-cell->sexp (rest maze) col-num))]))

(: row->sexp (-> Maze Natural SExp))
(define (row->sexp maze dimension)
  (build-list dimension (λ (col) (find-cell->sexp maze col))))
    

(: maze->sexp (-> Maze SExp))
(define (maze->sexp maze)
  (local [(define (maze->sexp-helper maze dimension curr-row)
            (cond
              [(= curr-row dimension) '()]
              [else (cons (row->sexp (find-row maze curr-row) dimension)
                          (maze->sexp-helper maze dimension (add1 curr-row)))]))]
    (cond
      [(empty? maze) '()]
      [(cons? maze) (maze->sexp-helper maze (find-dimension maze) 0)])))

(check-expect (maze->sexp '()) '()) ;; Empty maze should return an empty list

(check-expect (maze->sexp (list 
                           (make-at 'player (make-posn 0 0))
                           (make-at 'exit (make-posn 1 0))
                           (make-at 'wall (make-posn 2 0))
                           (make-at 'empty (make-posn 0 1))
                           (make-at 'empty (make-posn 1 1))
                           (make-at 'wall (make-posn 2 1))))
              '((P E X)
                (_ _ X)
                (X X X)))

(check-expect (maze->sexp (list
                           (make-at 'wall (make-posn 0 0))
                           (make-at 'wall (make-posn 1 0))
                           (make-at 'wall (make-posn 2 0))
                           (make-at 'empty (make-posn 0 1))
                           (make-at 'player (make-posn 1 1))
                           (make-at 'exit (make-posn 2 1))))
              '((X X X)
                (_ P E)
                (X X X)))

;; Problem 6

(: cell=? (-> (At Cell Posn) (At Cell Posn) Boolean))
(define (cell=? c1 c2)
  (and (equal? (at-c c1)
               (at-c c2))
       (= (posn-x (at-pos c1))
          (posn-x (at-pos c2)))
       (= (posn-y (at-pos c1))
          (posn-y (at-pos c2)))))

(: maze-contains (-> Maze (At Cell Posn) Boolean))
(define (maze-contains m c)
  (ormap (λ (x) (cell=? x c)) m))

(: maze-contains-pos (-> Maze (At Cell Posn) Boolean))
(define (maze-contains-pos m c)
  (ormap (λ (x) (and
                 (= (posn-x (at-pos x)) (posn-x (at-pos c)))
                 (= (posn-y (at-pos x)) (posn-y (at-pos c))))) m))

(: maze-subset (-> Maze Maze Boolean))
(define (maze-subset m1 m2)
  (andmap (λ (c) (maze-contains m2 c)) m1))

(: rest-all-wall (-> Maze Maze Boolean))
(define (rest-all-wall m1 m2)
  (andmap
   (λ (c) (equal? 'wall (at-c c)))
   (filter (λ (c) (not (maze-contains m1 c))) m2)))

(: remove-dupes (-> Maze Maze))
(define (remove-dupes maze)
  (cond
    [(empty? maze) '()]
    [(cons? maze) (if (maze-contains-pos (rest maze) (first maze))
                      (remove-dupes (rest maze))
                      (cons (first maze) (remove-dupes (rest maze))))]))

(: maze=? (-> Maze Maze Boolean))
(define (maze=? maze1 maze2)
  (local [(define m1 (remove-dupes maze1))
          (define m2 (remove-dupes maze2))]
    (and (= (find-dimension m1)
            (find-dimension m2))
         (if (>= (length m1) (length m2))
             (and (maze-subset m2 m1)
                  (rest-all-wall m2 m1))
             (and (maze-subset m1 m2)
                  (rest-all-wall m1 m2))))))


(: maze-roundtrip-prop (-> Maze True))
(define (maze-roundtrip-prop maze)
  (maze=? (sexp->maze (maze->sexp maze))
          maze))

;(check-contract maze-roundtrip-prop 15)

;; Problem 7

(: find-player (-> Maze (Maybe (At Cell Posn))))
(define (find-player maze)
  (cond
    [(empty? maze) #f]
    [(cons? maze) (if (equal? 'player (at-c (first maze)))
                      (first maze)
                      (find-player (rest maze)))]))

(: find-exit (-> Maze (Maybe Posn)))
(define (find-exit maze)
  (cond
    [(empty? maze) #f]
    [(cons? maze) (if (equal? 'exit (at-c (first maze)))
                      (at-pos (first maze))
                      (find-exit (rest maze)))]))

(: find-cell (-> Maze Natural Natural (At Cell Posn)))
(define (find-cell maze col-num row-num)
  (cond
    [(empty? maze) (make-at 'wall (make-posn row-num col-num))]
    [(cons? maze) (if (= col-num (posn-x (at-pos (first maze))))
                      (first maze)
                      (find-cell (rest maze) col-num row-num))]))

(: find-all-neighbors (-> Maze Posn (List (At Cell Posn))))
(define (find-all-neighbors maze pos)
  (local [(define max-coord (sub1 (find-dimension maze)))
          (define x (posn-x pos))
          (define y (posn-y pos))
          (define candidates (list (make-posn (add1 x) y)
                                   (make-posn (sub1 x) y)
                                   (make-posn x (add1 y))
                                   (make-posn x (sub1 y))))
          (define (valid-pos? p)
            (and (>= (posn-x p) 0)
                 (<= (posn-x p) max-coord)
                 (>= (posn-y p) 0)
                 (<= (posn-y p) max-coord)))
          (define valid-candidates (filter valid-pos? candidates))]
    (map (λ (p) (find-cell (find-row maze (posn-y p)) (posn-x p) (posn-y p)))
         valid-candidates)))

(: path-exists? (-> Maze Boolean))
(define (path-exists? maze)
  (local [(define (helper visited curr)
            (cond
              [(maze-contains visited curr) #f]
              [(equal? 'wall (at-c curr)) #f]
              [(equal? 'exit (at-c curr)) #t]
              [else (ormap (λ (x) (helper (cons curr visited) x)) (find-all-neighbors maze (at-pos curr)))]))]
    (helper '() (find-player maze))))

(: random-maze-3 (-> Natural Maze))
(define (random-maze-3 dimension)
  (let ([maze (random-maze-2 dimension)])
    (if (path-exists? maze)
        maze
        (random-maze-3 dimension))))

(check-expect (length (random-maze-3 3)) 9)
(check-expect (length (random-maze-3 4)) 16)
(check-expect (length (random-maze-3 5)) 25)

;; Problem 8

(: path-length? (-> Maze (Maybe Natural)))
(define (path-length? maze)
  (local [(define (helper visited curr)
            (cond
              [(maze-contains visited curr) '()]
              [(equal? 'wall (at-c curr)) '()]
              [(equal? 'exit (at-c curr)) (list (length visited))]
              [else 
               (apply append
                      (map (λ (x) (helper (cons curr visited) x))
                           (find-all-neighbors maze (at-pos curr))))]))]
    (if (empty? (helper '() (find-player maze)))
        #f
        (apply min (helper '() (find-player maze))))))

(check-expect (path-length? (list
                             (make-at 'player (make-posn 0 0))
                             (make-at 'exit (make-posn 1 0))
                             (make-at 'wall (make-posn 2 0))
                             (make-at 'wall (make-posn 0 1))
                             (make-at 'empty (make-posn 1 1))
                             (make-at 'wall (make-posn 2 1))
                             (make-at 'wall (make-posn 0 2))
                             (make-at 'empty (make-posn 1 2))
                             (make-at 'empty (make-posn 2 2)))) 1)

(: random-maze-4 (-> Natural Maze))
(define (random-maze-4 dimension)
  (let ([maze (random-maze-3 dimension)])
    (if (> (path-length? maze) dimension)
        maze
        (random-maze-4 dimension))))

;; Student tests for path-length?
;(check-expect (path-length? '()) #f) ;; No maze, should return #f
(check-expect (path-length? (list
                             (make-at 'player (make-posn 0 0))
                             (make-at 'exit (make-posn 1 0))
                             (make-at 'wall (make-posn 2 0))
                             (make-at 'wall (make-posn 0 1))
                             (make-at 'empty (make-posn 1 1))
                             (make-at 'wall (make-posn 2 1))))
              1) ;; Player to exit in 1 step
(check-expect (path-length? (list
                             (make-at 'player (make-posn 0 0))
                             (make-at 'wall (make-posn 1 0))
                             (make-at 'wall (make-posn 0 1))
                             (make-at 'exit (make-posn 2 2))))
              #f) ;; No valid path

;; Student tests for maze->sexp
(check-expect (maze->sexp '()) '()) ;; Empty maze should return empty list
#|
(check-expect (maze->sexp (list (make-at 'player (make-posn 0 0))
                                (make-at 'exit (make-posn 1 0))
                                (make-at 'wall (make-posn 2 0))
                                (make-at 'empty (make-posn 0 1))
                                (make-at 'empty (make-posn 1 1))
                                (make-at 'wall (make-posn 2 1))))
              '((P E X)
                (_ _ X))) ;; Correctly formatted output
|#

;; Student tests for sexp->maze
(check-expect (sexp->maze '()) '()) ;; Empty input should return empty maze
(check-expect (sexp->maze '((P E X)
                            (_ _ X)))
              (list (make-at 'player (make-posn 0 0))
                    (make-at 'exit (make-posn 1 0))
                    (make-at 'wall (make-posn 2 0))
                    (make-at 'empty (make-posn 0 1))
                    (make-at 'empty (make-posn 1 1))
                    (make-at 'wall (make-posn 2 1)))) ;; Correctly converted back to maze

;; Student tests for sexp->cell
(check-expect (sexp->cell 'X) 'wall) ;; Wall should map correctly
(check-expect (sexp->cell '_) 'empty) ;; Empty should map correctly
(check-expect (sexp->cell 'P) 'player) ;; Player should map correctly
(check-expect (sexp->cell 'E) 'exit) ;; Exit should map correctly
(check-error (sexp->cell 'A)) ;; Invalid symbol should raise an error

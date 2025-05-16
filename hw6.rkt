#lang lsl

;; Problem 1

;; part p1a
(define-struct push [num])
(define-struct add [])
(define-struct mul [])
(define-struct sub [])
(define-contract (Push X) (Struct push [X]))
(define-contract Add (Struct add []))
(define-contract Mul (Struct mul []))
(define-contract Sub (Struct sub []))
(define-contract SimpleInstr (OneOf (Push Integer) Add Mul Sub))

(: simple-eval (-> (List Integer) (List SimpleInstr) (List Integer)))
(define (simple-eval stk instrs)
  (local [(: stack-binop (-> [-> Integer Integer Integer] [List Integer]
                             [List SimpleInstr]
                             [List Integer]))
          ; evaluates a binary operator on top two numbers of stack, if present
          (define (stack-binop op stk instrs)
            (if (>= (length stk) 2)
                (simple-eval (cons (op (first stk) (second stk))
                            (rest (rest stk)))
                      instrs)
                (list)))

          (: eval-instr (-> Instr [List Integer] [List SimpleInstr] [List Integer]))
          ; evaluates a single instruction, given a stack and rest of instructions
          (define (eval-instr i stk instrs)
            (cond [(add? i) (stack-binop + stk instrs)]
                  [(mul? i) (stack-binop * stk instrs)]
                  [(sub? i) (stack-binop - stk instrs)]
                  [(push? i) (simple-eval (cons (push-num i) stk) instrs)]))]
    (cond [(empty? instrs) stk]
          [(cons? instrs) (eval-instr (first instrs) stk (rest instrs))])))
;; part p1a

;; part p1b
(: simple-stack-verify (-> (List SimpleInstr) (List SimpleInstr) Boolean))
(define (simple-stack-verify p1 p2)
  (equal? (simple-eval '() p1) (simple-eval '() p2)))

;; part p1b


;; Problem 2

;; part p2
(: simple-const-fold (-> (List SimpleInstr) (List SimpleInstr)))
(define (simple-const-fold p)
  (cond [(< (length p) 3) p]
        [(and (push? (first p)) (push? (second p)) (add? (third p)))
         (cons (make-push (+ (push-num (second p)) (push-num (first p)))) (simple-const-fold (rest (rest (rest p)))))]  ; Skip 3 and continue
        [else (cons (first p) (simple-const-fold (rest p)))]))

;; part p2

;; Problem 3

;; part p3
(: simple-const-fold-prop (-> (List SimpleInstr) True))
(define (simple-const-fold-prop instr)
  (equal? (simple-eval (list 1 2 3 4 5) instr) (simple-eval (list 1 2 3 4 5) (simple-const-fold instr))))

; (check-contract simple-const-fold-prop)

;; part p3

;; Problem 4

;; part p4a
(define-struct var [name])
(define-contract (Var X) (Struct var [X]))
(define-contract Instr (OneOf (Push Integer) Add Mul Sub (Var String)))

(define-struct bind [name value])
(define-contract (Bind X Y) (Struct bind [X Y]))
(define-contract Binding (Bind String Integer))


(: eval (-> (List Binding) (List Integer) (List Instr) (List Integer)))
; will return an empty list if it reaches an unbound variable, or a malformed
; program (trying to do an operation without enough values on stack).
(define (eval env stk instrs)
  (local [(: stack-binop (-> [-> Integer Integer Integer] [List Integer]
                             [List Instr]
                             [List Integer]))
          ; evaluates a binary operator on top two numbers of stack, if present
          (define (stack-binop op stk instrs)
            (if (>= (length stk) 2)
                (eval env
                      (cons (op (first stk) (second stk))
                            (rest (rest stk)))
                      instrs)
                (list)))

          (: lookup-var (-> String [List Binding] [List Integer]
                            [List Instr] [List Integer]))
          (define (lookup-var name env stk instrs)
            (cond [(empty? env) (list)]
                  [(cons? env) (if (equal? name (bind-name (first env)))
                                   (eval env
                                         (cons (bind-value (first env))
                                               stk)
                                         instrs)
                                   (lookup-var name (rest env) stk instrs))]))

          (: eval-instr (-> Instr [List Integer] [List Instr] [List Integer]))
          ; evaluates a single instruction, given a stack and rest of instructions
          (define (eval-instr i stk instrs)
            (cond [(add? i) (stack-binop + stk instrs)]
                  [(mul? i) (stack-binop * stk instrs)]
                  [(sub? i) (stack-binop - stk instrs)]
                  [(push? i) (eval env (cons (push-num i) stk) instrs)]
                  [(var? i) (lookup-var (var-name i) env stk instrs)]))]
    (cond [(empty? instrs) stk]
          [(cons? instrs) (eval-instr (first instrs) stk (rest instrs))])))
;; part p4a

;; Your first task is to first define an updated version of `simple-stack-verify`.
;; This time it will take a substitution (set of variable bindings) that it
;; can pass to `eval`.

;; part p4b
(: stack-verify (-> (List Binding) (List Instr) (List Instr) Boolean))
(define (stack-verify env p1 p2)
  (equal? (eval env '() p1) (eval env '() p2)))

;; part p4b

;; part p4c
(: const-fold (-> (List Instr) (List Instr)))
(define (const-fold p)
  (cond
    [(< (length p) 3) p]
    [(and (push? (first p)) (push? (second p)) (or (add? (third p)) (sub? (third p)) (mul? (third p))))
     (cond 
       [(add? (third p)) (cons (make-push (+ (push-num (second p)) (push-num (first p)))) (const-fold (rest (rest (rest p)))))]
       [(sub? (third p)) (cons (make-push (- (push-num (second p)) (push-num (first p)))) (const-fold (rest (rest (rest p)))))]
       [(mul? (third p)) (cons (make-push (* (push-num (second p)) (push-num (first p)))) (const-fold (rest (rest (rest p)))))])]
    [else (cons (first p) (const-fold (rest p)))]))

;; part p4c

;; part p4d
(: const-fold-prop (-> (List Instr) True))
(define (const-fold-prop instr)
  (stack-verify '() instr (const-fold instr)))

(check-contract const-fold-prop)

;; part p4d


;; Problem 5

;; part p5
(define-struct leaf [value])
(define-struct node [left right])
(define-contract (Leaf X) (Struct leaf [X]))
(define-contract (Node X Y) (Struct node [X Y]))
(define-contract (Tree X) (OneOf (Leaf X) (Node (Tree X) (Tree X))))

(: tree-map (All (A B) (-> (-> A B) (Tree A) (Tree B))))
(define (tree-map f t)
  (cond [(leaf? t) (make-leaf (f (leaf-value t)))]
        [(node? t) (make-node (tree-map f (node-left t))
                              (tree-map f (node-right t)))]))

(check-contract tree-map)
;; part p5


;; Problem 6

;; part p6

(: p6a (All (X) (-> Boolean X X X)))
(define (p6a b x1 x2) (if b x1 x2))

(: p6b (All (X) (-> Boolean X X X)))
(define (p6b b x1 x2) (if b x2 x1))

(: p6c (All (X) (-> Boolean X X X)))
(define (p6c b x1 x2) x1)

(: p6d (All (X) (-> Boolean X X X)))
(define (p6d b x1 x2) x2)

;; part p6



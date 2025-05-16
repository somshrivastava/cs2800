#lang lsl

(define (random-ref 1)
(list-ref l (random (length 1))))
• (define (p n)
(process
(name (string-append "p" (number-›string n)))
(on-start (lambda (_) (action 0 empty) ))
(on- receive
(lambda (st pkt)
(action st
(list
(send-packet (string-append "p" (number-›string (addl n)))
(* 2 (receive-packet-msg pkt)))))))))
15 (start random-ref (list (process (name "init")
(on-start (lambda (_)
(action 0 (list (send-packet "pl" 1)))))
(on- receive (Lambda (st _) (action st empty))))
(p 1)
(p
2)
(p
3)
(p
4)
(p 5)
(process (name "p6")
(on-start (lambda (-) (action 0 empty) ))
(on-receive (lambda (st _) (action st empty) )))))
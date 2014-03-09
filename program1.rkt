#lang racket

;; Choose heuristic function on lines 252-3
;; null-heuristic always return 0
;; heuristic is manhattan distance

(define goal-state '((1 2 3)
                     (4 5 6)
                     (7 8 0)))

(define (state-elem state row col)
  (list-ref (list-ref state row) col))

(define (is-goal? state goalS) (equal? state goalS))

(define (move state)
  (cond
    [(eq? (state-elem state 0 0) 0) (square0 state)]
    [(eq? (state-elem state 0 1) 0) (square1 state)]
    [(eq? (state-elem state 0 2) 0) (square2 state)]
    [(eq? (state-elem state 1 0) 0) (square3 state)]
    [(eq? (state-elem state 1 1) 0) (square4 state)]
    [(eq? (state-elem state 1 2) 0) (square5 state)]
    [(eq? (state-elem state 2 0) 0) (square6 state)]
    [(eq? (state-elem state 2 1) 0) (square7 state)]
    [else (square8 state)] ; in the regular game, no moves would be needed, but we could change game in future for different rules
    ))

; return (State, Action, Weight) for if blank is in top left
(define (square0 state)
  (list (list (list (list (state-elem state 1 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 0 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 1 0) 'U) 1)
        (list (list (list (state-elem state 0 1) (state-elem state 0 0) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 0 1) 'L) 1)
        )
 )

; return (State, Action, Weight) for if blank is in top middle
(define (square1 state)
  (list (list (list (list (state-elem state 0 0) (state-elem state 1 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 0 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              ) 
              (list (state-elem state 1 1) 'U) 1)
        (list (list (list (state-elem state 0 1) (state-elem state 0 0) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 0 0) 'R) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 2) (state-elem state 0 1) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 0 2) 'L) 1)
        )
 )

; return (State, Action, Weight) for if blank is in top right
(define (square2 state)
  (list (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 1 2) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 0 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 1 2) 'U) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 2) (state-elem state 0 1) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 0 1) 'R) 1)
        )
 )

; return (State, Action, Weight) for if blank is in middle left
(define (square3 state)
  (list (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 2 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 1 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 2 0) 'U) 1)
        (list (list (list (state-elem state 1 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 0 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 0 0) 'D) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 1) (state-elem state 1 0) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 1 1) 'L) 1)
        )
 )

; return (State, Action, Weight) for if blank is in middle middle
(define (square4 state)
  (list (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 2 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 1 1) (state-elem state 2 2) )
              )
              (list (state-elem state 2 1) 'U) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 1 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 0 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 0 1) 'D) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 1) (state-elem state 1 0) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 1 0) 'L) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 1 2) (state-elem state 1 1) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 1 2) 'R) 1)
        )
 )

; return (State, Action, Weight) for if blank is in middle right
(define (square5 state)
  (list (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 2 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 1 2) )
              )
              (list (state-elem state 2 2) 'U) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 1 2) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 0 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 0 2) 'D) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 1 2) (state-elem state 1 1) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 1 1) 'R) 1)
        )
  )
 

; return (State, Action, Weight) for if blank is in bottom left
(define (square6 state)
  (list (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 2 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 1 0) (state-elem state 2 1) (state-elem state 2 2) )
              )
              (list (state-elem state 1 0) 'D) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 2 1) (state-elem state 2 0) (state-elem state 2 2) )
              )
              (list (state-elem state 2 1) 'L) 1)
        )
 )

; return (State, Action, Weight) for if blank is in bottom middle
(define (square7 state)
  (list (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 2) (state-elem state 2 1) )
              )
              (list (state-elem state 2 2) 'L) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 2 1) (state-elem state 2 0) (state-elem state 2 2) )
              )
              (list (state-elem state 2 0) 'R) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 2 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 1 1) (state-elem state 2 2) )
              )
              (list (state-elem state 1 1) 'D) 1)
        )
 )

; return (State, Action, Weight) for if blank is in bottom right
(define (square8 state)
  (list (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 2 2) )
                    (list (state-elem state 2 0) (state-elem state 2 1) (state-elem state 1 2) )
              )
              (list (state-elem state 1 0) 'D) 1)
        (list (list (list (state-elem state 0 0) (state-elem state 0 1) (state-elem state 0 2) )
                    (list (state-elem state 1 0) (state-elem state 1 1) (state-elem state 1 2) )
                    (list (state-elem state 2 0) (state-elem state 2 2) (state-elem state 2 1) )
              )
              (list (state-elem state 2 1) 'R) 1)
        )
 )

(define (proper-pos i)
	(cond
	[(eq? i 0) (cons 2 2)]
	[(eq? i 1) (cons 0 0)]
	[(eq? i 2) (cons 0 1)]
	[(eq? i 3) (cons 0 2)]
	[(eq? i 4) (cons 1 0)]
	[(eq? i 5) (cons 1 1)]
	[(eq? i 6) (cons 1 2)]
	[(eq? i 7) (cons 2 0)]
	[else (cons 2 1)]
	)
)

(define (null-heuristic state) 0)

(define (heuristic state)
	(define (manh-dst val pos)
		(if (eq? val 0) 0
		(let* [(correctpos (proper-pos val)) (posx (car pos)) (posy (cdr pos)) (corx (car correctpos)) (cory (cdr correctpos))]
		      (+ (abs (- posx corx)) (abs (- posy cory)))
		))
	)

	(define retVal 0)
	(foldl + 0 (for/list ([i 3])
	     (foldl + 0 (for/list ([j 3])
	     	  (let [(v (state-elem state i j))]
		       (manh-dst v (cons i j))
		  )
	     ))
	))
)

(require data/heap)
(require (lib "trace.ss"))
; For Heap can use:
; (heap-add! <heap> <val1> <val2> ...)
; (heap-count <heap>)
; (heap-min <heap>) ; if empty throws error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For Hash table can use:
; (hash-set! <ht> <key> <elem>)
; (hash-ref <ht> <key>)

(define (tile-puzzle startS goalS)
  (define (print-state state)
  (for ([i 3])
    (for ([j 3])
      (printf "~a " (state-elem state i j))
      )
    (printf "\n")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Choose heuristic function to use               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define heuristic_function heuristic)
  ;(define heuristic_function null-heuristic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (goal? state) (equal? state goal-state))
  (struct node (state pred move g h f))
  (define (node<=? x y)
    (<= (node-f x) (node-f y)))
  (define Q (make-heap node<=?))
  (define ht (make-hash))
  (define counter 0)
  (define (not-in-hash? SAW) 
    (eq? #f (hash-ref ht (get-hash (car SAW)) #f)))
  (define (print-solution anode)
    (cond 
    [(null? anode) ] ; return nothing
    [(null? (node-pred anode)) (list 'start (node-state anode) (node-g anode) (node-h anode) (node-f anode))]
    [else (list (print-solution (node-pred anode)) (list (node-move anode) (node-state anode) (node-g anode) (node-h anode) (node-f anode)))]
    ))
  (define (add-SAW-to-heap SAW prev)
    (let* [(weight (car (cdr (cdr SAW)))) (h (heuristic_function (car SAW))) (action (car (cdr SAW)))]
      (let [(g (if (null? prev) weight (+ weight (node-g prev))))]
        (define f (+ g h))
        (node (car SAW) prev action g h f) 
      )))
  (define (get-hash state) state)
      ;(string (integer->char (state-elem state 0 0)) (integer->char (state-elem state 0 1)) (integer->char (state-elem state 0 2))
      ;        (integer->char (state-elem state 1 0)) (integer->char (state-elem state 1 1)) (integer->char (state-elem state 1 2))
      ;        (integer->char (state-elem state 2 0)) (integer->char (state-elem state 2 1)) (integer->char (state-elem state 2 2)))
      ;)
  
  (define (no-solution counter) (printf "Nodes extracted: ~a\n" counter) "no solution")
  ; begin tile-puzzle

  ; add initial state to the queue
  (let [(h (heuristic_function startS))]
  (heap-add! Q (node startS '() 'start 0 h h)))

  (let loop ()
    (define curr (heap-min Q))
    (hash-set! ht (get-hash (node-state curr)) curr) ; add node to the hash
    (heap-remove-min! Q)                             ; remove the node from the queue
    (set! counter (+ 1 counter))
    (cond 
      [(goal? (node-state curr)) (printf "Nodes extracted: ~a\n" counter) (print-solution curr)]
      [else
       (let ([SAWs (filter not-in-hash? (move (node-state curr)))])
         (define (add-SAW SAW) (add-SAW-to-heap SAW curr)) 
         (heap-add-all! Q (map add-SAW SAWs))
         ) ; end let
       (if (= (heap-count Q) 0) (no-solution counter) (loop))
       ] ; end else
      ) ; loop if the queue is not empty, else we didn't find a solution
    ) 
  )

;(trace tile-puzzle)
(define test1 '((6 4 2)
                (1 5 3)
                (7 0 8)))

(define test2 '((6 4 2)
                (8 5 3)
                (1 0 7)))

(define test3 '((6 4 7)
                (8 5 0)
                (3 2 1)))

(define test4 '((8 0 7)
                (6 5 4)
                (3 2 1)))

(define test5 '((1 2 3)
                (4 5 6)
                (8 7 0)))


(tile-puzzle test1 goal-state)
(tile-puzzle test2 goal-state)
(tile-puzzle test3 goal-state)
(tile-puzzle test4 goal-state)
(tile-puzzle test5 goal-state)

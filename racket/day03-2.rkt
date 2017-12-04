#lang racket/base
(require dyoo-while-loop)

(define up '(0 1))
(define right '(1 0))
(define left '(-1 0))
(define down '(0 -1))
(define upright '(1 1))
(define upleft '(-1 1))
(define downright '(1 -1))
(define downleft '(-1 -1))

(define (move pos dpos)
  (list (+ (car pos) (car dpos))
        (+ (cadr pos) (cadr dpos))))

(define (neighbors pos)
  (list (move pos right)
        (move pos up)
        (move pos left)
        (move pos down)
        (move pos upright)
        (move pos upleft)
        (move pos downright)
        (move pos downleft)))

(define (neighbor-keys ht pos)
  (map (λ (k) (hash-ref ht k 0))
       (neighbors pos)))

(define (sum-neighbor-keys ht pos)
  (apply + (neighbor-keys ht pos)))

(define (coordinates num)
  (let ([x 0]
        [y 0]
        [dx 0]
        [dy -1]
        [curr 0]
        [ht (make-hash '(((0 0) . 1)))])
    (while (< curr num)
           (begin
             (when (or (eq? x y)
                       (and (< x 0) (eq? x (- y)))
                       (and (> x 0) (eq? x (- 1 y))))
               (set!-values (dx dy) (values (- dy) dx)))
             (set!-values (x y) (values (+ x dx) (+ y dy)))
             (letrec ([pos (list x y)]
                      [sum-pos (sum-neighbor-keys ht pos)])
               (hash-set! ht pos sum-pos)
               (set! curr sum-pos)
               )))
    curr))

(coordinates 368078)

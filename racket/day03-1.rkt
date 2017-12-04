#lang racket/base

(define (coordinates num)
  (let ([x 0]
        [y 0]
        [dx 0]
        [dy -1])
    (for/fold ([pos '(0 0)])
              ([i (sub1 num)])
              (begin
                (when (or (eq? x y)
                        (and (< x 0) (eq? x (- y)))
                        (and (> x 0) (eq? x (- 1 y))))
                  (set!-values (dx dy) (values (- dy) dx)))
                (set!-values (x y) (values (+ x dx) (+ y dy))))
              (list x y))))

(define (spiral-distance num)
  (letrec ([coord (coordinates num)]
           [x (car coord)]
           [y (cadr coord)])
    (+ (abs x) (abs y))))

(module+ test
  (require rackunit)
  [check-equal? (coordinates 1) '(0 0)]
  [check-equal? (coordinates 2) '(1 0)]
  [check-equal? (coordinates 3) '(1 1)]
  [check-equal? (coordinates 4) '(0 1)]
  [check-equal? (coordinates 5) '(-1 1)]
  [check-equal? (coordinates 6) '(-1 0)]
  [check-equal? (coordinates 7) '(-1 -1)]
  [check-equal? (coordinates 8) '(0 -1)]
  [check-equal? (coordinates 9) '(1 -1)]
  [check-equal? (coordinates 10) '(2 -1)]
  [check-equal? (spiral-distance 1) 0]
  [check-equal? (spiral-distance 12) 3]
  [check-equal? (spiral-distance 23) 2]
  [check-equal? (spiral-distance 1024) 31])

(spiral-distance 368078)

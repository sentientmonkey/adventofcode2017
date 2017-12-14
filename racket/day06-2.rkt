#lang racket/base
(require racket/string)
(require racket/list)

(define (parse-numbers str)
  (map string->number (string-split str "\t")))

(define (rebalance lst)
  (let* ([maxlst (apply max lst)]
         [maxpos (index-of lst maxlst)]
         [len (length lst)])
    (for/fold ([xlst (list-set lst maxpos 0)])
              ([i maxlst])
              (let ([idx (modulo (+ i maxpos 1) len)])
                (list-update xlst idx add1)))))

(define (realloc-helper lst h idx)
  (let ([nxt (rebalance lst)])
    (if (not (hash-has-key? h nxt))
      (realloc-helper nxt (hash-set h nxt idx) (add1 idx))
      (- idx (hash-ref h nxt)))))

(define (realloc str)
  (realloc-helper (parse-numbers str) (make-immutable-hash) 0))

(module+ test
  (require rackunit)
  [check-equal? (realloc "0\t2\t7\t0") 4])

(realloc "4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5")


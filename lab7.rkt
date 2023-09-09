; Technically the lab didn't specify that these functions must be
; recursive. I assume that recursion was implied though.
#lang racket
(define (count item lst)
  (if (null? lst) 
      0
      (if (equal? item (car lst)) 
          (+ 1 (count item (cdr lst))) 
          (count item (cdr lst)))))

(define (make-list count item)
  (if (= count 0) 
      '() 
      (cons item (make-list (- count 1) item))))

(define (bag-difference bag1 bag2)
  (if (null? bag1) 
      '()
      (let ((count (- (count (car bag1) bag1) (count (car bag1) bag2))))
        (if (> count 0) 
            (cons (car bag1) (bag-difference (cdr bag1) bag2)) 
            (bag-difference (cdr bag1) bag2)))))

(define (remove item lst)
  (if (null? lst)
      '()
      (if (equal? item (car lst))
          (remove item (cdr lst))
          (cons (car lst) (remove item (cdr lst))))))

(define (bag-union bag1 bag2)
  (if (null? bag1)
      bag2
      (let* ((item (car bag1))
             (rest-bag1 (remove item bag1))
             (rest-bag2 (remove item bag2))
             (count1 (count item bag1))
             (count2 (count item bag2))
             (max-count (max count1 count2)))
        (append (make-list max-count item) 
                (bag-union rest-bag1 rest-bag2)))))

(define (bag-intersection bag1 bag2)
  (if (null? bag1)
      '()
      (let* ((item (car bag1))
             (rest-bag1 (remove item bag1))
             (rest-bag2 (remove item bag2))
             (count1 (count item bag1))
             (count2 (count item bag2))
             (min-count (min count1 count2)))
        (if (> min-count 0)
            (append (make-list min-count item) 
                    (bag-intersection rest-bag1 rest-bag2))
            (bag-intersection rest-bag1 rest-bag2)))))


#lang racket

(define (gen-list start end)
  (if (> start end)
      '()
      (cons start (gen-list (+ start 1) end))))

(define (sum lst)
  (cond ((null? lst) 0)
        ((not (pair? lst)) lst)
        (else (+ (car lst) (sum (cdr lst))))))

(define (retrieve-first-n n lst)
  (cond ((or (null? lst) (<= n 0)) '())
        ((= n 1) (list (car lst)))
        (else (cons (car lst) (retrieve-first-n (- n 1) (cdr lst))))))

(define (pair-sum? lst val)
  (cond ((or (null? lst) (null? (cdr lst))) #f)
        ((= (+ (car lst) (cadr lst)) val) #t)
        (else (pair-sum? (cdr lst) val))))

(define (mystery-tail lst acc)
  (cond ((null? lst) acc)
        ((null? (cdr lst)) (cons (car lst) acc))
        (else (mystery-tail (cdr lst) (cons (car lst) acc)))))
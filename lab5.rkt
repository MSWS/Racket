#lang racket

; Problem 1: Reflexive?
(define (Reflexive? L S)
  (cond
    [(null? S) (if (null? L) #t #f)]
    [(member (list (car S) (car S)) L)
     (Reflexive? (remove (list (car S) (car S)) L) (cdr S))]
    [else #f]))

; Problem 2: Symmetric?
(define (Symmetric? L)
  (define (remove-pair l p)
    (cond
      [(null? l) '()]
      [(equal? p (car l)) (cdr l)]
      [else (cons (car l) (remove-pair (cdr l) p))]))

  (define (symmetric-helper l)
    (cond
      [(null? l) #t]
      [(member (reverse (car l)) l)
       (symmetric-helper (remove-pair (remove-pair l (car l)) (reverse (car l))))]
      [else #f]))

  (symmetric-helper L))

; Problem 3: Transitive?
(define (Transitive? L)
  (define (exists? x y L)
    (cond
      [(null? L) #f]
      [(equal? (list x y) (car L)) #t]
      [else (exists? x y (cdr L))]))

  (define (check-transitive L)
    (cond
      [(null? L) #t]
      [(ormap
        (lambda (p1)
          (ormap
            (lambda (p2)
              (and (equal? (cadr p1) (car p2)) (not (exists? (car p1) (cadr p2) L))))
            L))
        L)
       #f]
      [else #t]))

  (check-transitive L))

; Test Cases
(display "Reflexive?\n")
(display (Reflexive? '((a a) (b b) (c c)) '(a b c))) ; ---> #t
(display (Reflexive? '((a a) (b b)) '(a b c))) ; ---> #f
(display (Reflexive? '((a a) (a s) (b b) (c c)) '(a b c))) ; ---> #f
(display (Reflexive? '() '())) ; ---> #t
(display '"\n")

(display "Symmetric?\n")
(display (Symmetric? '((a a) (a b) (b a) (b c) (c b)))) ; ---> #t
(display (Symmetric? '((a a) (a b) (a c) (c a)))) ; ---> #f
(display (Symmetric? '((a a) (b b)))) ; ---> #t
(display (Symmetric? '())) ; ---> #t
(display '"\n")

(display "Transitive?\n")
(display (Transitive? '((a b) (b c) (a c)))) ; ---> #t
(display (Transitive? '((a a) (b b) (c c)))) ; ---> #t
(display (Transitive? '((a b) (b a)))) ; ---> #f
(display (Transitive? '((a b) (b a) (a a)))) ; ---> #f
(display (Transitive? '((a b) (b a) (a a) (b b)))) ; ---> #t
(display (Transitive? '())) ; ---> #t

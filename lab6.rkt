#lang racket

; Question 1
(define (Reflexive-Closure L S)
  (cond
    ((null? S) L)
    ((not (member (list (car S) (car S)) L))
     (cons (list (car S) (car S)) (Reflexive-Closure L (cdr S))))
    (else (Reflexive-Closure L (cdr S)))))

; Quesiton 2
(define (Symmetric-Closure L)
  (if (null? L)
      '()
      (let* ((pair (car L))
             (reverse-pair (list (cadr pair) (car pair))))
        (if (member reverse-pair L)
            (cons pair (Symmetric-Closure (cdr L)))
            (cons pair (cons reverse-pair (Symmetric-Closure (cdr L))))))))

; Question 3 - Helper Stuff
(define (pair-in-list? pair L)
  (cond
    ((null? L) #f)
    ((equal? pair (car L)) #t)
    (else (pair-in-list? pair (cdr L)))))

; Helper function to add a pair to a list if it doesn't already exist in the list
(define (add-if-not-exist pair L)
  (if (member pair L) L (cons pair L)))

; Helper function to get all pairs that start with a given element
(define (get-pairs-start-with element L)
  (filter (lambda (pair) (equal? (car pair) element)) L))

; Helper function to get all pairs that end with a given element
(define (get-pairs-end-with element L)
  (filter (lambda (pair) (equal? (cadr pair) element)) L))

; Helper function to find pairs starting with a given element
(define (find-pairs elem L)
  (cond ((null? L) '())
        ((equal? elem (caar L)) (cons (car L) (find-pairs elem (cdr L))))
        (else (find-pairs elem (cdr L)))))

; Mama Mia this-a spaghetti is a too spicy

(define (Transitive-Closure L)
  (define (pair-exists? pair L)
    (cond ((null? L) #f)
          ((equal? pair (car L)) #t)
          (else (pair-exists? pair (cdr L)))))

  (define (find-pairs b L)
    (cond ((null? L) '())
          ((eq? b (caar L)) (cons (car L) (find-pairs b (cdr L))))
          (else (find-pairs b (cdr L)))))

  (define (closure-helper L)
    (cond ((null? L) '())
          (else (let* ((pair (car L))
                       (a (car pair))
                       (b (cadr pair))
                       (pairs (find-pairs b L))
                       (new-pairs (map (lambda (p) (list a (cadr p))) pairs)))
                  (append L
                          (closure-helper (cdr L))
                          (filter (lambda (p) (not (pair-exists? p L))) new-pairs))))))

  (let* ((trans-closure (closure-helper L))
         (transitive-pairs (apply append (map (lambda (p) (find-pairs (cadr p) trans-closure)) trans-closure))))
    (remove-duplicates
     (foldl (lambda (p acc)
              (if (pair-exists? (list (cadr p) (car p)) (append acc trans-closure transitive-pairs))
                  (cons (list (car p) (car p)) acc)
                  acc))
            (append trans-closure transitive-pairs)
            (append trans-closure transitive-pairs)))))


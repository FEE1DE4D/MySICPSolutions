; Exercise 2.1
(define make-rat2 (lambda (n d)
    (let ((g (gcd n d)))
        (cons ((and (< n 0) (< d 0))
                (cons (abs (/ n g)) (abs (/ d g))))
              ((and (< d 0))
                (cons (abs (/ n g) (/ d g))))
              (else
                (cons (/ n g) (abs (/ d g))))))))

; Exercise 2.2
(define make-point (lambda (x y)
    (cons x y)))

(define x-point (lambda (p)
    (car p)))

(define y-point (lambda (p)
    (cdr p)))

(define make-segment (lambda (ss es)
    (cons ss es)))

(define start-segment (lambda (s)
    (car s)))

(define end-segment (lambda (s)
    (cdr s)))

(define midpoint-segment (lambda (s)
    (let ((ss (start-segment s)) (es (end-segment s)))
        (make-point (average (x-point ss) (x-point es))
                    (average (y-point ss) (y-point es))))))

(define average (lambda (x y)
        (/ (+ x y) 2)))

(define print-point (lambda (p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")")))

; Exercise 2.3
; Previous code + this
(define make-rectangle (lambda (a b)
    (cons a b)))

(define a-side (lambda (r)
    (car r)))

(define b-side (lambda (r)
    (cdr r)))

(define width-rectangle (lambda (r)
    (let ((s (a-side r)))
        (- (x-point (end-segment s))
           (x-point (start-segment s))))))

(define height-rectangle (lambda (r)
    (let ((s (b-side r)))
        (- (y-point (end-segment s))
           (y-point (start-segment s))))))

(define perimeter (lambda (r)
    (* 2 (+ (width-rectangle r) (height-rectangle r)))))

Different representation can be achieved by using only lower-left
and upper-right point.

; Exercise 2.4
(define (cdr z)
    (z (lambda (p q) q)))

It is easy, cons returns a function with parameter m that applies m on x and y,
car and cdr then call this function m (return value of cons) with the lambda
function (lambda p q) p) or (lambda p q) q) as parameter and this function
applied on x and y returns first or second element of pair.

; Exercise 2.6
(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define + (lambda (a b)
    (lambda (f) (lambda (x) ((a f) ((b f) x))))))

; Exercise 2.7

(define lower-bound (lambda (i)
  (car i)))

(define upper-bound (lambda (i)
  (cdr i)))

; Exercise 2.8
(define sub-interval (lambda (x y)
    (add-interval x
        (make-interval (- (upper-bound y)) (- (lower-bound y))))))

; Exercise 2.17
(define last-pair (lambda (l)
    (if (= (length l) 1) l
        (last-pair (cdr l)))))

; Exercise 2.18
(define reverse (lambda (l)
    (define reverse-iter (lambda (rl pos)
        (if (= pos 0) rl
            (reverse-iter (append rl (list (list-ref l (- pos 1))))
                          (- pos 1)))))
    (reverse-iter (list) (length l))))

; Better version
(define reverse (lambda (l)
    (if (null? l) (list)
        (append (reverse (cdr l)) (list (car l))))))

;Exercise 2.19
(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+
                (cc amount (except-first-denomination coin-values))
                (cc (- amount (first-denomination coin-values))
                                                            coin-values)))))

(define no-more? (lambda (l)
    (null? l)))


(define first-denomination (lambda (l)
    (car l)))

(define except-first-denomination (lambda (l)
    (cdr l)))

; Exercise 2.20

(define same-parity (lambda (int . l)
    (define iter (lambda (l2 rl evenodd)
        (cond ((null? l2) rl)
              ((evenodd (car l2))
                  (iter (cdr l2) (cons (car l2) rl) evenodd))
          (else (iter (cdr l2) rl evenodd)))))

    (if (even? int) (reverse (iter l (list int) even?))
        (reverse (iter l (list int) odd?)))))

; Exercise 2.21
(define square-list (lambda (items)
    (if (null? items)
        ()
        (cons (square (car items)) (square-list (cdr items))))))

(define square-list (lambda (items)
    (map (lambda (x) (* x x)) items)))

; Exercise 2.22
Because cons prepends element to list, Louise Reasoner is building the list in
reverse order.

Reversing cons order dosnt work, because cons creates a new list only if first
argument is element and second list, in the differnt order you simply create
a pair of list and element.

; Exercise 2.23
(define for-each (lambda (fce items)
    (if (null? items) #t
        (begin (fce (car items))
        (for-each fce (cdr items))))))

; Exercise 2.24
(list 1 (list 2 (list 3 4)))
   (1 (2 (3 4)))
       /   \
      1  (2 (3 4))
           /  \
          2  (3 4)
              / \
             3   4

; Exercise 2.25
(define l (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l)))))
(define l (list (list 7)))
(car (car l))
(define l (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))

; Exercise 2.26
Append creates list of all numbers, cons a pair of (123) (456) and list a
list of two lists (123) (456)

; Exercise 2.28
(define deep-reverse (lambda (l)
    (if (null? l) ()
        (let ((element (car l)))
            (if (pair? element)
                (append (reverse (cdr l)) (reverse element))
            (append (reverse (cdr l)) (list element)))))))

; Exercise 2.29
(define fringe (lambda (l)
    (if (null? l) ()
        (let ((element (car l)))
            (if (pair? element)
                (append (fringe element) (fringe (cdr l)))
            (append (list element) (fringe (cdr l))))))))

; Exercise 2.30
;Pure
(define square-tree (lambda (l)
    (cond ((null? l) ())
          ((not (pair? l)) (square l))
    (else (cons (square-tree (car l)) (square-tree (cdr l)))))))

;Map
(define square-tree (lambda (l)
    (map (lambda (x)
            (if (not (pair? x)) (square x)
                (square-tree x))) l)))

; Exercise 2.31
(define tree-map (lambda (fce tree)
    (map (lambda (x)
        (if (not (pair? x)) (fce x)
            (tree-map fce x))) tree)))

; Exercise 2.33
(define accumulate (lambda (op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))))

(define map (lambda (p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) () sequence)))

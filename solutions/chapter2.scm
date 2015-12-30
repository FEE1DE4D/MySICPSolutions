;;2.1
(define (make-rat n d)
  (let ((sign
	 (cond ((and (> n 0) (> d 0)) +)
	       ((and (< n 0) (< d 0)) +)
	       (else -)))
	(g (gcd n d))
	(n (abs n))
	(d (abs d)))
    (cons (sign (/ n g)) (/ d g))))

;;2.2
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (let ((a (start-segment s))
	(b (end-segment s)))
    (make-point (/ (+ (x-point a) (x-point b))
		   2)
		(/ (+ (y-point a) (y-point b))
		   2))))
;;2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;;2.7
(define (upper-bound interval) (max (car interval) (cdr interval)))
(define (lower-bound interval) (min (cdr interval) (car interval)))

;;2.17
(define (last-pair arg-list)
  (cons (list-ref arg-list (- (length arg-list) 1))
	'()))

;;2.18
(define (reverse arg-list)
  (if (null? arg-list) '()
      (append (reverse (cdr arg-list)) (list (car arg-list)))))

;;2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(define no-more? null?)
(define first-denomination car)
(define except-first-denomination cdr)

;;2.20
(define (odd x) (if (= (modulo x 2) 1) #t #f))
(define (even x) (if (= (modulo x 2) 0) #t #f))

(define (same-parity x . y)
  (let ((parity (if (odd x) odd even)))
    (filter parity y)))

;;2.21
(define (square-list items)
  (if (null? items) '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items) (map square items))

;;2.22
;; a) Recursive vs. iterative process works differently, with substitution its clearly visible that this version of sqare-list is also reversing.

;; b) (cons (list 1 2 3) 4) is not (1 2 3 4) but ((1 2 3) . 4), because list is determined by '() beign last value.

;;2.23
(define (for-each proc items)
  (if (null? items) #t
      (begin (proc (car items))
	     (for-each proc (cdr items)))))

;;2.25
(cdaddr (1 3 (5 7) 9))
(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;;2.27
(define (deep-reverse items)
  (cond  ((null? items) '())
	 ((pair? items) (append (deep-reverse (cdr items))
				(list (deep-reverse (car items)))))
	 (else items)))

;;2.28
(define (fringe items)
  (cond ((null? items) '())
	((not (pair? (car items))) (cons (car items)
					 (fringe (cdr items))))
	(else (append (fringe (car items)) (fringe (cdr items))))))

;;2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;2.30
(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (el)
	 (if (pair? el) (square-tree el)
	     (square el)))
       tree))

;;2.31
(define (tree-map proc tree)
  (map (lambda (el)
	 (if (pair? el) (tree-map el)
	     (proc el)))
       tree))

;;2.32
(define (subsets s)
  (if (null? s) (list '())
      (let ((rest (subsets (cdr s)))
	    (this (car s)))
	(append rest (map (lambda (x)
			    (cons this x)) rest)))))

;;2.33
(define (accumulate op start items)
  (if (null? items) start
      (op (car items) (accumulate op start (cdr items)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

;;2.35
(define (count-leaves x)
       (cond ((null? x) 0)
             ((not (pair? x)) 1)
             (else (+ (count-leaves (car x))
                      (count-leaves (cdr x))))))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y))
	      0
	      (map (lambda (x)
		     (if (pair? x) (count-leaves x) 1)) t)))

;;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;;2.39
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))

;;2.40
(define (enumerate-interval a b) (if (> a b) '() (cons a (enumerate-interval (+ a 1) b))))

(define (unique-pairs n)
  (flatmap
   (lambda (x) (map (lambda (y) (list x y))
		    (enumerate-interval 1 (- x 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;;2.41
;;ugly
(define (unique-triples n)
  (flatmap (lambda (z)
	     (map (lambda (q) (append z (list q)))
		  (enumerate-interval 1 (- (cadr z) 1))))
	   (flatmap
	    (lambda (x) (map (lambda (y) (list x y))
			     (enumerate-interval 2 (- x 1))))
	    (enumerate-interval 3 n))))

(define (sum-triples n s)
  (filter (lambda (x) (= (accumulate + 0 x) s))
	  (unique-triples n)))

;;2.46
(define (make-vec x y) (cons x y))
(define (xcor-vec vec) (car vec))
(define (ycor-vec vec) (cdr vec))
(define (add-vec vec1 vec2)
  (make-vec (+ (xcor-vec vec1) (xcor-vec vec2))
	    (+ (ycor-vec vec1) (ycor-vec vec2))))

(define (sub-vec vec1 vec2)
  (make-vec (- (xcor-vec vec1) (xcor-vec vec2))
	    (- (ycor-vec vec1) (ycor-vec vec2))))

(define (scale-vec vec1 scal)
  (make-vec (* (xcor-vec vec1) scal)
	    (* (ycor-vec vec1) scal)))

;;2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin frame) (car frame))
(define (frame-edge1 frame) (cadr frame))
(define (frame-edge2 frame) (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame-origin frame) (car frame))
(define (frame-edge1 frame) (cadr frame))
(define (frame-edge2 frame) (cddr frame))

;;2.54
(define (equal? items1 items2)
  (if (not (= (length items1) (length items2))) #f
      ((lambda (x) (x x items1 items2))
       (lambda (self items1 items2)
	 (cond ((null? items1) #t)
	       ((eq? (car items1) (car items2)) (self self (cdr items1) (cdr items2)))
	       (else #f))))))

(define (equal? list1 list2)
  (if (not (= (length list1) (length list2))) #f
      (letrec ((compare
	      (lambda (seq1 seq2)
		(cond ((null? seq1) #t)
		      ((eq? (car seq1) (car seq2))
		       (compare (cdr seq1) (cdr seq2)))
		      (else #f)))))
	(compare list1 list2))))

;;2.59
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
	(else (cons (car set1) (union-set (cdr set1) set2)))))

;;2.61
(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
	((= x (car set)) set)
	(else (cons (car set) (adjoin-set x (cdr set))))))

;;2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set (cdr set1) set2)))
		 ((< x2 x1)
		  (cons x2 (union-set set1 (cdr set2)))))))))

;;2.66
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

(define (lookup tree key)
  (let ((this-key (car (entry tree)))
	(this-val (cdr (entry tree))))
    (cond ((= key this-key) this-val)
	  ((< key this-key) (lookup (left-branch tree) key))
	  ((> key this-key) (lookup (right-branch tree) key)))))


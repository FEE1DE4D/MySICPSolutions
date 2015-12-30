;;1.2
(/ (+ 5
      4
      (- 2
	 (- 3
	    (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;;1.3
(define (square a) (* a a))

(define (sum-square a b)
  (+ (square a)
     (square b)))

(define (square-two-larger-out-of-three a b c)
  (cond ((and (<= a b) (<= a c))
	 (sum-square b c))
	((and (<= a b) (not (<= a c)))
	 (sum-square a b))
	((and (not (<= a b)) (not (<= a c)))
	 (sum-square a (if (> b c) b c)))))

;;1.6
If as special form only evaluate one of the two expressions, new-if as a standart function first evaluate both arguments that should be passed to 'cond'.

;;1.7
(define (sqrt-iter guess1 guess2 x)
       (if (good-enough? guess1 guess2)
           guess2
           (sqrt-iter guess2
		      (improve guess2 x)
                      x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess1 guess2)
  (< (abs (- guess2 guess1)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 (improve 1.0 x) x))

;;1.8
(define (improve guess x)
  (/ (+ (/ guess
	   (* y y))
	(* 2 y))
     3))

;;1.9
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
Recursive process


(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
Iterative process

;;1.11
(define (f n)
  (if (< n 3) n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f n)
  (define (iter a b c loopcount)
    (if (= loopcount 0) c
	(iter b c (+ c
		     (* 2 b)
		     (* 3 a))
	      (- loopcount 1))))
  (iter 0 1 2 (- n 2)))

;;1.12
(define (pascal-triangle row column)
  (if (or (= column 1) (= column row))
      1
      (+ (pascal-triangle (- row 1) (- column 1))
	 (pascal-triangle (- row 1) column))))

;;1.16
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (fast-expt b n)
  (define (iter n product)
    (cond ((= n 1) product)
	  ((even? n) (iter (/ n 2)
			   (* product (square b))))
	  (else (iter (- n 1)
		      (* b product)))))
  (iter n 1))

;;1.17
(define (double x)
  (+ x x))

(define (halve x)
  (if (even? x) (/ x 2)
      (error "Cant halve odd number")))

(define (* a b)
  (cond ((= b 0) 0)
	((even? b) (double (* a (halve b))))
	(else (+ a (* a (- b 1))))))



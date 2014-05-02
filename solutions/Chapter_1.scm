; I am not using syntactic sugar for defining new functions,
; because i like lambdas :)

; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))

; Exercise 1.3
(define sum-square-two-larger-from-three (lambda (a b c)
    (cond ((and (< a b) (< a c)) (+ (square b) (square c)))
          ((and (< b a) (< b c)) (+ (square a) (square c)))
          (else (+ (square a) (square b))))))

; Exercise 1.4
This procedure works as (+ a (abs b))
If b > 0 operator is +, else it is -

; Exercise 1.5
Application order - interpret will try to evaluate (p) in ifinite loop
Normal order - interpret will never have to evaluate (p) since (= 0 0) is true
and 0 will be returned.

; Exercise 1.6
The standart if dont evaluate the else clause if the predicate is true,
but the new-if always evaluate the else clause, so the program ends with
infinite recursion.

; Exercise 1.7
; The old sqrt misscalculated very small and very large numbers
; Implementing the new good-enought? method gives correct results
(define sqrt (lambda (x)
    (define (sqrt-iter guess pguess x)
        (if (good-enough? guess pguess)
            guess
            (sqrt-iter (improve guess x) guess x)))

    (define (improve guess x)
        (average guess (/ x guess)))

    (define (average x y)
        (/ (+ x y) 2))

    (define (good-enough? guess pguess)
        (< (abs (/ (- guess pguess) pguess)) 0.001))

    (sqrt-iter 1.0 x x)))

; Exercise 1.8
(define cbrt (lambda (x)
    (define (sqrt-iter guess pguess)
        (if (good-enough? guess pguess)
            guess
            (sqrt-iter (improve guess) guess)))

    (define (improve guess)
        (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

    (define (good-enough? guess pguess)
        (< (abs (/ (- guess pguess) pguess)) 0.001))

    (sqrt-iter 1.0 x)))

; Exercise 1.9
First process is recursive and second is iterative

1.
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc ( + (dec 3) 5)))
(inc (inc (+ 2 5)))
...

2.
(+ (dec 4) (inc 5))
(+ (dec 3) (inc 6))
...

; Exercise 1.10
1024
65536
65536

(f n) = 2*n
(g n) = 2^n
(h n) = 2^(2^(n-1))

; Exercise 1.11
(define f (lambda (n)
    (if (< n 3)
        n
        (+ (f (- n 1))
           (* 2 (f (- n 2)))
           (* 3 (f (- n 3)))))))

(define f (lambda (n)
    (define (f-iter a b c counter)
        (if (< counter 3)
            a
            (f-iter (+ a (* 2 b) (* 3 c)) a b (- counter 1))))
    (if (< n 3)
        n
        (f-iter 2 1 0 n))))

; Exercise 1.12
(define pascal (lambda (l p)
    (cond ((or (= p 1) (= p l)) 1)
          ((or (< p 1) (> p l)) 0)
          (else (+ (pascal (- l 1) (- p 1))
                   (pascal (- l 1) p))))))

; Exercise 1.13
http://www.billthelizard.com/2009/12/sicp-exercise-113-fibonacci-and-golden.html

; Exercise 1.14
; Drawing tree is very simple and if you are not sure about the count-change
; algorithm its a good way how to understand it
Order of growth in space is O(n) - increase in amount = increase in tree depth
Order of growth in steps is O(n^m) - very nice proof can be found there
http://wqzhang.wordpress.com/2009/06/09/sicp-exercise-1-14/

; Exercise 1.15
5
O(log(n))


; Exercise 1.16
(define expt (lambda (b n)
    (define expt-iter (lambda (b n acc)
        (if (= n 0) acc
            (if (= (remainder n 2) 1) (expt-iter b (- n 1) (* acc b))
                                      (expt-iter (square b) (/ n 2) acc)))))
    (expt-iter b n 1)))

; Exercise 1.17
(define * (lambda (a b)
    (define double (lambda (a)
        (+ a a)))

    (define halve (lambda (a)
        (/ a 2)))

    ;Not working because cond evaluates else clause, stupid mistake..
    ;(define mult-iter (lambda (a b acc)
    ;    (cond ((= b 0) acc))
    ;         ((= (remainder b 2) 0) (mult-iter (double a) (halve b) acc))
    ;         (else (mult-iter a (- b 1) (+ acc a)))))

    (define mult-iter (lambda (a b acc)
        (if (= b 0) acc
            (if ( = (remainder b 2) 0) (mult-iter (double a) (halve b) acc)
                                       (mult-iter a (- b 1) (+ acc a))))))

    (mult-iter a b 0)))

; Exercise 1.18
1.17 solution is already iterative.

;Exercise 1.21
199
1999
7

; Exercise 1.30
(define sum (lambda (term a next b)
    (define iter (lambda (a result)
        (if (> a b) result
            (iter (next a) (+ result (term a))))))

    (iter a 0)))

; Exercise 1.31
; Recursive
(define product (lambda (term a next b)
    (if (> a b) 1
        (* (term a) (product term (next a) next b))))))


; Iterative
(define product (lambda (term a next b)
    (define iter (lambda (a acc)
        (if (> a b) acc
            (iter (next a) (* acc (term a))))))

    (iter a 1)))

; Factorial
(define factorial (lambda (n)
    (product (lambda (x) x)
             1
             (lambda (x) (+ 1 x))
             n)))

; Exercise 1.32
; Recursive
(define accumulate (lambda (combiner null-value term a next b)
    (if (> a b) null-value
        (combiner (term a) (accumulate combiner null-value
                           term (next a) next b)))))

; Iterative
(define accumulate (lambda (combiner null-value term a next b)
    (define iter (lambda (a acc)
        (if (> a b) acc
            (iter (next a) (combiner acc (term a))))))
    (iter a null-value)))

; Sum
(define sum (lambda (term a next b)
    (accumulate + 0 term a next b)))

; Product
(define sum (lambda (term a next b)
    (accumulate * 1 term a next b)))


; Exercise 1.34
(f f) -> (f 2) -> (2 2) -> 2 is not applicable

; Exercise 1.40
(define cubic (lambda (a b c)
    (lambda (x)
        (+ (* (* x x) x)
           (* (* x x) a)
           (* x b)
           c))))

; Exercise 1.41
(define double (lambda (p)
    (lambda (x)
        (p (p x)))))

(((double (double double)) inc) 5) this is quite hard,
basicly try to imagine that (double (double)) means to (quadruple) and
(quadruple quadruple) means to perform some action 16x, thats why
the result is 21

; Exercise 1.42
(define compose (lambda (p q)
    (lambda (x)
        (p (q x)))))

; Exercise 1.43
(define repeated (lambda (p n)
    (if (= n 1) p
        (compose p (repeated p (- n 1))))))

; Exercise 1.46
(define iterative-improve (lambda (good-enough improve)
    (lambda (g)
        (if (good-enough g) g
            ((iterative-improve good-enough) (improve g))))))


;;3.1
(define (make-accumulator balance)
  (define (increment val)
    (begin (set! balance (+ balance val))
	   balance))
  increment)

;;3.2
(define (make-monitored func)
  (let ((counter 0))
    (define (call-and-increment args)
      (begin (set! counter (+ counter 1))
	     (apply func args)))
    (define (get-calls-number) counter)
    (define (dispatch . args)
      (cond ((null? args) (call-and-increment '()))
	    ((eq? (car args) 'how-many-calls?) (get-calls-number))
	    (else (call-and-increment args))))
    dispatch))

;;3.3
(define (make-account balance acc-passwd)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch passwd m)
    (cond ((not (eq? passwd acc-passwd))
	   (lambda (x) "Incorrect password"))
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  dispatch)

;;3.4
(define (make-account balance acc-passwd)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops) "I am calling the cops, punk!")
  (let ((attempts 0)
	(max-attempts 0))
    (define (dispatch passwd m)
      (cond ((not (eq? passwd acc-passwd))
	     (begin (set! attempts (+ attempts 1))
	     (if (= attempts max-attempts) (call-the-cops)
		 (lambda (x) "Incorrect password"))))
	    ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    (else (error "Unknown request -- MAKE-ACCOUNT"
			 m))))
    dispatch))


;;3.7
(define (make-joint account old-pass new-pass)
  (define (dispatch arg-pass m)
    (if (not (eq? arg-pass new-pass)) "Incorrect password"
	(account old-pass m)))
  dispatch)

;;3.8
(define (make-f)
  (let ((y 0))
    (define (dispatch x)
      (let ((ret y))
	(set! y x)
	ret))
    dispatch))

;;3.17
(define (count-pairs items)
  (let ((visited '()))
    (define (proc x)
    (if (or (not (pair? x)) (memq x visited))
	0
	(begin
	  (set! visited (cons x visited))
	  (+ (proc (car x))
	     (proc (cdr x))
	     1))))
    (proc items)))

;;3.18
(define (cycle? items)
  (let ((visited '()))
    (define (proc x)
    (if (and (pair? x) (not (memq x visited)))
	(set! visited (cons x visited))
	(begin
	  (set! visited (cons x visited))
	  (+ (proc (car x))
	     (proc (cdr x))
	     1))))
    (proc items)))

(define (append items1 items2)
  (if (null? items1) items2
      (cons (car items1) (append (cdr items1) items2))))


;;3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

;;3.54
(define (add-streams stream1 stream2)
  (stream-map + stream1 stream2))

(define (mul-streams stream1 stream2)
  (stream-map * stream1 stream2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define factorials
  (cons-stream 1
	       (mul-streams factorials
			    (integers-starting-from 2))))

;;3.55
(define (partial-sums arg-stream)
  (cons-stream 0 (add-streams partial-sums arg-stream)))

(define three (cons-stream 2 (stream-map (lambda (x) (* x 3)) three)))


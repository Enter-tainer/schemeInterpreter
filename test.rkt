(define (f n)
  (if (< n 3)
  1
    (+ (f (- n 1))
       (f (- n 2))
    )
  )
)
(f 3)

(define (fib n) (if (< n 3) 1 (+ (fib (- n 1)) (fib(- n 2))))) (fib 3)

(define (fact n) (if (= 1 n) 1 (* n (fact (- n 1))))) (fact 5)
(define (app f n) (f n)) (app fib 5)
(define (app f n) (f n)) (define (appp app f n) (app f n)) (appp app fib 5)
(cond ((> 3 3) 1) ((< 3 3) 2) (else 3))

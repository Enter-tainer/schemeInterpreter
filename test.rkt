(define (f n)
  (if (< n 3)
  1
    (+ (f (- n 1))
       (f (- n 2))
    )
  )
)
(f 3)

(define (f n) (if (< n 3) 1 (+ (f (- n 1)) (f(- n 2))))) (f 3)

(define (fact n) (if (= 1 n) 1 (* n (fact (- n 1))))) (fact 5)

;; Alexandra Josephine Ruud (alexajru)
;; Larissa van der Velpen (ljvelpen)
;; Daniel Ryan Burch (danierb)

;; Oblig 3


(load "prekode3.scm")

;; Task 1
;; a) + b)


(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define (mem msg proc)
  (cond ((eq? msg 'memoize) (memoize proc))
        (eq? msg 'unmemoize) (unmemoize proc)))
  
(fib 3)
(display "**MEMOIZE**")
(newline)


(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 4)




;; b)

(newline)
(display "**UNMEMOIZE**")
(newline)
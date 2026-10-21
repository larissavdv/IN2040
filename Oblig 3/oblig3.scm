;; Alexandra Josephine Ruud (alexajru)
;; Larissa van der Velpen (ljvelpen)
;; Daniel Ryan Burch (danierb)

;; Oblig 3


(load "prekode3.scm")

;; Task 1
;; a) + b)


(define (memoize f)
  (let ((table (make-table)))
    (lambda args
      (let ((previously-computed-result
             (lookup args table)))
        (or previously-computed-result
            (let ((result (apply f args)))
              (insert! args result table)
              result))))))


(define mem
  (let ((table (make-table)))
    (lambda (msg proc)
      (cond ((eq? msg 'memoize)
             (let ((mem-proc (memoize proc)))
                   (insert! mem-proc proc table)
                   mem-proc))
            ((eq? msg 'unmemoize)
             (if (not (lookup proc table))
                 proc
             (lookup proc table)))))))
      
(display "REGULAR FIB")
(newline)
(fib 3)

(display "**MEMOIZE FIB**")
(newline)

(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 4)



(newline)
(display "**UNMEMOIZE FIB**")
(newline)
(set! fib (mem 'unmemoize fib))
(fib 3)
(fib 3)

(newline)
(newline)
(display "**MEMOIZE TEST-PROC**")
(newline)
(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 42 43 44)
(test-proc 40 41 42)


;;c

;; The reason it does not behave as expected is because we are not changing the
;; binding for fib with set!. That means that when we call memo-fib, the table
;; is created and the first call is stored, but when we actually enter the 
;; recursive calls inside of memo-fib, the recursive calls will still
;; point to the originial fib. Set! is destructive, and thereby changes the
;; binding of fib to the memoized version of fib. In this example however,
;; we have just created a new pointer to the memoized version of fib, but
;; fib itself is not changed, which again is why the recursive calls will
;; call the unmemoized fib and not memo-fib. 



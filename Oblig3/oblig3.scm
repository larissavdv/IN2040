;; Alexandra Josephine Ruud (alexajru)
;; Larissa van der Velpen (ljvelpen)
;; Daniel Ryan Burch (danierb)

;; Oblig 3


(load "prekode3.scm")

;; Task 1
;; a) + b)

;; memoize as defined by the book, except this version returns a procedure that
;; takes an arbitrary number of arguments.

;; memoize returns a memoized version (lambda args..) of the function that it
;; takes as argument. 


(define (memoize f)
  (let ((table (make-table)))
    (lambda args
      (let ((previously-computed-result
             (lookup args table)))
        (or previously-computed-result
            (let ((result (apply f args)))
              (insert! args result table)
              result))))))


;; The definition of mem is *not* a procedure, it is a variable bound to a
;; procedure object.

;; The let is evaluated when we define mem, so mem-table lives in that
;; environment. The lambda that mem points to, points to that environment. 
;; This way, all calls share the same mem-table.
;;
;; mem-table has memoized procedures as keys, and their original (unmemoized)
;; versions as values.

;; Message passing distinguishes between 'memoize and 'unmemoize.

;; 'memoize:
;; If proc is a key in the table already, that means it is already memoized,
;; and we return it as is. Otherwise we memoize it, store the pair
;; (memoized . unmemoized) in the mem-table and return the memoized version.
;; This is done in case someone calls 'memoize on a already memoized procedure.

;; 'unmemoize:
;; If proc is a key in the table, it means it is memoized, and we return the
;; original procedere that is associated with that key. If proc is not a
;; key in mem-table, that means it is not memoized, so we return it unchanged.
;; This is done in case someone calls 'unmemoize on a regular procedure. 


(define mem
  (let ((mem-table (make-table)))
    (lambda (msg proc)
      (cond ((eq? msg 'memoize)
             (let ((memoized (lookup proc mem-table)))
               (if memoized 
                   proc     
                   (let ((mem-proc (memoize proc)))
                     (insert! mem-proc proc mem-table)
                     mem-proc))))
            ((eq? msg 'unmemoize)
             (let ((unmemoized (lookup proc mem-table)))
               (if unmemoized 
                   unmemoized
                   proc)))))))



(display "Task 1")
(newline)
(display "------")
(newline)
(newline)

(display "*MEMOIZE fib**")
(newline)
(display "---------------")
(newline)

(set! fib (mem 'memoize fib))
(set! fib (mem 'memoize fib))

(fib 3)
;;computing fib of 3
;;computing fib of 2
;;computing fib of 1
;;computing fib of 0
;;2

(fib 3)
;;2
(fib 2)
;;1
(fib 4)
;;computing fib of 4
;;3

(newline)
(display "*MEMOIZE fac*")
(newline)
(display "---------------")
(newline)

(set! fac (mem 'memoize fac))
(fac 3)
;;computing fac of 3
;;computing fac of 2
;;computing fac of 1
;;6
(fac 3)
;;6

(newline)
(display "*UNMEMOIZE fib*")
(newline)
(display "---------------")
(newline)

(set! fib (mem 'unmemoize fib))
(fib 3)
;;computing fib of 3
;;computing fib of 2
;;computing fib of 1
;;computing fib of 0
;;computing fib of 1
;;2

(newline)
(display "*UNMEMOIZE fac*")
(newline)
(display "---------------")
(newline)

(set! fac (mem 'unmemoize fac))
(fac 3)
;;computing fac of 3
;;computing fac of 2
;computing fac of 1
;;6

(newline)
(display "*MEMOIZE TEST-PROC*")
(newline)
(display "-------------------")
(newline)

(set! test-proc (mem 'memoize test-proc))
(test-proc)
;;computing test-proc of ()
;;0

(test-proc)
;;0

(test-proc 40 41 42 43 44)
;;computing test-proc of (40 41 42 43 44)
;;computing test-proc of (41 42 43 44)
;;computing test-proc of (42 43 44)
;;computing test-proc of (43 44)
;;computing test-proc of (44)
;;10

(test-proc 40 41 42 43 44)
;;10

(test-proc 42 43 44)
;;5


;;c)

;; The reason it does not behave as expected is because we are not changing the
;; binding for fib with set!. When we call memo-fib, the call and result are
;; actually stored in the table, because the first call is on the memoized
;; version. This is why when we do the same call again it is able to return the
;; value instantly. But when we actually enter the recursive calls inside of
;; memo-fib, the recursive calls will still point to the originial fib. Set! is
;; destructive, and thereby changes the binding of fib to the memoized version
;; of fib. In this example however, we have just created a new pointer to the
;; memoized version of fib, but fib itself is not changed, which again is why
;; the recursive calls will call the unmemoized fib and not memo-fib. 



;; Task 2
;; a)

;; list-to-stream

(define (list-to-stream lst)
  (cond ((null? lst) the-empty-stream)
        (else (cons-stream (car lst) (list-to-stream (cdr lst))))))


;; stream-to-list

;; If the user calls stream-to-list with arguments, then args will be a list
;; containing the argument (for example (5)). But if it is called without an
;; argument then n will be the empty list '().

;; If args is null, it means no arguments were passed into the procedure, and
;; we start building our list directly from there. It will then never hit the
;; next if-check (zero? (car args...)

;; If args was not null, we can access the argument it holds with (car args).
;; We can then use that as a counter that decreases for each recursive call.
;; Once args is zero or we run out of elements in our stream, we return the
;; empty list. 


(define (stream-to-list strm . args)
  (cond ((stream-null? strm) '())
        ((null? args) (cons (stream-car strm) (stream-to-list (stream-cdr strm))))
        ((zero? (car args)) '())
        (else (cons (stream-car strm) (stream-to-list (stream-cdr strm) (- (car args) 1))))))

     
(newline)
(display "Task 2")
(newline)
(display "------")
(newline)
(newline)

(display "(list-to-steam '(1 2 3 4 5))")
(newline)
(display "----------------------------")
(newline)

(list-to-stream '(1 2 3))
;;(1 . #<promise>)

(newline)
(display "*stream-to-list without arguments*")
(newline)
(display "----------------------------------")
(newline)

(stream-to-list (stream-interval 10 20))
;;(10 11 12 13 14 15 16 17 18 19 20)

(newline)
(display "(show-stream nats 15)")
(newline)
(display "---------------------")
(newline)

(show-stream nats 15)
;;1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...

(newline)
(display "*stream-to-list with arguments*")
(newline)
(display "-------------------------------")
(newline)

(stream-to-list nats 10)
;;(1 2 3 4 5 6 7 8 9 10)



;;b)

;; We continue building the stream until we either run out of elements in the
;; stream, or until n is decreased to 0. 

(define (stream-take n stream-in)
  (if (or (= n 0) (stream-null? stream-in))
      the-empty-stream
      (cons-stream (stream-car stream-in) (stream-take (- n 1) (stream-cdr stream-in)))))

(newline)
(display "(stream-take 10 nats)")
(newline)
(display "---------------------")
(newline)

(define foo (stream-take 10 nats))
foo
;;(1 . #<promise>)

(newline)
(display "* show stream x2 *")
(newline)
(display "------------------")
(newline)

(show-stream foo 5)
;;1 2 3 4 5 ...
(show-stream foo 20)
;;1 2 3 4 5 6 7 8 9 10

(newline)
(display "(show-stream (stream-take 15 nat) 10)")
(newline)
(display "-------------------------------------")
(newline)

(show-stream (stream-take 15 nats) 10)
;;1 2 3 4 5 6 7 8 9 10 ...


;;c)

;;The issue with Petter's suggestion is that, while this may work on streams that
;;have a finite number in them, it would not work on an infinite stream. memq is
;;used until the stream is empty (to determine if the final item is also a duplicate),
;;which never happens for an infinite stream.


;;d)


(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (remove-duplicates
                    (stream-filter (lambda(x)(not(eq? x (stream-car stream))))
                     (stream-cdr stream))))))

                
(newline)
(display "*TESTING d)*")
(newline)
(display "---------------")
(newline)

(define mystream (list-to-stream '(1 2 2 3 2 1)))
(show-stream (remove-duplicates mystream))


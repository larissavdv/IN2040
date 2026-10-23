;; Alexandra Josephine Ruud (alexajru)
;; Larissa van der Velpen (ljvelpen)
;; Daniel Ryan Burch (danierb)

;; Oblig 3


(load "prekode3.scm")

;; Task 1
;; a) + b)

(define (fib n)
  (display "computing fib of ")
  (display n) (newline)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))


(define (memoize function)
  (let ((table (make-table)))
    (lambda args
      (let ((previously-computed-result
             (lookup args table)))
        (or previously-computed-result
            (let ((result (apply function args)))
              (insert! args result table)
              result))))))


(define mem
  (let ((table (make-table)))
    (lambda (msg proc)
      (cond ((eq? msg 'memoize)
             (let ((memo-proc (memoize proc)))
               (insert! memo-proc proc table)
               memo-proc))
            ((eq? msg 'unmemoize)
             (lookup proc table))))))
                        

(define (sum . numbers)
  (display "computing sum of ")
  (display numbers) (newline)
  (if (null? numbers)
      0
      (+ (car numbers)
         (apply sum (cdr numbers)))))




(newline)
(display "**FIB**")
(newline)

(newline)
(display "**MEMOIZE**")
(newline)
(set! fib (mem 'memoize fib))



(newline)
(display "**UNMEMOIZE**")
(newline)
(fib 3)

(newline)
(display "**FAC**")
(newline)


(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 4)




;; b)

(newline)
(display "**UNMEMOIZE**")
(newline)


;; c)

(display "**C**")

(set! test-proc (mem 'memoize test-proc))
(newline)

(newline)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)
(test-proc 41 42 43 44)
(test-proc 42 43 44)



(display "SUM")
(newline)
(set! sum (mem 'memoize sum))
(sum 1 2 3 4 5)
(sum 1 2 3 4 5)
(sum 3 2 1 4 5)
(sum 1 2 3 4 5)
(sum 1 2 3 4)
(sum 2 3 4)



;;Oppgave 2

(newline)
(newline)
(display "Oppgave 2")
(newline)
(newline)
(display "a:")
(newline)

;;a

(newline)
(display "list-to-stream")
(newline)

(define (list-to-stream lst)
  (cond ((null? lst) the-empty-stream)
        (else (cons-stream (car lst) (list-to-stream (cdr lst))))))


(list-to-stream '(1 2 3 4 5))
(list-to-stream '(5 4 1 4 5 2))

(show-stream (list-to-stream '(5 4 1 4 5 2)))

(show-stream (list-to-stream '(4 5 6 7)))

(show-stream (list-to-stream '(4 3 6 7)))


(newline)
(display "stream-to-list")
(newline)

(define (stream-to-list strm . args)
  (cond ((stream-null? strm) '())
        ((null? args) (cons (stream-car strm) (stream-to-list (stream-cdr strm))))
        ((zero? (car args)) '())
        (else (cons (stream-car strm) (stream-to-list (stream-cdr strm) (- (car args) 1))))))
                  

(stream-to-list (stream-interval 10 20))
(show-stream nats 15)
(stream-to-list nats 10)

;;b

(newline)
(display "b:")
(newline)

(define (stream-take n strm)
  (cond ((stream-null? strm) the-empty-stream)
        ((zero? n) the-empty-stream)
        (else (cons-stream (stream-car strm) (stream-take (- n 1) (stream-cdr strm))))))



(newline)
(display "stream-take")
(newline)

(define foo (stream-take 10 nats))
foo
(show-stream foo 5)
(show-stream foo 20)
(show-stream (stream-take 15 nats) 10)

;;c

(newline)
(display "c: written as a comment")
(newline)

;;The issue with Petter's suggestion is that, while this may work on streams that
;;have a finite number in them, it would not work on an infinite stream. memq is
;;used until the stream is empty (to determine if the final item is also a duplicate),
;;which never happens for an infinite stream.


;;d

(newline)
(display "d:")
(newline)

(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream XXXXX (remove-duplicates (stream-filter pred

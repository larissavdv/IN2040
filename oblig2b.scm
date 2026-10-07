
;;Oppgave 1
;;a)

(define (make-counter)
  (let((count 0))
    (lambda()
      (set! count (+ count 1))
      count)))

;;Test

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))

"Oppgave 1 a"

(c1)     ;; 1
(c1)     ;; 2
(c1)     ;; 3
count    ;; 42
(c2)     ;; 1


;;b)
;;Tegninger

;;Oppgave 2

(define (make-stack elements)
  (let ((stack elements))
    (define (push! lst)                ;;helper method to add every item to the stack in args 
      (cond ((not(null? lst))
             (set! stack (cons (car lst) stack))
             (push! (cdr lst)))))

    (define (dispatch msg . args)
      (cond ((eq? msg 'push!)
             (push! args))
            ((eq? msg 'pop!)
             (if (not (null? stack))
                 (set! stack (cdr stack))))
            ((eq? msg 'stack) stack)))
    dispatch))

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))

"Daniels kode"

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack) ;; (bar)
(s2 'pop!) ;; popper en tom stack
(s2 'push! 1 2 3 4)
(s2 'stack) ;;(4 3 2 1)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack) ;; (baz zip zap bah bar)




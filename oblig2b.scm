
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

;; NB NB NB!!  We did not finish this one yet
;; in the gruppetime on tuesday 6th october they gave us code for this. We can look at that on wednesday 

(define (make-stack init) 
  (let ((stack (if (null? init) (cons '() '()) init)))
    (define (pop!)
      (set! stack (cdr stack)))  ;;NOTE TO SELF: Se mer på hvorfor man ikke kan bruke set-car! her 
    (define (push! . args)
      (define (recurse list)
        (if (null? list)
            '()
            (cons(car list)
                 (recurse (cdr list)))))
      (recurse args))
    (lambda (msg)
      (cond ((eq? msg 'pop!) (pop!)) 
            ((eq? msg 'push!) (push! args))
            ((eq? msg 'stack) stack)))))

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))

"Oppgave 2 a"
(s1 'pop!)
(s1 'stack)

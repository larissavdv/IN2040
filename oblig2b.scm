
;;Task 1
;;a)

(define (make-counter)
  (let((count 0))
    (lambda()
      (set! count (+ count 1))
      count)))

"Calls for task 1a"  ;;We want this to print to the REPL

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))

(c1)     ;; 1
(c1)     ;; 2
(c1)     ;; 3
count    ;; 42
(c2)     ;; 1


;;b)
;;Drawing 

;;Task 2
;;a)

(define (make-stack elements)
  (let ((stack elements))
    (define (push! lst)               
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

"Calls for task 2a"
(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))

(s1 'pop!)
(s1 'stack) 
(s2 'pop!) 
(s2 'push! 1 2 3 4)
(s2 'stack) 
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack) 

;;b)

(define (stack my-stack)
  (my-stack 'stack))

(define (pop! my-stack)
  (my-stack 'pop!))

(define (push! my-stack . args)
  (apply my-stack 'push! args))


"Calls for task 2b"
(pop! s1)
(stack s1) 
(push! s1 'foo 'faa)
(stack s1) 

;;Oppgave 3
;;a)
;;          ADD EXPLAINATION

;;b)        ADD EXPLAINATION 

;c)

(define (cycle? my-list)
  (define (recurse rest seen)
    (cond ((null? rest) #f)
          ((memq rest seen) #t)
          (else (recurse (cdr rest) (cons rest seen)))))
  (recurse my-list '()))

          
"Calls for task 3c"

(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
(set-car! (car bah) 42)

(cycle? '(hey ho)) 
(cycle? '(la la la)) 
(cycle? bah) 
(cycle? bar) 


;;d)

;;The definition of lists in scheme is structures that end in an empty list.
;;With cylics lists however, we no longer have something ending in a empty list.
;;Running list? will iterate the list until it either hits the empty list '()
;;or something else.
;;If it hits the empty list it will return true (at the end of the list)
;;but as soon it hits something else than a cons pair, it will terminate
;;it will therefore never run in a infinite loop. 




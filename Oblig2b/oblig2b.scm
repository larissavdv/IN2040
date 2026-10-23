;; Alexandra Josephine Ruud (alexajru)
;; Larissa van der Velpen (ljvelpen)
;; Daniel Ryan Burch (danierb)

; Oblig 2b

;; Task 1
;; a)

(define (make-counter)
  (let((count 0)) 
    (lambda()
      (set! count (+ count 1))
      count)))

;; We want this to print to the REPL for better readability
;; of the results. This is why we haven't put it in ";;"
" ----------------- "
" Calls for task 1a "
" ----------------- "

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))

(c1)     ;; 1
(c1)     ;; 2
(c1)     ;; 3
count    ;; 42
(c2)     ;; 1


;; b)
;; See pdf for drawings

;; Task 2
;; a)

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
    dispatch)) ;;dispatch is what is ultimately returned

;; We defined push! as an inner (private) procedure, so that
;; it would be easier to call within the cond block.
;; Within push! we use (set! stack (cons (car lst) stack))
;; to build our stack as we iterate the list of arguments.

" ----------------- "
" Calls for task 2a "
" ----------------- "

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))

(s1 'pop!)
(s1 'stack) ;; (bar)
(s2 'pop!) 
(s2 'push! 1 2 3 4)
(s2 'stack) ;; (4 3 2 1)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack) ;; (bah zip zap bah bar)

;; b)

(define (stack my-stack)
  (my-stack 'stack))

(define (pop! my-stack)
  (my-stack 'pop!))

(define (push! my-stack . args)
  (apply my-stack 'push! args))

;; Here we have wrapped the calls inside their own interface
;; We needed to use apply because args will get "passed on"
;; as a list, and therefore we need to "unpack" each element
;; (argument) again from that list 

" ----------------- "
" Calls for task 2b "
" ----------------- "
(pop! s1)
(stack s1) ;; (zip zap bah bar)
(push! s1 'foo 'faa)
(stack s1) ;;(faa foo zip zap bah bar)

;; Task 3

;; a)
;; See PDF for drawings

;; * Explanation *
;; "list-ref" gets the item at a given "index" in a list,
;; in other words the item after a certain number of cdr.
;; After changing bar with set-cdr!, calling list-ref with
;; a value greater than 3 (indexing starts from 0) will loop
;; back around to the pair with 'b in the car position, 
;; and will continue to loop over 'b 'c and 'd indefinately.


;; b)
;; See PDF for drawings

;; * Assumption *
;; We have made the assumption that we did not need to
;; provide a box-pointer-diagram for the last call
;; on set-car!

;; * Explanation *
;; After the first set-car! call on bah, the car of bah points
;; to the cdr of bah. (cdr bah) gives a list, which is why
;; we get the nested list structure. The cdr of bah points
;; to the same list, but will view it as a "continuous" list.

;; The second call (set-car! (car bah) 42) will take the car
;; of bah, which now points to the list ('a 'towel). When we
;; take the car of that again it will give us the element 'a,
;; which will then be replaced with 42.

;; In other words will the structure of bah remain the same
;; after the second call, which is why we still have the
;; nested list structure. What happens is that the symbol 'a
;; gets replaced with 42. 

;;c)

(define (cycle? my-list)
  (define (recurse rest seen)
    (cond ((null? rest) #f)
          ((memq rest seen) #t)
          (else (recurse (cdr rest) (cons rest seen)))))
  (recurse my-list '()))

;; Here we have made a helper method "recurse" that will
;; check wether each pair we get to is a pair we have been
;; to before. memq checks whether our current pair
;; already exists in the "seen" list that we build as we go.

   ;; Initially we tried to run memq with (car rest),but
   ;; that only compares the element at the car position, 
   ;; and not the pair itself. Ultimately rest will always
   ;; work as a pointer to a pair (even though cdr holds
   ;; a pointer to another pair) 

;; If rest becomes empty, that means we got through the
;; whole list without ever getting back to a pair we
;; have already seen. This means it is not a cycle and we
;; can therefore return #f (not a cycle).

;; However, if we do hit a pair that is in "seen", then the
;; procedure returns #t immidiately so that we don't end up
;; in an infinite loop. The first "hit" is enough to say it
;; is a cyclic list, and in this version we do not check if
;; there are multiple cycles. 

" ----------------- "     
" Calls for task 3c "
" ----------------- "

(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
(set-car! (car bah) 42)

(cycle? '(hey ho)) 
(cycle? '(la la la)) 
(cycle? bah) 
(cycle? bar) 


;; d)

;; A list in Scheme is a chain of pairs that ends in the
;; empty list '(). Cyclic lists however, will never end in
;; '(), and therefore "break" with the definition of lists.

;; For bar, set-cdr! made the cdr point back to an earlier
;; pair, so it forms a cycle and never reaches '().
;; bar is therefore not a proper list, and list? returns #f.

;; For bah however, the chain is intact. We can follow the
;; cdr of bah and still reach '(). This makes bah a "proper"
;; list, which is why (list? bah) returns #t.





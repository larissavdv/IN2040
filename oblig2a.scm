;;Alexandra Josephine Ruud (alexajru), Larissa van der Velpen (ljvelpen), Daniel Ryan Burch (danierb)

;;Oppgave 1

;;(a)

(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y)
    x)))

(define (p-cdr proc)
  (proc (lambda (x y)
    y)))
        
(p-car (p-cons "foo" "bar"))

(p-cdr (p-cons "foo" "bar"))

(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar"))))

;;(b)

(define foo 42)

((lambda (foo x)
  (if (= x foo)
      'same
      'different))
  5 foo)

;;first lambda: var1 = bar, var2 = baz | second lambda: var1 = bar, var2 = foo
;;first lambda: exp1 = foo, exp2 = 'towel | second lambda: exp1 = (list bar baz), exp2 = baz


((lambda (bar baz)
   ((lambda (bar foo)
     (list foo bar))
   (list bar baz) baz))
 foo 'towel)

;;(c)

(define (infix-eval exp)
  (let ((x (car exp))
        (y (cadr exp))
        (z (caddr exp)))
    (y x z)))

(define foo (list 21 + 21))
(define baz (list 21 list 21))
(define bar (list 84 / 2))

(infix-eval foo)
(infix-eval baz)
(infix-eval bar)

;;(d)
;(define bah ’(84 / 2))
;;(infix-eval bah)
;;The reason that this gives an error is because, with the quotation, the forward slash
;;no longer reads as an operand, but is rather interpreted as a (STRING)???. Within the
;;parentheses expects y to be a procedure and instead it gets a (STRING).

;;Oppgave 2

;;(a)
(load "huffman.scm")

(define (decode bits tree)
  (define (decode-tail bits current-branch result)
    (if (null? bits)
        (reverse result)
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-tail (cdr bits) tree (cons (symbol-leaf next-branch) result))
              (decode-tail (cdr bits) next-branch result)))))
  (decode-tail bits tree '()))
             
(decode sample-code sample-tree)

;;(b)
;;(samurais fight ninjas by night)

;;(c)

(define (encode symbols tree)
  
  (define (encode-symbol symbol tree)
    (if (






    
  (define (encode-help symbols tree path results)
    (if (null? symbols)
        result
        (let ((left (left-branch tree))
              (right (right-branch tree)))
          (cond ((leaf? left) (encode-help (cdr symbols) tree '() (append (append 0 path) result))
                (leaf? right) (encode-help (cdr symbols) tree '() (append (append 1 path) result))
                
                
                
                   
                      
    



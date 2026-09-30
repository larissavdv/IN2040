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
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x  (car set)) #t)
        (else (element-of-set? x (cdr set)))))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (encode-symbol word tree)
  (cond ((leaf? tree) '())                                        ;;First condition
        ((element-of-set? word (symbols (left-branch tree)))      ;;Second condition 
         (cons 0 (encode-symbol word (left-branch tree))))
        (else (element-of-set? word (symbols (right-branch tree))) ;;Third condition 
         (cons 1 (encode-symbol word (right-branch tree))))))


;;(d)
(define (grow-huffman-tree freqlist)
  (define sorted-leafs (make-leaf-set freqlist))

    (define (huffman-helper set)
    (if (null? (cdr set))
        (car set)
        (huffman-helper (adjoin-set (make-code-tree (car set) (cadr set)) (cddr set)))))
    (huffman-helper sorted-leafs))
           
(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook)

;;(e)
                
(define alfabet
  '((samurais 57) (ninjas 20) (fight 45) (night 12)
    (hide 3) (in 2) (ambush 2) (defeat 1)
    (the 5) (sword 4) (by 12) (assassin 1)
    (river 2) (forest 1) (wait 1) (poison 1)))

(define tree (grow-huffman-tree alfabet))

tree

(define message '(ninjas fight ninjas
                         fight ninjas ninjas
                         fight samurais samurais
                         fight samurais fight
                         ninjas ninjas fight by night))

(display (encode message tree))
(newline)
(length (encode message tree))


;;(f)

(define (huffman-leaves tree)
  (define (huffman-helper tree leaves)
    (if (leaf? tree)
        (cons (list (symbol-leaf tree) (weight-leaf tree)) leaves)
        (huffman-helper (left-branch tree)
                        (huffman-helper (right-branch tree) leaves))))
  (huffman-helper tree '()))

(huffman-leaves sample-tree)
           
        
                
                   
                      
    



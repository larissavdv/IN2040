;;Group members: 




;; Import ;;
(load "huffman.scm")

;; Unit Test
(define (test result expected)
  (cond ((not (equal? expected result))
         (begin
           (display "Result: ")
           (display result)
           (display " -- Expected: ")
           (display expected)
           (newline)))))


;; 2.c

;; Helper to check if a branch contains a symbol
(define (branch-contains? branch symbol)
  (define (br-con-helper symbol-list)
    (cond
      ((null? symbol-list) #f)
      ((eq? (car symbol-list) symbol) #t)
      (else (br-con-helper (cdr symbol-list)))))
  (if (leaf? branch)
      (eq? (cadr branch) symbol)
      (br-con-helper (caddr branch))))

;; Main encode
(define (encode symbols tree)
  (define (encode-helper symbols branch bits)
    (cond
      ((null? symbols) (reverse bits))
      ((leaf? branch) (encode-helper (cdr symbols) tree bits))
      ((branch-contains? (left-branch branch) (car symbols))
       (encode-helper symbols (left-branch branch) (cons 0 bits)))
      (else (encode-helper symbols (right-branch branch) (cons 1 bits)))))
  (encode-helper symbols tree '() ))

;; Test
(test
 (encode '(ninjas fight ninjas) sample-tree)
 '(0 1 0 0 0 1) )
(test
 (decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)
 '(ninjas fight ninjas) )


;; 2.d
(define (grow-huffman-tree freqs-pairs)
  ;; assuming I don't need to guard against empty list
  (let ((nodes (make-leaf-set freqs-pairs)))
    (define (grow-helper)
      (if (= (length nodes) 1)
          (car nodes)
          (begin
            (set! nodes
                  (adjoin-set
                   (make-code-tree (car nodes) (cadr nodes))
                   (cddr nodes)))
            (grow-helper))))
    (grow-helper)))

;; Test
(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(test (decode (encode '(a b c) codebook) codebook) '(a b c))
(test (grow-huffman-tree '((x 10)) ) '(leaf x 10)) ; case of just a leaf

;; 2.e
#|
TODO
|#


;; Print Debug
(define anime-tree
  (grow-huffman-tree 
   '( (samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3) (in 2)
                    (ambush 2) (defeat 1) (the 5) (sword 4) (by 12)
                    (assassin 1) (river 2) (forest 1) (wait 1) (poison 1) )))

(define message '( ninjas ambush samurais in the poison assassin forest ) )
(display (encode message anime-tree))
(newline)

(display (decode (encode message anime-tree) anime-tree) )
(newline)

;; Test
(test (decode (encode message anime-tree) anime-tree) message)


;; 2.f
;; TODO
(define (huffman-leaves tree) 0)

;; Test
;(test
; (huffman-leaves sample-tree)
; '((fight 6) (ninjas 5) (samurais 4) (night 2) (by 1)))

;; End Confirmation
(newline)
(display "all good")


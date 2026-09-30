;;Group members:


(load "huffman.scm")


;; Task 2 c


;;Used element-of-set instead of memq

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



(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)

;;2d


(define (grow-huffman-tree freqs)
  (define sorted-leafs (make-leaf-set freqs))  ;;Makes a sorted set of leafs (lowest freq/weight first) from the frequencies (list of pairs)

  (define (huffman-helper set)
    (if (null? (cdr set))
        (car set)
        (let ((new-subtree (make-code-tree (car set) (cadr set))))    
          (huffman-helper (adjoin-set new-subtree (cddr set))))))

  (huffman-helper sorted-leafs))


(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook)



      

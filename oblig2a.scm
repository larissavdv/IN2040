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
  (cond ((leaf? tree) '())
        ((element-of-set? word (symbols (left-branch tree)))
         (cons 0 (encode-symbol word (left-branch tree))))
        (else (element-of-set? word (symbols (right-branch tree)))
         (cons 1 (encode-symbol word (right-branch tree))))))



(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)
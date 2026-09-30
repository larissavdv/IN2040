
;; 2.f
(define (huffman-leaves tree)
  (let ((acc '() ))  
    (define (helper tree)
      (cond
        ((leaf? tree) (set!
                       acc (append
                            acc
                            (list (list
                                   (symbol-leaf tree)
                                   (weight-leaf tree))))))
        (else (begin
                (helper (left-branch tree))
                (helper (right-branch tree))))))
    (helper tree)
    acc))

(display (huffman-leaves sample-tree))

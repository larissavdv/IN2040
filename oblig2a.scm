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
  (define sorted-leaves (make-leaf-set freqs))  ;;Makes a sorted set of leaves (lowest freq/weight first) from the frequencies (list of pairs)

  (define (huffman-helper set)
    (if (null? (cdr set))
        (car set)
        (let ((new-subtree (make-code-tree (car set) (cadr set))))    
          (huffman-helper (adjoin-set new-subtree (cddr set))))))

  (huffman-helper sorted-leaves))


(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook)


;2e)

(define alfabet '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3) (in 2)
                                (ambush 2) (defeat 1) (the 5) (sword 4) (by 12) (assassin 1)
                                (river 2) (forest 1) (wait 1) (poison 1)))
      
(define tree (grow-huffman-tree alfabet))

;; We have made the assumption that the message provided is one message,
;; and not 6 different sentences

(define message '(ninjas fight ninjas fight ninjas ninjas fight samurais
                         samurais fight samurais fight ninjas ninjas fight by night))

(define bits (encode message tree))

;; Hvor mange bits bruker det på å kode meldingen?
(length bits) ;; This gives 43, so that means it takes 60 bits to kode the message. 

;; Hva er den gjennomsnittlige lengden på hvert kodeord som brukes? (Vi tenker at alle symbolene
;; representeres i én og samme liste slik at linjeskift ignoreres.)
;;43 / 17 = 2,53 


;; Til slutt: hva er det minste antall bits man ville trengt for å kode meldingen med en kode med
;; fast lengde (fixed-length code) over det samme alfabetet? Begrunn kort svaret ditt.

;;That would be log(2)16 = 4 bits * 17 = 68 bits.
;; This means we have saved 25 bits by huffman-coding


;2f)

(define (make-freq leaf) ;;helper procedure to make a list of symbol + weight
  (list (symbol-leaf leaf)(weight-leaf leaf)))

(define (huffman-leaves tree)
  (if (leaf? tree)
      (cons (make-freq tree) '())                     ;;If our current branch is a leaf 
      (append (huffman-leaves (left-branch tree))
              (huffman-leaves (right-branch tree)))))

        
(huffman-leaves sample-tree)



; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2017                              *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "dictionary.ss")
;; -----------------------------------------------------
;; HELPER FUNCTIONS
(define flatten
    (lambda (l)
    (cond
      ((null? l) '())
       ((list? (car l)) (append (flatten (car l)) (flatten (cdr l))))
        (else(cons (car l) (flatten(cdr l))))
    )
   )
)
(define hashword ;returns a list of hashes of w
  (lambda (hashlist w)
    (cond
      ((null? hashlist) '())
      (else
       (cons ((car hashlist) w) (hashword (cdr hashlist) w)))
    )
  )
)
(define wordify ;go down to a word to hash it
  (lambda (hashlist dict)
    (cond
      ((null? dict) '())
      (else
       (cons (hashword hashlist (car dict)) (wordify hashlist (cdr dict))) )
    )
   )
 )
(define inBV ;is word hashes in vector
  (lambda (bv wh)
    (cond
     ((null? wh) #t)
     (else
       (and (inBV bv (cdr wh)) (cond
                                 ((= (mem (car wh) bv) 0) #f )
                                 (else #t)
                                )
       )
     )
    )
  )
)
(define mem
  (lambda (e l)
    (cond
      ((null? l) 0)
      (else
       (+
        (cond
          ((= e (car l)) 1)
          (else
           0))
        (mem e (cdr l))
        )
       )
      )
    )
  )
;; -----------------------------------------------------
;; KEY FUNCTION
;(define key
 ; (lambda (w)
  ;  (cond
   ;   ((null? (cdr w)) (cons (+ (* (reduce + '(1729 1729 1729) 0) 1 29) (ctv (car w))) '()))
    ;  (else
     ;  (reduce + (flatten (cons (* (flatten(key (cdr w))) 29) (ctv (car w)))) 0))
      ;)
    ;)
  ;)
(define key
  (lambda (w)
    (cond
      ((null? (cdr w)) (+ (* (reduce + '(1729 1729 1729) 0) 29) (ctv (car w))))
      (else
       (+ (* (key (cdr w)) 29) (ctv (car w))))
      )
    )
  )
;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 106402241991
;;   (key '(m a y))           = 126526810
;;   (key '(t r e e f r o g)) = 2594908189083745

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS
;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (k)
     (reduce modulo (cons (key k) '()) size)
    )
  )
)
;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17
(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (k)
       (floor (* size (- (* (key k) A) (floor (* (key k) A)))))))) 
;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 7224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))       ==> 35616
;;  (hash-1 '(m a y))           ==> 46566
;;  (hash-1 '(t r e e f r o g)) ==> 48238
;;
;;  (hash-2 '(h e l l o))       ==> 48849
;;  (hash-2 '(m a y))           ==> 81025
;;  (hash-2 '(t r e e f r o g)) ==> 16708
;;
;;  (hash-3 '(h e l l o))       ==> 6331.0
;;  (hash-3 '(m a y))           ==> 2456.0
;;  (hash-3 '(t r e e f r o g)) ==> 1806.0
;;
;;  (hash-4 '(h e l l o))       ==> 788.0
;;  (hash-4 '(m a y))           ==> 306.0
;;  (hash-4 '(t r e e f r o g)) ==> 225.0
;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR
(define gen-checker
  (lambda (hashfunctionlist dict)
   (let ((bitvec (wordify hashfunctionlist dict)))
     (lambda (w)
      (inBV (flatten bitvec) (hashword hashfunctionlist w))
     )
   )
  )
)       
;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #t  // false positive

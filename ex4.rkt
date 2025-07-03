#lang racket

(provide (all-defined-out))

(define integers-from
  (lambda (n)
    (cons-lzl n (lambda () (integers-from (+ n 1))))))

(define cons-lzl cons)
(define empty-lzl? empty?)
(define empty-lzl '())
(define head car)
(define tail
  (lambda (lzl)
    ((cdr lzl))))

(define leaf? (lambda (x) (not (list? x))))

;; Signature: map-lzl(f, lz)
;; Type: [[T1 -> T2] * Lzl(T1) -> Lzl(T2)]
(define map-lzl
  (lambda (f lzl)
    (if (empty-lzl? lzl)
        lzl
        (cons-lzl (f (head lzl))
                  (lambda () (map-lzl f (tail lzl)))))))

;; Signature: take(lz-lst,n)
;; Type: [LzL*Number -> List]
;; If n > length(lz-lst) then the result is lz-lst as a List
(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
      empty-lzl
      (cons (head lz-lst)
            (take (tail lz-lst) (- n 1))))))

; Signature: nth(lz-lst,n)
;; Type: [LzL*Number -> T]
;; Pre-condition: n < length(lz-lst)
(define nth
  (lambda (lz-lst n)
    (if (= n 0)
        (head lz-lst)
        (nth (tail lz-lst) (- n 1)))))


;;; Q3.1
; Signature: append$(lst1, lst2, cont) 
; Type: [List * List * [List -> T]] -> T
; Purpose: Returns the concatenation of the given two lists, with cont post-processing
(define append$
  (lambda (lst1 lst2 cont)
    (let loop ((x lst1) (k cont))
      (if (empty? x)
          (k lst2)
          (loop (cdr x)
                (lambda (r)
                  (k (cons (car x) r))))))))


;;; Q3.2
; Signature: equal-trees$(tree1, tree2, succ, fail) 
; Type: [Tree * Tree * [Tree ->T1] * [Pair->T2] -> T1 U T2]
; Purpose: Determines structural identity of two trees with CPS
(define equal-trees$
  (lambda (tree1 tree2 succ fail)
    (cond
      ;; both leaves
      [(and (not (pair? tree1)) (not (pair? tree2)))
       (succ (cons tree1 tree2))]

      ;; one is a pair, the other is not => structure mismatch
      [(or (and (pair? tree1) (not (pair? tree2)))
           (and (not (pair? tree1)) (pair? tree2)))
       (fail (append (flatten-leaf1 tree1) (flatten-leaf1 tree2)))]

      ;; both are lists — traverse recursively
      [(and (pair? tree1) (pair? tree2))
       (let loop ((xs tree1) (ys tree2) (acc '()))
         (cond
           [(and (null? xs) (null? ys)) (succ (reverse acc))]
           [(or (null? xs) (null? ys))
            (fail (append (flatten-leaf1 xs) (flatten-leaf1 ys)))]
           [else
            (equal-trees$
             (car xs) (car ys)
             (lambda (res) (loop (cdr xs) (cdr ys) (cons res acc)))
             fail)]))]

      ;; default mismatch
      [else (fail (append (flatten-leaf1 tree1) (flatten-leaf1 tree2)))])))


;; Helper to flatten all leaves from two subtrees into a single list
(define (flatten-leaves t1 t2)
  (append (flatten-leaf1 t1) (flatten-leaf1 t2)))

(define (flatten-leaf1 t)
  (cond
    [(null? t) '()]
    [(pair? t) (append (flatten-leaf1 (car t)) (flatten-leaf1 (cdr t)))]
    [else (list t)]))


;; Flatten a (possibly nested) list into a flat list of leaves
(define (flatten-leaf-list t)
  (cond
    [(null? t) '()]
    [(pair? t) (append (flatten-leaf-list (car t)) (flatten-leaf-list (cdr t)))]
    [else (list t)]))

;; Drop the first n elements from a list
(define (drop lst n)
  (if (or (zero? n) (null? lst))
      lst
      (drop (cdr lst) (- n 1))))

;;; Q4.1

;; Signature: as-real(x)
;; Type: [ Number -> Lzl(Number) ]
;; Purpose: Convert a rational number to its form as a
;; constant real number
(define as-real
  (lambda (x)
    (cons-lzl x (lambda () (as-real x)))
  )
)


;; Signature: ++(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Addition of real numbers
(define ++
  (lambda (x y)
   (cons-lzl (+ (head x)(head y)) (lambda () (++ (tail x) (tail y))))
  )
)

;; Signature: --(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Subtraction of real numbers
(define --
(lambda (x y)
   (cons-lzl (- (head x)(head y)) (lambda () (-- (tail x) (tail y))))
  )
)

;; Signature: **(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Multiplication of real numbers
(define **
 (lambda (x y)
   (cons-lzl (* (head x)(head y)) (lambda () (** (tail x) (tail y))))
  )
)
;; Signature: //(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Division of real numbers
(define //
 (lambda (x y)
   (cons-lzl (/ (head x)(head y)) (lambda () (// (tail x) (tail y))))
  )
)

;;; Q4.2.a
;; Signature: sqrt-with(x y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Lzl(Number)) ]
;; Purpose: Using an initial approximation `y`, return a 
;; sequence of real numbers which converges into the 
;; square root of `x`
(define sqrt-with
  (lambda (x y)
      (cons-lzl y (lambda ()
            (sqrt-with x
              (// (++ y (// x y)) (as-real 2)))))
  )
)

;;; Q4.2.b
;; Signature: diag(lzl)
;; Type: [ Lzl(Lzl(T)) -> Lzl(T) ]
;; Purpose: Diagonalize an infinite lazy list
(define diag
  (lambda (lzl)
     (define go
      (lambda (mat n)
        (cons-lzl (nth (head mat) n)
                  (lambda () (go (tail mat) (+ n 1))))))
    (go lzl 0)
  )
)

;;; Q4.2.c
;; Signature: rsqrt(x)
;; Type: [ Lzl(Number) -> Lzl(Number) ]
;; Purpose: Take a real number and return its square root
;; Example: (take (rsqrt (as-real 4.0)) 6) => '(4.0 2.5 2.05 2.0006097560975613 2.0000000929222947 2.000000000000002)
(define rsqrt
  (lambda (x)
    (diag (sqrt-with x x)))
)


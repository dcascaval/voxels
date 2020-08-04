#lang rosette/safe
 (require rosette/lib/synthax)
 (require rosette/solver/smt/z3)
 (require (only-in racket hash))

(current-solver (z3 
    #:path "C:/Users/Dan/z3.exe" ; sub out your own path
    #:logic 'QF_BV 
    #:options (hash
    ':parallel.enable 'true
    ':parallel.threads.max 8)
  )
)

; We choose an encoding of a 4x4x4 space represented by a 64-bit vector, where
; a set bit corresponds to a filled cube and an unset bit to empty space. 
; 
; It is nested such that: 
; the last 16 bits correspond to the 4x4 plane at Z of 0, 
; the next 16 bits are the 4x4 plane at Z of 1, etc. 
; Likewise with Y, the last 4 bits are a line of boxes at y = 0, x = (1,2,3,4)
; the next 4 bits are at Y = 1. 
(current-bitwidth 64)

(define (union a b) (bvor a b))
(define (diff a b) (bvand a (bvnot b)))
(define (translateX+ a) (bvshl  a (bv 1 64)))
(define (translateX- a) (bvlshr a (bv 1 64)))
(define (translateY+ a) (bvshl  a (bv 4 64)))
(define (translateY- a) (bvlshr a (bv 4 64)))
(define (translateZ+ a) (bvshl  a (bv 16 64)))
(define (translateZ- a) (bvlshr a (bv 16 64)))

; Mirrors across the X-axis, i.e. every other 4-bit sequence in the vector becomes the 
; reverse of the one immediately following it. 
(define (mirrorX a) 
  (bvor 
    (bvor 
      (bvand a (bv #x3333333333333333 64)) 
      (bvshl (bvand a (bv #x2222222222222222 64)) (bv 1 64))) 
    (bvshl (bvand a (bv #x1111111111111111 64)) (bv 3 64)) ))

; Mirrors across the Y-axis, i.e. every other 16-bit sequence in the vector
(define (mirrorY a) 
  (define b1 (bvshl (bvand a (bv #x00FF00FF00FF00FF 64)) (bv 8 64)))
  (define b2 
    (bvor (bvshl (bvand b1 (bv #x0F0F0F0F0F0F0F0F 64)) (bv 4 64)) (bvlshr (bvand b1 (bv #xF0F0F0F0F0F0F0F0 64)) (bv 4 64))))
  (define b3 
    (bvor (bvshl (bvand b2 (bv #x2222222222222222 64)) (bv 2 64)) (bvlshr (bvand b2 (bv #xCCCCCCCCCCCCCCCC 64)) (bv 2 64))))
  (define b4 
    (bvor (bvshl (bvand b3 (bv #x5555555555555555 64)) (bv 1 64)) (bvlshr (bvand b3 (bv #xAAAAAAAAAAAAAAAA 64)) (bv 1 64))))
  (bvor
    (bvand b4 (bv #xFF00FF00FF00FF00 64))
    (bvand a (bv #x00FF00FF00FF00FF 64))))

; Mirrors across the Z-axis, i.e. the first 32 bits become the reverse of the last 32 bits.
(define (mirrorZ a)
  (define b1 (bvshl (bvand a (bv #x00000000FFFFFFFF 64)) (bv 32 64)))
  (define b2 
    (bvor (bvshl (bvand b1 (bv #x0000FFFF00000000 64)) (bv 16 64)) (bvlshr (bvand b1 (bv #xFFFF000000000000 64)) (bv 16 64))))
  (define b3 
    (bvor (bvshl (bvand b2 (bv #x00FF00FF00000000 64)) (bv 8 64)) (bvlshr (bvand b2 (bv #xFF00FF0000000000 64)) (bv 8 64))))
  (define b4 
    (bvor (bvshl (bvand b3 (bv #x0F0F0F0F00000000 64)) (bv 4 64)) (bvlshr (bvand b3 (bv #xF0F0F0F000000000 64)) (bv 4 64))))
  (define b5 
    (bvor (bvshl (bvand b4 (bv #x2222222200000000 64)) (bv 2 64)) (bvlshr (bvand b4 (bv #xCCCCCCCC00000000 64)) (bv 2 64))))
  (define b6 
    (bvor (bvshl (bvand b5 (bv #x5555555500000000 64)) (bv 1 64)) (bvlshr (bvand b5 (bv #xAAAAAAAA00000000 64)) (bv 1 64))))
  (bvor b6 (bvand a (bv #x00000000FFFFFFFF 64))))

; Actual language encoding
(define-synthax (vx a depth)
  #:base a
  #:else (choose 
    a ((choose union diff) (vx a (- depth 1)) (vx a (- depth 1)))
      ((choose mirrorX mirrorY mirrorZ) (vx a (- depth 1)))
      ((choose translateX+ translateX- translateY+ translateY- translateZ+ translateZ-) 
        (vx a (- depth 1)))
  )
)


;(define (vx-4 a) (vx a 4))
 (define (vx-5 a) (vx a 5))
;(define (vx-6 a) (vx a 6))
;(define (vx-7 a) (vx a 7))
;(define (vx-8 a) (vx a 8))
;(define (vx-9 a) (vx a 9))
'(define (union-skeleton a) 
  (union 
    (union 
      (union (vx a 6) (vx a 6))
      (union (vx a 6) (vx a 6))
    )
    (translateZ+ (translateZ+ (union 
      (union (vx a 6) (vx a 6))
      (union (vx a 6) (vx a 6))
    )))
  )
)

; This 'implementation' of Box-2-2 corresponds to the following CSG:
; Rosette is actually smarter than this, as this is of depth 7, and 
; Rosette manages to do it with 5 (see results). 
; `i` represents input, U is union, x+, y+ etc are translations.
;                 U                     (1)
;        z+                 U           (2)
;        U            y+        U       (3)
;   y+        U       U       x+  i     (4)
;   U       x+  i   x+  i     i         (5)
; x+  i     i       i                   (6)
; i                                     (7)
(define (x2 x) (union (translateX+ x) x))
(define (y2 y) (union (translateY+ y) y))
(define (z2 z) (union (translateZ+ z) z))
(define (xyz2 i) (z2 (y2 (x2 i)))) ; equal to: x0000 0000 0033 0033


(define (box-2-2) (xyz2 (bv 1 64)))
(define (mirror-box) (bv #x9009064002609009 64))
(define (tetrominoes) (bv #x0990600240060990 64))
(define (full-box) (bv #xFFFFFFFFFFFFFFFF 64))
(define (planes-3) (bv #x9009000000009009 64))

; Input is always the unit (0,0) voxel, i.e. (bv 1 64). 
; We guarantee that the result is equal to the specified geometry output.
(define-symbolic y (bitvector 64))
 (print-forms 
  (synthesize 
    #:forall (list y)
    #:guarantee (assert (equal? (box-2-2) (vx-5 (bv 1 64))))
  )
)
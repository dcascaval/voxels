#lang rosette/safe
 (require rosette/lib/synthax)
 (require rosette/solver/smt/z3)
  (require (only-in racket hash in-range for for/list with-handlers flush-output
                    thread thread-wait break-thread exn:break?
                    make-semaphore semaphore-wait semaphore-post call-with-semaphore/enable-break
                    processor-count file-exists? string->path))

(current-solver (z3 
    #:path "C:/Users/Dan/z3.exe"
    #:logic 'QF_BV 
    #:options (hash
    ':parallel.enable 'true
    ':parallel.threads.max 8)
  )
)

(current-bitwidth 64)

(struct plus (left right) #:transparent)
(struct mul (left right) #:transparent)
(struct square (arg) #:transparent)

(require rosette/lib/match)

(define (d) (4))

(define (interpret p)
  (match p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(mul a b)   (* (interpret a) (interpret b))]
    [(square a)  (expt (interpret a) 2)]
    [_ p]))

(define (union a b) (bvor a b))
(define (diff a b) (bvand a (bvnot b)))
(define (translateX+ a) (bvshl  a (bv 1 64)))
(define (translateX- a) (bvlshr a (bv 1 64)))
(define (translateY+ a) (bvshl  a (bv 4 64)))
(define (translateY- a) (bvlshr a (bv 4 64)))
(define (translateZ+ a) (bvshl  a (bv 16 64)))
(define (translateZ- a) (bvlshr a (bv 16 64)))
(define (mirrorX a) 
  (bvor 
    (bvor 
      (bvand a (bv #x3333333333333333 64)) 
      (bvshl (bvand a (bv #x2222222222222222 64)) (bv 1 64))) 
    (bvshl (bvand a (bv #x1111111111111111 64)) (bv 3 64)) ))

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

		;;; mirrorX:
		;;;  ret (x & 0x3333 3333 3333 3333) | ((x & 0x2222 2222 2222 2222) << 1) | ((x & 0x1111 1111 1111 1111) << 3)
		;;; mirrorY:
		;;;  b1 = (x & 0x00FF00FF00FF00FF) << 8;
		;;;  b2 = ((b1 & 0x0F0F0F0F0F0F0F0F) << 4 | (b2 & 0xF0F0F0F0F0F0F0F0 >> 4)); 
		;;;  b3 = ((b2 & 0x2222222222222222) << 2 | (b3 & 0xCCCCCCCCCCCCCCCC >> 2)); 
		;;;  b4 = ((b3 & 0x5555555555555555) << 1 | (b4 & 0xAAAAAAAAAAAAAAAA >> 1)); 
		;;;  ret (b4 & 0xFF00FF00FF00FF00) | (x & 0x00FF00FF00FF00FF)
		;;; mirrorZ:
		;;; 	b1 = (x  & (0x00000000FFFFFFFF) << 32);
		;;; 	b2 = ((b1 & 0x0000FFFF00000000) << 16) | ((b1 & 0xFFFF000000000000) >> 16)
		;;; 	b3 = ((b2 & 0x00FF00FF00000000) << 8 ) | ((b2 & 0xFF00FF0000000000) >> 8 )
		;;; 	b4 = ((b3 & 0x0F0F0F0F00000000) << 4 ) | ((b3 & 0xF0F0F0F000000000) >> 4 )
		;;; 	b5 = ((b4 & 0x2222222200000000) << 2 ) | ((b4 & 0xCCCCCCCC00000000) >> 2 )
		;;; 	b6 = ((b5 & 0x5555555500000000) << 1 ) | ((b5 & 0xAAAAAAAA00000000) >> 1 )
		;;; 	ret b6 | (x & 0x00000000FFFFFFFF);


(define-synthax (vx a depth)
  #:base a
  #:else (choose 
    a ((choose union diff) (vx a (- depth 1)) (vx a (- depth 1)))
      ((choose mirrorX mirrorY mirrorZ) (vx a (- depth 1)))
      ((choose translateX+ translateX- translateY+ translateY- translateZ+ translateZ-) 
        (vx a (- depth 1)))
  )
)

(define (sub-up a) 
  (bvand a (bvnot (bvshl a (bv 1 64))))) 


; Here's a program that would construct the 2x2 box. Rosette is actually smarter than this!
;                 U                     (1)
;        z+                 U           (2)
;        U            y+        U       (3)
;   y+        U       U       x+  x     (4)
;   U       x+  x   x+  x     x         (5)
; x+  x     x       x                   (6)
; x                                     (7)


;(define (vx-5 a) (vx a 4))
 (define (vx-6 a) (vx a 6))
;(define (vx-7 a) (vx a 7))
;(define (vx-8 a) (vx a 8))
;(define (vx-9 a) (vx a 9))
;(define (vx-9 a) 
;  (union 
;    (union 
;      (union (vx a 6) (vx a 6))
;      (union (vx a 6) (vx a 6))
;    )
;    (translateZ+ (translateZ+ (union 
;      (union (vx a 6) (vx a 6))
;      (union (vx a 6) (vx a 6))
;    )))
;  )
;)


(define (x2 x) (union (translateX+ x) x))
(define (y2 y) (union (translateY+ y) y))
(define (z2 z) (union (translateZ+ z) z))
(define (xyz2 i) (z2 (y2 (x2 i))))

(define (box-2-2) (xyz2 (bv 1 64)))
(define (mirror-box) (bv #x9009064002609009 64))
(define (tetrominoes) (bv #x0990600240060990 64))
(define (full-box) (bv #xFFFFFFFFFFFFFFFF 64))
(define (planes-3) (bv #x9009000000009009 64))

(define-symbolic y (bitvector 64))
 (print-forms 
  (synthesize 
    #:forall (list y)
    #:guarantee (assert (equal? (planes-3) (vx-6 (bv 1 64))))
  )
)


;;; DEFINE min_max(prefix,z0,z1,z2,z3) { 
;;;   int $prefix_min = 0 + 1 * (z1 & !z0) + 2 * (z2 & !z0 & !z1) + 3 * (z3 & !z2 & !z1 & !z0);
;;;   int $prefix_max = (3 * z3) + 2 * (z2 & !z3) + 1 * (z1 & !z2 & !z3) + 0;
;;; }

;; Implicitly with a dimension of 4. If we want a variable dimension
;; This will probably need some loops.
;;; bv bbox(bv s) { 

;;;   bv z_mask = 0xFFFF000000000000; 
;;;   bool z0 = (z_mask >> 0  & s) > 0; 
;;;   bool z1 = (z_mask >> 16 & s) > 0;
;;;   bool z2 = (z_mask >> 32 & s) > 0;
;;;   bool z3 = (z_mask >> 48 & s) > 0;
;;;   min_max(z,z0,z1,z2,z3); 

;;;   bv y_mask = 0xF000F000F000F000;
;;;   bool y0 = (y_mask >> 0  & s) > 0;
;;;   bool y1 = (y_mask >> 4  & s) > 0; 
;;;   bool y2 = (y_mask >> 8  & s) > 0; 
;;;   bool y3 = (y_mask >> 12 & s) > 0;
;;;   min_max(y,y0,y1,y2,y3); 

;;;   bv x_mask = 0x8888888888888888;
;;;   bool x0 = (x_mask >> 0 & s) > 0; 
;;;   bool x1 = (x_mask >> 1 & s) > 0; 
;;;   bool x2 = (x_mask >> 2 & s) > 0; 
;;;   bool x3 = (x_mask >> 3 & s) > 0;
;;;   min_max(x,x0,x1,x2,x3);

;;;   ; Now that we have masked everything, we
;;;   ; would like to OR together everything between the min & max for each dimension.
;;;   bv z =  (0 >= zmin & 0 <= zmax & zmask >> 0 )
;;;         & (1 >= zmin & 1 <= zmax & zmask >> 16)
;;;         & (2 >= zmin & 2 <= zmax & zmask >> 32)
;;;         & (3 >= zmin & 3 <= zmax & zmask >> 48); 
;;;   bv y =  (0 >= ymin & 0 <= ymax & ymask >> 0 )
;;;         & (1 >= ymin & 1 <= ymax & ymask >> 4 )
;;;         & (2 >= ymin & 2 <= ymax & ymask >> 8 )
;;;         & (3 >= ymin & 3 <= ymax & ymask >> 12); 
;;;   bv x =  (0 >= xmin & 0 <= xmax & xmask >> 0 )
;;;         & (1 >= xmin & 1 <= xmax & xmask >> 1 )
;;;         & (2 >= xmin & 2 <= xmax & xmask >> 2 )
;;;         & (3 >= xmin & 3 <= xmax & xmask >> 3 ); 

;;;   return s > 0 & (x | y | z); 
;;; }


;;;  ;; This is by far the most complex operation, particularly as the
;;;  ;; semantics are unclear. Unlike the other operations, it doesn't have
;;;  ;; an inverse, and can very easily overflow. But basically we want to 
;;;  ;; replicate each dimensional slice F times, offsetting the later slices
;;;  ;; by as many times as needed to accomodate the earlier ones 
;;;  ;; (so, lot of duplications, OR, translates, etc.)
;;;  bv scale (bv s, int f, int dimension) { 
;;;   ;; We can either use the base point of the bbox, or the base point of the origin. 
      ;; TODO: implement
;;;  }

;;;  bv translate (bv s, int direction, int magnitude) { 
     ;; alternatively: shift = magnitude * (1 << (2 * direction)), 
     ;; and allow negative shifts to work, then just s >> shift;
;;;   match (direction) { 
;;;     ;; X 
;;;     0 => magnitude > 0 ? s >> magnitude : s << magnitude 
;;;     ;; Y
;;;     1 => magnitude > 0 ? s >> (magnitude * 4) : s << (magnitude * 4)
;;;     ;; Z
;;;     2 => magnitude > 0 ? s >> (magnitude * 16) : s << (magnitude * 16) 
;;;   }
;;;  }
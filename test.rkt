#lang rosette
 (require rosette/lib/synthax)

(struct plus (left right) #:transparent)
(struct mul (left right) #:transparent)
(struct square (arg) #:transparent)

(define (interpret p)
  (match p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(mul a b)   (* (interpret a) (interpret b))]
    [(square a)  (expt (interpret a) 2)]
    [_ p]))

;;; (struct union (left right) #:transparent)
;;; (struct diff (left right) #:transparent)
;;; (struct translateX+ (arg) #:transparent)
;;; (struct translateX- (arg) #:transparent)
;;; (struct translateY+ (arg) #:transparent)
;;; (struct translateY- (arg) #:transparent)
;;; (struct translateZ+ (arg) #:transparent)
;;; (struct translateZ- (arg) #:transparent)

(define (union a b) (bvor a b))
(define (diff a b) (bvand a (bvnot b)))
(define (translateX+ a) (bvshl  a (bv 1 64)))
(define (translateX- a) (bvlshr a (bv 1 64)))
(define (translateY+ a) (bvshl  a (bv 4 64)))
(define (translateY- a) (bvlshr a (bv 4 64)))
(define (translateZ+ a) (bvshl  a (bv 16 64)))
(define (translateZ- a) (bvlshr a (bv 16 64)))

;;; (define (interpret-vector p) 
;;;   (match p 
;;;     [(union a b)     (bvor   (interpret-vector a) (interpret-vector b))]
;;;     [(diff  a b)     (bvand  (interpret-vector a) (bvnot (interpret-vector b)))]
;;;     [(translateX+ a) (bvshl  (interpret-vector a) (bv 1 64))] 
;;;     [(translateX- a) (bvlshr (interpret-vector a) (bv 1 64))] 
;;;     [(translateY+ a) (bvshl  (interpret-vector a) (bv 4 64))] 
;;;     [(translateY- a) (bvlshr (interpret-vector a) (bv 4 64))] 
;;;     [(translateZ+ a) (bvshl  (interpret-vector a) (bv 16 64))] 
;;;     [(translateZ- a) (bvlshr (interpret-vector a) (bv 16 64))] 
;;;     [_ p]
;;;   ))

(define-synthax (vx a depth)
  #:base a
  #:else (choose 
    a ((choose union diff) (vx a (- depth 1)) (vx a (- depth 1)))
      ((choose translateX+ translateX- translateY+ translateY- translateZ+ translateZ-) 
        (vx a (- depth 1)))
  )
)

(define (vx-id a) (vx a 2))

(define (sub-up a) 
  (bvand a (bvnot (bvshl a (bv 1 64))))) 

;; Okay, so this works. We can get a sub-up working.
;; Now... we want to synthesize the program such that it's not for all y, it's just
;; for a single given input, we want the single given output.
;;; (define-symbolic y (bitvector 64))

; (print-forms 
;   (synthesize 
;     #:forall (list y)
;     #:guarantee (assert (equal? (sub-up y) (vx-id y)))
;   )
; )


; Here's a program that would do it: 
         

;                 U                     (1)
;        z+                 U           (2)
;        U            y+        U       (3)
;   y+        U       U       x+  x     (4)
;   U       x+  x   x+  x     x         (5)
; x+  x     x       x                   (6)
; x                                     (7)

;(define (vx-5 a) (vx a 5))
;(define (vx-6 a) (vx a 6))
;(define (vx-7 a) (vx a 7))
 (define (vx-8 a) (vx a 8))
;(define (vx-9 a) (vx a 9))

(define (x2 x) (union (translateX+ x) x))
(define (y2 y) (union (translateY+ y) y))
(define (z2 z) (union (translateZ+ z) z))
(define (xyz2 i) (z2 (y2 (x2 i))))

(define (box-2-2) (xyz2 (bv 1 64)))
(define (mirror-box) (bv #x9009064002609009 64))
(define (tetrominoes) (bv #x0990600240060990 64))
(define (full-box) (bv #xFFFFFFFFFFFFFFFF 64))

(define-symbolic y (bitvector 64))
(print-forms 
  (synthesize 
    #:forall (list y)
    #:guarantee (assert (equal? (full-box) (vx-8 (bv 1 64))))
  )
)


;;; (define-symbolic y integer?)
;;; (interpret (square (plus y 2))) 

;;; (define-symbolic x c integer?)
;;; (assert (even? x))
;;; (asserts)   ; assertion pushed on the store
;;; ; ((= 0 (remainder x 2)))

;;; (define sol
;;;     (synthesize #:forall x
;;;                 #:guarantee (assert (odd? (+ x c)))))
;;; (asserts)   ; assertion store same as before
;;; ; '((= 0 (remainder x 2)))

;;; (evaluate x sol) ; x is unbound
;;; ; x

;;; (evaluate c sol) ; c must an odd integer

; ---------------------------------------------------------------------------------

; Define the div2 function as a sketch, with a choice of these operations.forall
; The correct choice is left shift, and the hole represents how much we shift by
(define (div2 x)
  ; Hole grammar: ?? maybe-type, returns fresh symbolic variable of type maybe-type
  ; or integer? if one is not provided.
  ([choose bvshl bvashr bvlshr bvadd bvsub bvmul] x (?? (bitvector 8))))

; Create a new symbolic variable for the solver
(define-symbolic i (bitvector 8))

;;; (print-forms ; What does this do?
;;;      (synthesize 
;;;         #:forall (list i)
;;;      ; Assert that this is equal to dividing by a bv value 2 of length 8
;;;         #:guarantee (assert (equal? (div2 i) (bvudiv i (bv 2 8))))))

; In the end, the result is that shifting right by 1 is always equal to div2, so:
; '(define (div2 x) (bvlshr x (bv 1 8)))

;; Trying to get: x \ (x+ x- (x)) or any of the equivalents
;;; (define (zro x)
;;;   ([choose union diff] x x
;;;     ;;; ([choose translateX+ translateX- translateY+ translateY- translateZ+ translateZ-]
;;;     ;;;   ([choose translateX+ translateX- translateY+ translateY- translateZ+ translateZ-]
;;;     ;;;     x 
;;;     ;;;   )
;;;     ;;; )
;;;   )
;;; )



;;; (interpret-vector (diff y (translateX+ (translateX- y))))
;;; (interpret-vector (diff y y))
;;; (interpret-vector (union y y))

;;; (define-symbolic q (bitvector 64))
;;; (interpret-vector (zro q))

;;; (synthesize 
;;;   #:forall (list q)
;;;   #:guarantee (assert (equal? (interpret-vector (zro q)) q))
;;; )

;;; (define-symbolic r (bitvector 64))
;;; (synthesize 
;;;   #:forall (list r)
;;;   #:guarantee (assert (equal? (interpret-vector (zro r)) (bv #x0000000000000000 64)))
;;; )




;;; (assert (equal? (bv 0 64) (zro q)))
;;; (asserts)

;;; (print-forms 
;;;   (synthesize
;;;     #:forall (list q)
;;;     #:guarantee (assert (equal? (zro q) (bv 0 64)))
;;;   )
;;; )



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
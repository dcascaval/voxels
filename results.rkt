; box-2-2
'(define (vx-5 a)
   (union
    (union
     (translateX+ (translateY+ (translateZ+ a)))
     (translateY+ (union (translateZ+ a) a)))
    (union
     (translateX+ (union a (translateZ+ a)))
     (union (translateY+ (translateX+ a)) (union (translateZ+ a) a)))))


; tetrominoes (complex!)
'(define (vx-9 a)
   (union
    (translateY-
     (union
      (union
       (translateY+ (translateZ+ (translateZ+ (translateY+ (translateZ+ a)))))
       (union
        (translateY+ (translateZ+ (translateX+ (translateX+ a))))
        (translateY-
         (union (translateY- (translateZ+ a)) (translateX- (translateZ+ a))))))
      (union
       (union
        (translateZ+ (translateZ+ (translateX+ (translateY+ a))))
        (union
         (union (translateY- (translateZ+ a)) (translateX- (translateZ+ a)))
         (translateY+ (translateX+ (translateZ+ a)))))
       (translateZ+
        (translateX+ (translateX+ (translateZ+ (translateZ+ a))))))))
    (translateZ+
     (union
      (union
       (union
        (translateY+ (translateY+ (translateZ+ (translateZ+ a))))
        (union
         (translateX- (translateX- (translateZ+ a)))
         (diff (translateZ+ (translateX+ a)) (translateY- (translateZ+ a)))))
       (union
        (translateX+ (translateZ+ (translateY- (translateZ+ a))))
        (diff
         (translateY+ (translateZ+ (translateZ+ a)))
         (translateX- (translateX- (translateZ+ a))))))
      (union
       (translateZ+ (translateY- (translateZ+ (translateX- (translateZ+ a)))))
       (translateX-
        (translateZ+ (translateZ+ (translateY+ (translateY+ a))))))))))


; mirror-box (could maybe do with 8)
 (union
    (translateZ+
     (union
      (union
       (union
        (translateZ+ (translateZ+ (translateY- (translateZ+ a))))
        (translateY+ (translateZ+ (translateX- (translateZ+ a)))))
       (union
        (union
         (translateY+ (translateX+ (translateX+ a)))
         (translateY+ (translateY+ (translateX+ a))))
        (union
         (translateY+ (translateX+ (translateX+ a)))
         (union (translateZ+ (translateZ+ a)) (translateY+ (translateX+ a))))))
      (union
       (translateZ+
        (translateX+
         (union (translateX+ (translateY+ a)) (translateY+ (translateY+ a)))))
       (translateZ+
        (translateY- (translateX- (translateX- (translateZ+ a))))))))
    (diff
     (union
      (union
       (translateZ+ (translateZ+ (translateX- (translateZ+ (translateZ+ a)))))
       (union
        (union
         (translateX- (union (translateZ+ a) (translateY+ a)))
         (union (translateY- (translateZ+ a)) (diff (translateY+ a) a)))
        (translateY+ (translateY+ (translateX- (translateZ+ a))))))
      a)
     (diff
      (union
       (translateY+
        (union
         (translateZ+ (translateX- (translateY+ a)))
         (union (translateX+ (translateX+ a)) (union a (translateY+ a)))))
       (union
        (translateZ- (translateZ+ (translateY+ (translateY+ a))))
        (translateZ+ (translateY+ (translateX+ (translateX+ a))))))
      (translateY-
       (translateZ+
        (translateY+ (translateY+ (translateX- (translateZ+ a)))))))))
        
; 3-planes - depth 6 (uses mirrors!)
'(define (vx-6 a)
   (mirrorX
    (diff
     (mirrorZ (mirrorX (mirrorY (mirrorX a))))
     (union
      (translateY+ (union (translateZ+ a) a))
      (union
       (translateZ+ (translateY+ a))
       (union (translateZ+ a) (translateX+ a)))))))

; 3-planes - depth 4 (simplest solution)
(mirrorZ (mirrorX (mirrorY (mirrorX a))))

; 3-planes with chunk missing, it found the diff-solution :)
(diff
    (mirrorZ (mirrorY (mirrorX a)))
    (translateZ- (mirrorX (translateZ+ a)))))